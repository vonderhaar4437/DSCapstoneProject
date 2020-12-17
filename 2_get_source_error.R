library(tidyverse)
library(KernSmooth)

options(scipen=999)
options(dplyr.summarise.inform=F)

TEST_YEAR <- 2020
PRINT_PROGRESS <- T

dt <- readRDS("1_clean_mocks.rds")


spl_start <- c(1,6,15,33,65,100,150,200)
spl_end <- c((spl_start - 1)[-1],500)
splits <- tibble(start = spl_start, end = spl_end)

## - 1
get_source_probs <- function(current_source,train){
  if(PRINT_PROGRESS){
    print(current_source)
  }
  train_source <- train %>% 
    filter(SourceName == current_source)
  
  map2_df(splits$start, splits$end, ~ get_split_data(.x,.y,train_source)) %>% 
    add_column(SourceName = current_source) %>% 
    rename(ProjPick = proj)
}

## - 2
get_split_data <- function(start_pick,end_pick,train_source){
  if(PRINT_PROGRESS){
    print(paste(start_pick,end_pick))
  }
  train_source_error <- train_source %>%
    filter(ProjPick >= start_pick,
           ProjPick <= end_pick) %>%
    pull(error)
  if(length(train_source_error)>1){
    train_source_kern <- bkde(train_source_error, kernel = "normal") %>% as_tibble()
    probs <- convert_to_int_kern(train_source_kern,start_pick,end_pick)
    probs
  }else{
    tibble()
  }
}

## - 3
convert_to_int_kern <- function(kern, start_pick,end_pick){
  temp <- crossing(proj = start_pick:end_pick, kern) %>% 
    mutate(new_x = proj + x) %>% 
    mutate(new_x_int = round(new_x,0)) %>% 
    mutate(abs_diff = abs(new_x_int-new_x)) %>% 
    group_by(proj,new_x_int) %>% 
    mutate(rank_val = rank(abs_diff,ties.method="min")) %>% 
    filter(rank_val <= 2) %>% 
    summarise(new_y = mean(y)) %>% 
    filter(new_x_int > 0) %>% 
    ungroup()
  temp2 <- crossing(proj = unique(temp$proj),
           new_x_int = 1:max(temp$new_x_int)) %>% 
    left_join(temp, by = c("proj", "new_x_int")) %>% 
    group_by(proj) %>% 
    mutate(next_y = lead(new_y),
           prev_y = lag(new_y)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(new_y = ifelse(is.na(new_y),mean(c(next_y,prev_y),na.rm=T),new_y)) %>% 
    ungroup() %>% 
    select(-c(next_y,prev_y)) %>% 
    mutate(new_y = replace_na(new_y,0)) %>% 
    group_by(proj) %>% 
    mutate(total_prob = sum(new_y)) %>% 
    ungroup() %>% 
    mutate(new_y = new_y / total_prob) %>% 
    rename(x = new_x_int, y = new_y) %>% 
    select(-total_prob)
  temp2
}



train <- dt %>% 
  filter(DraftYear != TEST_YEAR)

sources <- train %>% 
  pull(SourceName) %>% 
  unique()

training_probs <- sources %>% 
  map_df(~get_source_probs(.x,train))

saveRDS(training_probs,"2_source_training_probs.rds")
