library(tidyverse)

options(scipen=999)
options(dplyr.summarise.inform=F)

SOURCES_TEST <- c()
TEST_YEAR <- 2020
PRINT_PROGRESS <- T

dt <- readRDS("1_clean_mocks.rds")
training_probs <- readRDS("2_source_training_probs.rds")

test <- dt %>% 
  filter(DraftYear == TEST_YEAR)

test_joined <- test %>% 
  select(CollegePlayerID,PlayerName,DraftNumber,SourceName,ProjPick) %>% 
  inner_join(training_probs, by = c("SourceName", "ProjPick")) %>% 
  distinct()

if(length(SOURCES_TEST)>0){
  test_joined <-test_joined %>% 
    filter(SourceName %in% SOURCES_TEST)
}

players <- test_joined %>% 
  pull(CollegePlayerID) %>% 
  unique()

## - 1 
get_player_bayes_prob <- function(current_player,test_joined){
  player_df <- test_joined %>% 
    filter(CollegePlayerID == current_player)
  
  sources <- player_df %>% 
    pull(SourceName) %>% 
    unique()
  
  max_source_num <- length(sources)-1
  
  if(max_source_num == 0){
    source_num <- 1
    posterior <- player_df %>% 
      filter(SourceName == sources[source_num]) %>% 
      select(x,posterior_prob=y)
    if(PRINT_PROGRESS){
      print(paste(unique(player_df$PlayerName),sources[source_num]))
    }
  } else{
    for(source_num in 1:max_source_num){
      if(source_num == 1){
        prior <- player_df %>% 
          filter(SourceName == sources[source_num]) %>% 
          select(x,y)
        if(PRINT_PROGRESS){
          print(paste(unique(player_df$PlayerName),sources[source_num]))
        }
      } else {
        prior <- posterior %>% 
          rename(y = posterior_prob)
      }
      likelihood <- player_df %>% 
        filter(SourceName == sources[source_num+1]) %>% 
        select(x,y)
      if(PRINT_PROGRESS){
        print(paste(unique(player_df$PlayerName),sources[source_num+1]))
      }
      posterior <- update_prior(prior,likelihood)
    }
  }
  player_df %>% 
    select(CollegePlayerID,PlayerName,DraftNumber) %>% 
    distinct() %>% 
    crossing(posterior)
}


## - 2 
update_prior <- function(prior,likelihood,keep_old_values = F){
  if(nrow(prior)==0){
    return(likelihood %>% 
             select(x,posterior_prob=y))
  }else if(nrow(likelihood)==0) {
    return(prior %>% 
             select(x,posterior_prob=y))
  } else{
    bayes <- prior %>% 
      full_join(likelihood %>% 
                  rename(new_y=y),
                by = c("x")) %>% 
      mutate(y = replace_na(y,0),
             new_y = replace_na(new_y,0)) %>% 
      mutate(mult = y * new_y) %>% 
      mutate(total_mult = sum(mult)) %>% 
      mutate(posterior_prob = mult / total_mult)
    if(keep_old_values){
      return(bayes)
    }else{
      return(bayes %>% 
               select(x,posterior_prob))
    }
  }
}


all_player_probs <- players %>% 
  map_df(~get_player_bayes_prob(.x,test_joined)) %>% 
  filter(posterior_prob != 0) %>% 
  mutate(x = ifelse(x > 255, 256, x)) %>%
  group_by(CollegePlayerID,x) %>%
  mutate(posterior_prob = sum(posterior_prob)) %>%
  ungroup() %>%
  distinct()

all_player_probs_mod <- all_player_probs %>% 
  arrange(CollegePlayerID) %>% 
  group_by(CollegePlayerID) %>% 
  mutate(taken_by = cumsum(posterior_prob)) %>% 
  mutate(avail = 1 - taken_by + posterior_prob) %>% 
  ungroup() %>% 
  select(CollegePlayerID,PlayerName,pick=x,pick_prob=posterior_prob,available_prob=avail,actual_pick=DraftNumber)

saveRDS(all_player_probs_mod,"3_pre-draft_probs.rds")
