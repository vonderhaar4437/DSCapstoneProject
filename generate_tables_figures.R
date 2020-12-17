library(tidyverse)
library(gt)
library(modelr)

clean_mocks <- readRDS("1_clean_mocks.rds")
training_probs <- readRDS("2_source_training_probs.rds")

## table 1
clean_mocks %>% 
  filter(DraftYear == 2020) %>% 
  filter(DraftNumber == 5) %>% 
  select(DraftYear, SourceName, PlayerName, ProjPick, Actual_Pick = DraftNumber, error) %>% 
  head(10) %>% 
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    DraftYear = md("**Draft Year**"),
    SourceName = md("**Source Name**"),
    PlayerName = md("**Player Name**"),
    ProjPick = md("**Projected Pick**"),
    Actual_Pick = md("**Actual Pick**"),
    error = md("**Error**"),
  ) %>% 
  gtsave("temp.html")
webshot::webshot("temp.html", file = "figures/clean_mock.png", vwidth = 2000, vheight = 2000, selector = "div > table", expand = 5, zoom = 3)
file.remove("temp.html")


## figure 2
clean_mocks %>% 
  filter(DraftNumber < 256, 
         ProjPick < 256) %>% 
  mutate(`Draft Round` = paste0("Round ",DraftRound)) %>% 
  ggplot(aes(error,fill=`Draft Round`)) +
  geom_density(color = "black", lwd = 1, alpha = .5) +
  theme_bw() +
  labs(x = "Projection Error",
       y = "Density") 

## figure 3
clean_mocks %>% 
  filter(DraftRound == 1) %>% 
  filter(DraftNumber %in% c(3,8,16,24)) %>% 
  ggplot(aes(error,fill=as.factor(DraftNumber))) +
  geom_density(color = "black", lwd = 1, alpha = .5) +
  theme_bw() +
  labs(x = "Projection Error",
       y = "Density",
       fill = "Draft Selection") 

## figure 4
training_probs %>% 
  filter(ProjPick == 10) %>% 
  filter(SourceName %in% c("DraftWire", "DraftTek", "NFL.com","PFF")) %>% 
  ggplot(aes(x,y,color=SourceName)) +
  geom_line(lwd = 2) +
  xlim(0,100) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  labs(x = "Draft Selection",
       y = "Probability") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "dark gray", lwd = 1.5)



## figure 5
justin2 <- readRDS("jefferson_two_sources.rds")

clean_mocks %>% 
  filter(PlayerName == "Justin Jefferson") %>%
  filter(SourceName %in% c("DraftTek","DraftWire")) %>% 
  select(DraftYear, SourceName, PlayerName, ProjPick, Actual_Pick = DraftNumber, error) %>% 
  inner_join(training_probs, by = c("SourceName", "ProjPick")) %>% 
  select(SourceName,x,y,ProjPick) %>% 
  mutate(SourceName = recode(SourceName, "DraftWire"="DraftWire (Prior)",
                             "DraftTek" = "DraftTek (Evidence)")) %>% 
  bind_rows(justin2 %>% 
              select(x = pick, y = pick_prob) %>% 
              mutate(SourceName = "Combined (Posterior)")) %>% 
  ggplot(aes(x,y,color = SourceName)) +
  geom_line(lwd = 2)+
  xlim(0,100) +
  geom_line(aes(ProjPick,color = SourceName), lwd = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 22, lwd = 1.5, color = "red") + 
  annotate("text", x = 75, y = .013, label = "Red = Actual Pick", color = "red", fontface = 2) +
  annotate("text", x = 75, y = .014, label = "Dashed Lines = Source Projection",  fontface = 2) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  labs(x = "Draft Selection",
       y = "Probability") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(reverse = TRUE))



## figure 6
predraft_probs <- readRDS("3_pre-draft_probs.rds")

predraft_probs %>% 
  filter(!is.na(actual_pick)) %>% 
  filter(PlayerName %in% c("Joe Burrow","Tua Tagovailoa","Justin Herbert")) %>% 
  group_by(CollegePlayerID,PlayerName,actual_pick) %>% 
  mutate(pick_prob = pick_prob / sum(pick_prob)) %>% 
  sample_n(10000, replace = T, weight = pick_prob) %>% 
  ungroup() %>% 
  select(-c(pick_prob,available_prob)) %>% 
  ggplot() +
  geom_density(aes(pick,fill=PlayerName), bw = .5, alpha = .5, color = "black") +
  geom_vline(aes(xintercept = actual_pick, color = PlayerName), lwd = 2) +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = "Draft Selection",
       y = "Probability",
       color = "Player",
       fill = "Player") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


## figure 7
predraft_probs %>% 
  filter(!is.na(actual_pick)) %>% 
  filter(actual_pick %in% c(5,10,20,40)) %>% 
  mutate(PlayerName = paste(str_pad(actual_pick, side = "left", pad = "0", width = 2),PlayerName,sep="-")) %>% 
  group_by(CollegePlayerID,PlayerName,actual_pick) %>% 
  mutate(pick_prob = pick_prob / sum(pick_prob)) %>% 
  sample_n(10000, replace = T, weight = pick_prob) %>% 
  ungroup() %>% 
  select(-c(pick_prob,available_prob)) %>% 
  ggplot() +
  geom_density(aes(pick,fill=PlayerName), bw = .7, alpha = .5, color = "black") +
  geom_vline(aes(xintercept = actual_pick, color = PlayerName), lwd = 2) +
  scale_x_continuous(breaks = c(1,10,20,30,40,50,60)) +
  xlim(c(0,60)) +
  labs(x = "Draft Selection",
       y = "Probability",
       color = "Player",
       fill = "Player") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()


## table 2
predraft_probs %>% 
  filter(actual_pick == 7) %>% 
  head(10) %>% 
  mutate(available_true = pick <= actual_pick) %>% 
  select(-CollegePlayerID) %>% 
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    PlayerName = md("**Name**"),
    pick = md("**Pick**"),
    pick_prob = md("**Pick Probability**"),
    available_prob = md("**Available Probability**"),
    actual_pick = md("**Actual Pick**"),
    available_true = md("**Actual Available**"),
  ) %>%
  fmt_percent(columns = vars(pick_prob,available_prob), use_seps = TRUE, decimals =  1) %>% 
  gtsave("temp.html")
webshot::webshot("temp.html", file = "figures/predraft_probs.png", vwidth = 2000, vheight = 2000, selector = "div > table", expand = 5, zoom = 3)
file.remove("temp.html")




## figure 11
simple_picks <- clean_mocks %>% 
  filter(DraftYear == 2020) %>% 
  select(CollegePlayerID, PlayerName, SourceName, ProjPick, DraftNumber) %>% 
  filter(DraftNumber < 257) %>% 
  bind_rows(
    clean_mocks %>% 
      filter(DraftYear == 2020) %>% 
      select(CollegePlayerID, PlayerName, SourceName, ProjPick, DraftNumber) %>% 
      filter(DraftNumber < 257) %>% 
      select(CollegePlayerID) %>% 
      distinct() %>% 
      inner_join(
        predraft_probs, by = "CollegePlayerID"
      ) %>% 
      group_by(CollegePlayerID,PlayerName,actual_pick) %>% 
      filter(pick_prob == max(pick_prob)) %>% 
      ungroup() %>% 
      mutate(SourceName = "Combined") %>% 
      select(CollegePlayerID, PlayerName, SourceName, ProjPick = pick, DraftNumber = actual_pick)
  ) %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(DraftNumber <= pick_high,
         DraftNumber >= pick_low) %>% 
  select(-c(pick_high,pick_low))

simple_picks %>% 
  filter(SourceName == "NFL Network (Daniel Jeremiah)") %>% 
  filter(DraftNumber <= 32) %>% 
  ggplot(aes(ProjPick,DraftNumber)) +
  geom_point(size = 5, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", lwd = 2) +
  scale_x_continuous(breaks = seq(0,32,by=8)) +
  scale_y_continuous(breaks = seq(0,32,by=8),limits = c(0,32)) +
  theme_bw() +
  labs(x = "Projected Pick",
       y = "Actual Pick")



## table 3
get_rsq_round <- function(source_picks){
  r <- source_picks %>% 
    filter(ProjPick <= 255, DraftNumber <= 255) %>% 
    lm(DraftNumber ~ ProjPick, data = .) %>% 
    summary() %>% 
    .$r.squared
  
  tibble(SourceName = unique(source_picks$SourceName),
         draft_round = unique(source_picks$draft_round),
         rsq = r)
  
}

get_rsq<- function(source_picks){
  r <- source_picks %>% 
    filter(ProjPick <= 255, DraftNumber <= 255) %>% 
    lm(DraftNumber ~ ProjPick, data = .) %>% 
    summary() %>% 
    .$r.squared
  
  tibble(SourceName = unique(source_picks$SourceName),
         rsq = r)
  
}

source_round_projs <- clean_mocks %>% 
  filter(DraftYear == 2020) %>% 
  select(CollegePlayerID, PlayerName, SourceName, ProjPick, DraftNumber) %>% 
  filter(DraftNumber < 257) %>% 
  inner_join(
    training_probs
  ) %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(ProjPick <= pick_high,
         ProjPick >= pick_low) %>% 
  select(-c(pick_high,pick_low)) %>% 
  select(CollegePlayerID,SourceName,draft_round) %>% 
  distinct() %>% 
  count(SourceName,draft_round) %>% 
  
  mutate(p = n / 32) %>% 
  filter(p >= .7) %>% 
  group_by(SourceName) %>% 
  do(tail(., n=1)) %>% 
  arrange(draft_round) %>% 
  select(SourceName,draft_round) %>% 
  distinct()

source_round_projs %>% 
  ungroup() %>% 
  add_row(SourceName = "Combined", draft_round = "Rd7") %>% 
  add_row(SourceName = "Median Projection", draft_round = "Rd7") %>% 
  inner_join(
    tibble(pick_low = c(1,33,65,107,147,180,215),
           pick_high = c(32,64,106,146,179,214,256),
           draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))
  ) %>% 
  select(SourceName,max_pick = pick_high) %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(pick_high <= max_pick) %>% 
  inner_join(
    simple_picks
  ) %>% 
  filter(DraftNumber >= pick_low, DraftNumber <= pick_high) %>% 
  mutate(draft_round = if_else(draft_round %in% c("Rd5","Rd6","Rd7"),"Rd5+",draft_round)) %>% 
  group_by(SourceName,draft_round) %>% 
  do(get_rsq_round(.)) %>% 
  arrange(-rsq) %>% 
  ungroup() %>% 
  spread(draft_round,rsq) %>% 
  arrange(-Rd1) %>% 
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    SourceName = md("**Source Name**"),
    Rd1 = md("**Rd1**"),
    Rd2 = md("**Rd2**"),
    Rd3 = md("**Rd3**"),
    Rd4 = md("**Rd4**"),
    `Rd5+` = md("**Rd5+**"),
  ) %>% 
  fmt_number(
    columns = 2:6,
    decimals = 2
  ) %>% 
  fmt_missing(columns = 2:6, missing_text = "") %>% 
  tab_style(
    style = list(
      cell_fill(color = "yellow")
    ),
    locations = cells_body(
      rows = 3
    )) %>% 
  gtsave("temp.html")
webshot::webshot("temp.html", file = "figures/rsq.png", vwidth = 2000, vheight = 2000, selector = "div > table", expand = 5, zoom = 3)
file.remove("temp.html")

## table 4
source_round_projs %>% 
  ungroup() %>% 
  add_row(SourceName = "Combined", draft_round = "Rd7") %>% 
  filter(!draft_round %in% c("Rd1","Rd2")) %>% 
  inner_join(
    tibble(pick_low = c(1,33,65,107,147,180,215),
           pick_high = c(32,64,106,146,179,214,256),
           draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))
  ) %>% 
  select(SourceName,max_pick = pick_high) %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(pick_high <= max_pick) %>% 
  inner_join(
    simple_picks
  ) %>% 
  filter(DraftNumber >= pick_low, DraftNumber <= pick_high) %>% 
  filter(draft_round %in% c("Rd1","Rd2","Rd3")) %>% 
  group_by(SourceName) %>% 
  do(get_rsq(.)) %>% 
  arrange(-rsq) %>% 
  ungroup() %>% 
  rename(first3 = rsq) %>% 
  left_join(
    source_round_projs %>% 
      ungroup() %>% 
      add_row(SourceName = "Combined", draft_round = "Rd7") %>% 
      filter(!draft_round %in% c("Rd1","Rd2","Rd3")) %>% 
      inner_join(
        tibble(pick_low = c(1,33,65,107,147,180,215),
               pick_high = c(32,64,106,146,179,214,256),
               draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))
      ) %>% 
      select(SourceName,max_pick = pick_high) %>% 
      crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                      pick_high = c(32,64,106,146,179,214,256),
                      draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
      filter(pick_high <= max_pick) %>% 
      inner_join(
        simple_picks
      ) %>% 
      filter(DraftNumber >= pick_low, DraftNumber <= pick_high) %>% 
      filter(draft_round %in% c("Rd1","Rd2","Rd3","Rd4")) %>% 
      group_by(SourceName) %>% 
      do(get_rsq(.)) %>% 
      arrange(-rsq) %>% 
      ungroup() %>% 
      rename(first4 = rsq)
  ) %>%  
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    SourceName = md("**Source Name**"),
    first3 = md("**Rds 1-3**"),
    first4 = md("**Rds 1-4**"),
  ) %>% 
  fmt_number(
    columns = 2:3,
    decimals = 2
  ) %>% 
  fmt_missing(columns = 2:3, missing_text = "") %>% 
  tab_style(
    style = list(
      cell_fill(color = "yellow")
    ),
    locations = cells_body(
      rows = 1
    )) %>% 
  gtsave("temp.html")
webshot::webshot("temp.html", file = "figures/rsq_all.png", vwidth = 2000, vheight = 2000, selector = "div > table", expand = 5, zoom = 3)
file.remove("temp.html")




## figure 12 
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

div_calc <- 1 / 100

temp1 <- predraft_probs %>%
  filter(actual_pick <= 256, pick <= 256) %>%
  mutate(avail_true = as.numeric(pick <= actual_pick)) %>%
  filter(pick != 1) %>%
  mutate(round_win = round_any(available_prob,div_calc,round))

temp <- temp1 %>% 
  group_by(round_win) %>% 
  summarise(still_avail = sum(avail_true == 1),
            total_num = n(),
            calc = still_avail / total_num)

temp %>%
  ggplot(aes(round_win,calc)) + 
  geom_point(aes(size = total_num)) + 
  geom_smooth(se=F,method = 'loess',formula='y ~ x', na.rm=TRUE, lwd=1.5) + 
  geom_abline(aes(slope = 1, intercept = 0),lwd=1.5,color="red") +
  labs(      x = "Predicted Probability Still Available",
             y = "Actual Percentage Still Available") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1)) +
  annotate("text", x = .8,y = .2, label="Less Available\nThan Expected") +
  annotate("text", x = .2,y = .8, label="More Available\nThan Expected") +
  annotate("text", x = .5,y = .5, label="Perfect Prediction", angle = atan(1)*180/pi, vjust = -.75) +
  labs(size = "# Predictions") +
  theme_bw() +
  coord_fixed()


## table 5
results <- clean_mocks %>% 
  filter(DraftYear == 2020) %>% 
  select(CollegePlayerID, PlayerName, SourceName, ProjPick, DraftNumber) %>% 
  filter(DraftNumber < 300) %>% 
  inner_join(
    training_probs
  ) %>% 
  distinct() %>% 
  filter(y != 0) %>% 
  mutate(x = ifelse(x > 255, 256, x)) %>% 
  group_by(SourceName,CollegePlayerID,x) %>%
  mutate(y = sum(y)) %>%
  ungroup() %>%
  distinct() %>% 
  arrange(SourceName,CollegePlayerID) %>% 
  group_by(SourceName,CollegePlayerID) %>% 
  mutate(taken_by = cumsum(y)) %>% 
  mutate(avail = 1 - taken_by + y) %>% 
  ungroup() %>% 
  select(SourceName,CollegePlayerID,PlayerName,ProjPick,pick = x, pick_prob = y, available_prob = avail, actual_pick = DraftNumber) %>% 
  arrange(-actual_pick) %>% 
  filter(actual_pick < 300) %>% 
  bind_rows(
    predraft_probs %>% 
      filter(actual_pick < 300) %>% 
      mutate(SourceName = "Combined")
  ) %>% 
  mutate(taken_by = 1-available_prob + pick_prob) %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(actual_pick <= pick_high,
         actual_pick >= pick_low) %>% 
  select(-c(pick_high,pick_low))


brier_over_time <- crossing(pick = 1:max(results$pick),
                            CollegePlayerID = unique(results$CollegePlayerID),
                            SourceName = unique(results$SourceName)) %>% 
  inner_join(results %>% 
               select(SourceName,CollegePlayerID,PlayerName,actual_pick) %>% 
               distinct(), by = c("CollegePlayerID","SourceName")) %>% 
  left_join(results, by = c("pick", "CollegePlayerID", "PlayerName","actual_pick", "SourceName"))  %>% 
  mutate(taken_true = as.numeric(actual_pick <= pick),
         pick_prob = replace_na(pick_prob,0),
         taken_by = replace_na(taken_by,1),
         taken_true = replace_na(taken_true,1),
         picked_true = as.numeric(actual_pick == pick)) %>% 
  mutate(h_taken = (taken_by - taken_true)^2) %>% 
  group_by(SourceName,CollegePlayerID,actual_pick) %>%
  summarise(p_sum_taken = sum(h_taken),
            player_n = n()) %>% 
  ungroup() %>% 
  arrange(SourceName,actual_pick) %>% 
  mutate(p_count = 1) %>% 
  group_by(SourceName) %>% 
  mutate(cumul = cumsum(p_sum_taken),
         c_pred = cumsum(player_n)) %>% 
  ungroup() %>% 
  mutate(brier = (1/c_pred) * cumul)  %>% 
  left_join(clean_mocks %>% 
              select(CollegePlayerID,PlayerName,SourceName,ProjPick,error), by = c("SourceName", "CollegePlayerID"))


brier_over_time %>% 
  crossing(tibble(pick_low = c(1,33,65,107,147,180,215),
                  pick_high = c(32,64,106,146,179,214,256),
                  draft_round = c("Rd1","Rd2","Rd3","Rd4","Rd5","Rd6","Rd7"))) %>% 
  filter(actual_pick <= pick_high,
         actual_pick >= pick_low) %>% 
  select(-c(pick_high,pick_low)) %>% 
  group_by(SourceName, draft_round) %>% 
  mutate(player_count = n()) %>% 
  filter(player_count >= 20) %>% 
  filter(actual_pick == max(actual_pick)) %>% 
  ungroup() %>% 
  select(SourceName,draft_round,brier) %>% 
  spread(draft_round,brier) %>% 
  arrange(Rd1) %>% 
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    SourceName = md("**Source Name**"),
    Rd1 = md("**Rd1**"),
    Rd2 = md("**Rd2**"),
    Rd3 = md("**Rd3**"),
    Rd4 = md("**Rd4**"),
    Rd5 = md("**Rd5**"),
    Rd6 = md("**Rd5**"),
    Rd7 = md("**Rd5**"),
  ) %>% 
  fmt_number(
    columns = 2:8,
    decimals = 3
  ) %>% 
  fmt_missing(columns = 2:8, missing_text = "") %>% 
  tab_style(
    style = list(
      cell_fill(color = "yellow")
    ),
    locations = cells_body(
      rows = 4
    )) %>% 
  gtsave("temp.html")
webshot::webshot("temp.html", file = "figures/brier_all.png", vwidth = 2000, vheight = 2000, selector = "div > table", expand = 5, zoom = 3)
file.remove("temp.html")



