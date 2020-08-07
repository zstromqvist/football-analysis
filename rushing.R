library(tidyverse)
library(readr)
library(glue)
library(lubridate)

options(dplyr.summarise.inform = F)

setwd('~/projects/nfl/')
source("functions.R")

stadiums <- read_csv('stadiums.csv')
reg_pbp <- 
  fetch_pbp_data(2009, 2019)

run_df <- 
  reg_pbp %>% 
  filter(season %in% c(2009:2018)) %>% 
  #filter(game_id == 2019090500) %>% 
  filter(rush_attempt == 1) %>% 
  mutate(
    game_date = date(game_date),
    game_id = as.numeric(game_id),
    play_id = as.numeric(play_id),
    drive = as.numeric(drive),
    sp = as.numeric(sp),
    qtr = as.numeric(qtr),
    yards_gained = as.numeric(yards_gained),
    rush_attempt = as.numeric(rush_attempt),
    two_point_attempt = as.numeric(two_point_attempt),
    rush_touchdown = as.numeric(rush_touchdown),
    yardline_100 = as.numeric(yardline_100),
    down = as.numeric(down),
    goal_to_go = as.numeric(goal_to_go),
    ydstogo = as.numeric(ydstogo)
  ) %>% 
  select(
    season,
    game_date,
    game_id,
    play_id,
    home_team,
    away_team,
    posteam,
    defteam,
    posteam_type,
    side_of_field,
    drive,
    sp,
    qtr,
    desc,
    rusher_player_name,
    yards_gained,
    game_half,
    penalty,
    play_type,
    rush_attempt,
    touchdown,
    rush_touchdown,
    two_point_attempt,
    yardline_100,
    down,
    ydstogo,
    goal_to_go,
    yrdln,
    run_location,
    run_gap
  ) %>% 
  left_join(stadiums, by = c('home_team' = 'team', 'season' = 'season')) %>% 
  mutate(
    ydstogo_mod = case_when(
      down == 1 & (ydstogo > 10 & ydstogo < 15) ~ '10+',
      down == 1 & (ydstogo > 15 & ydstogo < 20) ~ '15+',
      down == 1 & (ydstogo > 20) ~ '20+',
      down == 2 & (ydstogo > 20) ~ '20+',
      down == 3 & (ydstogo > 15) ~ '15+',
      down == 4 & (ydstogo > 2) ~ '2+',
      TRUE ~ as.character(ydstogo)
    )
  )

run_output <- 
  run_df %>% 
  group_by(rusher_player_name) %>% 
  summarise(
    games = length(unique(game_id)),
    attempts = sum(rush_attempt) - sum(two_point_attempt),
    yards = sum(yards_gained),
    tds = sum(rush_touchdown)
  ) %>% 
  arrange(desc(yards))

avg_ypc <- 
  run_df %>% 
  filter(!game_id %in% c(2013112401, 2013120101)) %>% 
  group_by(down, ydstogo_mod) %>% 
  summarise(
    attempts = n(),
    rank = min(ydstogo),
    avg_ypc = round(mean(yards_gained), 3)
  ) %>% 
  arrange(
    down,
    rank
  )

test <- avg_ypc %>% filter(attempts < 100)
