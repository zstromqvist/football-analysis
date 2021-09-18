library(tidyverse)
library(readr)
library(glue)
library(lubridate)

options(dplyr.summarise.inform = F)

setwd('~/projects/nfl')
source("R/functions.R")

stadiums <- read_csv('stadiums.csv')
reg_pbp <- 
  fetch_pbp_data(2019, 2019)

pass_df <- 
  reg_pbp %>% 
  filter(pass_attempt == 1) %>% 
  mutate(
    game_date = date(game_date),
    game_id = as.numeric(game_id),
    play_id = as.numeric(play_id),
    drive = as.numeric(drive),
    yards_gained = as.numeric(yards_gained),
    complete_pass = as.numeric(complete_pass),
    incomplete_pass = as.numeric(incomplete_pass),
    sack = as.numeric(sack),
    penalty = as.numeric(penalty),
    pass_attempt = as.numeric(pass_attempt),
    touchdown = as.numeric(touchdown),
    pass_touchdown = as.numeric(pass_touchdown),
    interception = as.numeric(interception),
    two_point_attempt = as.numeric(two_point_attempt)
  ) %>% 
  select(
    season,
    game_date,
    game_id,
    play_id,
    home_team,
    posteam,
    posteam_type,
    drive,
    desc:yards_after_catch,
    passer_player_name,
    complete_pass,
    incomplete_pass,
    sack,
    game_half,
    penalty,
    pass_attempt,
    touchdown,
    pass_touchdown,
    interception,
    two_point_attempt
    ) %>% 
  left_join(stadiums, by = c('home_team' = 'team', 'season' = 'season'))

output <- 
  pass_df %>% 
  filter(!game_id %in% c(2013112401, 2013120101)) %>% 
  filter(sack == 0) %>% 
  group_by(stadium_type, posteam_type) %>% 
  summarise(
    games = length(unique(game_id)),
    completions = sum(complete_pass),
    attempts = sum(pass_attempt) - sum(two_point_attempt),
    compl_perc = round(completions / attempts, 3),
    yards = sum(yards_gained),
    tds = sum(pass_touchdown),
    ints = sum(interception)
  ) %>% 
  ungroup() %>% 
  mutate(
    yards_per_game = round(yards / games),
    tds_per_game = round(tds / games, 2),
    td_int_ratio = round(tds / ints, 2),
    qbr = calc_qbr(completions, attempts, yards, tds, ints)
  ) %>% 
  arrange(desc(qbr))

qb_stats <- 
  pass_df %>% 
  filter(!game_id %in% c(2013112401, 2013120101)) %>% 
  filter(sack == 0) %>% 
  filter(season == 2018) %>% 
  group_by(passer_player_name) %>% 
  summarise(
    games = length(unique(game_id)),
    completions = sum(complete_pass),
    attempts = sum(pass_attempt) - sum(two_point_attempt),
    compl_perc = round(completions / attempts, 3),
    yards = sum(yards_gained),
    tds = sum(pass_touchdown),
    ints = sum(interception)
  ) %>% 
  ungroup() %>% 
  mutate(
    yards_per_game = round(yards / games),
    tds_per_game = round(tds / games, 2),
    td_int_ratio = round(tds / ints, 2),
    qbr = calc_qbr(completions, attempts, yards, tds, ints)
  ) %>% 
  select(
    -c(a,b,c,d)
  ) %>% 
  arrange(desc(yards))
