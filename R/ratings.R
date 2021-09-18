library(tidyverse)
library(nflfastR)

pbp <- nflfastR::load_pbp(2020)


ff <- 
  pbp %>% 
  mutate(
    home_yards = case_when(
      posteam == home_team ~ yards_gained,
      TRUE ~ 0
    ),
    away_yards = case_when(
      posteam == away_team ~ yards_gained,
      TRUE ~ 0
    )
  ) %>% 
  filter(
    play_type %in% c('pass', 'run')
  ) %>% 
  group_by(old_game_id, home_team, away_team, play_type) %>% 
  summarise(
    home_yards = sum(home_yards),
    away_yards = sum(away_yards)
  ) %>% 
  pivot_wider(
    id_cols = c('old_game_id'),
    names_from = 'play_type',
    values_from = c('home_yards', 'away_yards')
  )

gg <- 
  pbp %>% 
  group_by(old_game_id, home_team, away_team) %>% 
  summarise(
    home_score = max(home_score),
    away_score = max(away_score)
  )


dd <-
  gg %>% 
  left_join(ff)
