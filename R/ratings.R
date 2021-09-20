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


# markov ratings
teams <- sort(unique(c(dd$home_team, dd$away_team)))

ratings_example_markov <- c()

for (team in teams) {
  
  t <- 
    data.frame(
      team = team,
      join = 1
    )
  
  y <-
    data.frame(
      opp = teams,
      join = 1
    )
  
  base <-
    t %>% 
    full_join(y, by = "join") %>% 
    select(-join)
  
  own <- 
    dd %>% 
    slice(0) %>% 
    add_row(
      home_team = team,
      away_team = team,
      home_score = 0,
      away_score = 0,
      home_yards_pass = 0,
      home_yards_run = 0,
      away_yards_pass = 0,
      away_yards_run = 0
    )
  
  tmp_df <- 
    dd %>% 
    filter(home_team == team | away_team == team) %>% 
    bind_rows(own) %>% 
    transmute(
      #old_game_id,
      team = team,
      opp = case_when(
        home_team == team ~ away_team,
        TRUE ~ home_team
      ),
      score = case_when(
        home_team == team ~ home_score,
        TRUE ~ away_score
      ),
      opp_score = case_when(
        home_team == team ~ away_score,
        TRUE ~ home_score
      )
    ) %>% 
    group_by(team, opp) %>% 
    summarise(
      score = sum(score),
      opp_score = sum(opp_score)
    ) %>% 
    right_join(base, by = c('team', 'opp')) %>% 
    arrange(
      team,
      opp
    ) %>% 
    mutate(
      score = replace_na(score, 0),
      opp_score = replace_na(opp_score, 0)
    )
  
  ratings_example_markov <- 
    ratings_example_markov %>% 
    bind_rows(tmp_df)
  
}

votes_per_team <- 
  ratings_example_markov %>% 
  mutate(
    vote = case_when(
      score < opp_score ~ 1,
      TRUE ~ 0
    )
  )

S <- matrix(votes_per_team$vote, nrow = length(teams), ncol = length(teams), byrow = T)
S2 <- matrix(votes_per_team$score, nrow = length(teams), ncol = length(teams), byrow = F)

for (row in 1:length(teams)) {
  if(sum(S[row,]) == 0){
    S[row,] = rep(1/length(teams), lenght(teams))
  } else {
    S[row,] = S[row,] / sum(S[row,])
  }
  
}

for (row in 1:length(teams)) {

  S2[row,] = S2[row,] / sum(S2[row,])
  
}

#
last_team <- 3
team_vector <- c()
jumps <- 10000

for (i in 1:jumps) {
  
  next_team <- sample(1:length(teams), 1, FALSE, S2[last_team,])
  team_vector[i] <- next_team
  last_team <- next_team
}

f <- 
  as.data.frame(table(team_vector)/jumps) %>% 
  transmute(
    team_vector = as.character(team_vector),
    points = Freq
  )

output <-
  data.frame(
    team = teams,
    team_id = as.character(1:32)
  ) %>% 
  left_join(
    f,
    by = c('team_id' = 'team_vector')
  ) %>% 
  mutate(
    points = replace_na(points, 0+0.001)
  ) %>% 
  select(-team_id) %>% 
  arrange(desc(points))
