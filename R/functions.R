
fetch_pbp_data <- function(start_year = 2009, end_year = 2009, path_var = "data/"){
  
  reg_pbp <- data.frame()
  
  for (year in start_year:end_year) {
    
    print(glue("Fetching data for year {year}"))
    file <- glue("reg_pbp_{year}.csv")
    path <- glue("{path_var}{file}")
    
    tmp_df <- 
      read_csv(path, col_types = cols(.default = col_character())) %>% 
      mutate(
        season = year
      )
    
    reg_pbp <- 
      reg_pbp %>% 
      bind_rows(tmp_df)
  }
  
  return(reg_pbp)
}

calc_qbr <- function(completions, attempts, yards, tds, ints){
  
  a = case_when(
    ((completions / attempts) - .3) * 5 > 2.375 ~ 2.375,
    ((completions / attempts) - .3) * 5 < 0 ~ 0,
    TRUE ~ ((completions / attempts) - .3) * 5
  )
  
  b = case_when(
    ((yards / attempts) - 3) * .25 > 2.375 ~ 2.375,
    ((yards / attempts) - 3) * .25 < 0 ~ 0,
    TRUE ~ ((yards / attempts) - 3) * .25
  )
  
  c = case_when(
    (tds / attempts) * 20 > 2.375 ~ 2.375,
    (tds / attempts) * 20 < 0 ~ 0,
    TRUE ~ (tds / attempts) * 20
  )
  
  d = case_when(
    2.375 - ((ints / attempts) * 25) > 2.375 ~ 2.375,
    2.375 - ((ints / attempts) * 25) < 0 ~ 0,
    TRUE ~ 2.375 - ((ints / attempts) * 25)
  )
  
  return(round(((a + b + c+ d) / (6)) * 100, 1))
  
}
