library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

steam_games_df <- readRDS('data/filtered_games_list.rds')

count_unique_var <- function(apps_df, var_name) {
  apps_summary <- apps_df %>% 
    filter(!is.na(.data[[var_name]])) %>%
    mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
    filter(!is.na(release_year) & release_year <= 2022 & release_year >= 1997) %>%
    separate_rows(.data[[var_name]], sep = ';') %>% 
    group_by(release_year) %>% 
    summarize(n_unique_var=n_distinct(.data[[var_name]])) 
  
  return(apps_summary)
}