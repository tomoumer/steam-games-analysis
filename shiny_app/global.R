library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(igraph)

steam_games_df <- readRDS('data/filtered_games_list.rds') %>% 
  mutate(release_year=str_extract(release_date, '\\d{4}')) 

genres_relations_df <- readRDS('data/genres_relations.rds')

count_unique_var <- function(apps_df, var_name) {
  apps_summary <- apps_df %>% 
    filter(!is.na(.data[[var_name]]) & !is.na(release_year) & release_year <= 2022 & release_year >= 1997) %>%
    separate_rows(.data[[var_name]], sep = ';') %>% 
    group_by(release_year) %>% 
    summarize(n_unique_var=n_distinct(.data[[var_name]])) 
  
  return(apps_summary)
}

# this below could be a function too
apps_count_by_genre_year <- steam_games_df %>% 
  filter(!is.na(genres) & !is.na(release_year) & release_year >= 1997 & release_year <= 2023) %>%
  separate_rows(genres, sep = ';') %>% 
  select(name_app, release_year, genres) %>%
  count(genres, release_year, sort=TRUE, name='num_apps')

random_phrases <- c('That was fun, give me another five!',
                    'Ok, just one more time...',
                    'Stanley ... is that you?',
                    'The cake is a lie!',
                    "I've got nothing better to do",
                    "Gotta see them all!",
                    'Would you kindly show me five more?',
                    "Valve can't count to 3, so 5 is basically infinite.",
                    'Rise and shine, Mr. Freeman',
                    'MOOOOOOREEEEE!!',
                    'Do not click this button!',
                    'PRAISE THE DOG!')

random_icons <- c('dice','dice-one', 'pizza-slice', 'location-dot', 'music', 'poo', 'dice-six',
                  'dice-d6', 'shuffle', 'circle-question', 'cake-candles', 'book')