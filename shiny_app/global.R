library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(igraph)
library(DT)

steam_games_df <- readRDS('data/filtered_games_list.rds') %>% 
  mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
  mutate(release_year=replace(release_year, is.na(release_year) |
                                release_year < 1997 |
                                release_year > 2023, 'unknown'))

genres_relations_df <- readRDS('data/genres_relations.rds')

top_genres_list <- steam_games_df %>% 
  separate_rows(genres, sep = ';') %>% 
  count(genres, sort=TRUE, name='num_apps') %>% 
  head(10) %>% 
  pull(genres)

top_categories_list <- steam_games_df %>% 
  separate_rows(categories, sep = ';') %>% 
  count(categories, sort=TRUE, name='num_apps') %>% 
  head(10) %>% 
  pull(categories)


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