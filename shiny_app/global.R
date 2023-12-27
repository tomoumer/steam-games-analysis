library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(igraph)
library(DT)

#9s to load online, 2.2 sec on my comp
# changes of filters take around 1.7 sec


steam_games_df <- readRDS('./data/filtered_games.rds')
genres_relations_df <- readRDS('./data/genres_relations_by_year.rds')
games_genres_df <- readRDS('./data/genres_by_year.rds')
games_categories_df <- readRDS('./data/categories_by_year.rds')
games_developers_df <- readRDS('./data/developers_by_year.rds')
games_publishers_df <- readRDS('./data/publishers_by_year.rds')

top_genres_list <- games_genres_df %>% 
  group_by(genres) %>% 
  summarize(num_apps = sum(num_apps)) %>% 
  arrange(desc(num_apps)) %>% 
  head(5) %>% 
  pull(genres)

top_categories_list <- games_categories_df %>% 
  group_by(categories) %>% 
  summarize(num_apps = sum(num_apps)) %>% 
  arrange(desc(num_apps)) %>% 
  head(5) %>% 
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
                    'PRAISE THE DOG!',
                    'Verba volant, scripta manent',
                    'Ez Game, Ez Life',
                    'GIT GUD',
                    'For science!',
                    'I Took an Arrow in the Knee')

random_icons <- c('dice','dice-one', 'pizza-slice', 'location-dot', 'music', 'poo', 'dice-six',
                  'dice-d6', 'shuffle', 'circle-question', 'cake-candles', 'book', 'steam', 'github',
                  'dungeon', 'glasses', 'person-walking-arrow-right')