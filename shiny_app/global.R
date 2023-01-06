library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

steam_games_df <- readRDS('data/filtered_games_list.rds')