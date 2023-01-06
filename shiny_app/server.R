

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$games_by_year <- renderPlotly({
    steam_games_df %>% 
      select(name_app, release_date) %>% 
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(!is.na(release_year) & release_year < 2028 & release_year > 1996) %>% 
      count(release_year, name='num_games') %>% 
      plot_ly(
      x = ~release_year,
      y = ~num_games,
      text = ~num_games,
      textposition = 'auto',
      name = "Games over years",
      type = "bar"
    )
  })
  
  output$fun_times <- renderTable({
    input$action
    
    steam_games_df %>% 
      select(name_app, release_date) %>% 
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(is.na(release_year) & 
               !str_detect(release_date, '[cC][oO][mM][iI][nN][gG] [sS][oO][oO][nN]') &
               !str_detect(release_date, '[tT][bB][aAdDcC]') &
               !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') &
               !str_detect(release_date, '[aA][nN][nN][oO][uU][nN][cC][eE]') &
               !str_detect(release_date, '[dD][eE][mM][oO]') &
               !str_detect(release_date, '[wW][iI][nN][tT][eE][rR]') &
               !str_detect(release_date, '[sS][pP][rR][iI][nN][gG]') &
               !str_detect(release_date, '[sS][uU][mM][mM][eE][rR]') &
               !str_detect(release_date, '[aA][uU][tT][uU][mM][nN]')) %>% 
      distinct('Release Date' = release_date) %>% 
      slice_sample(n=5)
  })
  
  output$games_list <- renderDataTable({
    steam_games_df %>% 
      select(ID, 'Game Name' = name_app, 'Release Date' = release_date)
  })
  
})
