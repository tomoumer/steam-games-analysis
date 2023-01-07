

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # the overview plot 
  output$games_by_year <- renderPlotly({
    steam_games_df %>% 
      select(name_app, release_date) %>% 
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(!is.na(release_year) & release_year <= 2027 & release_year >= 1997) %>% 
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
  
  # the table showing 5 random release dates
  output$fun_times <- renderTable({
    input$action
    
    steam_games_df %>% 
      select(name_app, release_date) %>% 
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(is.na(release_year) & 
               !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') 
      ) %>% 
      count('Release Date' = release_date, name='Number of Occurences') %>% 
      slice_sample(n=5)
  })
  
  # table showing games
  output$games_list <- renderDataTable({
    steam_games_df %>% 
      select(ID, 'Game Name' = name_app, 'Release Date' = release_date)
  })
  
  # analysis over time
  output$pubs_by_year <- renderDataTable ({
    count_unique_var(apps_df=steam_games_df, var_name='publishers')
  })
  
  output$devs_by_year <- renderDataTable ({
    count_unique_var(apps_df=steam_games_df, var_name='developers')
  })
  
  # linux, mac windows
  output$platforms_availability <- renderPlotly({
    steam_games_df %>% 
      filter(!is.na(windows_mac_linux)) %>%
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(!is.na(release_year) & release_year <= 2022 & release_year >= 2008) %>%
      separate(windows_mac_linux, c('win', 'mac', 'linux'), sep = ';') %>% 
      group_by(release_year) %>% 
      summarize(total=n(), num_win=sum(as.logical(win)), num_mac=sum(as.logical(mac)), num_linux=sum(as.logical(linux))) %>% 
      mutate(perc_win=num_win/total, perc_mac=num_mac/total, perc_linux=num_linux/total) %>% 
      plot_ly(
        x = ~release_year,
        y = ~num_win,
        name = "Windows",
        type = "bar"
      ) %>%
      add_trace(y = ~num_mac, name = 'MacOS', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(y = ~num_linux, name = 'Linux', marker = list(color = 'rgb(100,100,100)'))
  })
  
  output$platforms_percentage <- renderPlotly({
    steam_games_df %>% 
      filter(!is.na(windows_mac_linux)) %>%
      mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      filter(!is.na(release_year) & release_year <= 2022 & release_year >= 2008) %>%
      separate(windows_mac_linux, c('win', 'mac', 'linux'), sep = ';') %>% 
      group_by(release_year) %>% 
      summarize(total=n(), num_win=sum(as.logical(win)), num_mac=sum(as.logical(mac)), num_linux=sum(as.logical(linux))) %>% 
      mutate(perc_win=num_win/total, perc_mac=num_mac/total, perc_linux=num_linux/total) %>% 
      plot_ly(
        x = ~release_year,
        y = ~perc_mac,
        name = "MacOS",
        marker = list(color = 'rgb(204,204,204)'),
        type = "bar"
      ) %>%
      add_trace(y = ~perc_linux, name = 'Linux', marker = list(color = 'rgb(100,100,100)'))
  })
  
})
