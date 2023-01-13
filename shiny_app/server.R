

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
  
  # the table showing 5 random release dates
  observeEvent(input$random_five, {
    updateActionButton(session, 'random_five', label = sample(random_phrases, 1), icon= icon(sample(random_icons, 1)))
    
    output$fun_times <- renderTable({
      
      steam_games_df %>% 
        select(name_app, release_date) %>% 
        mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
        filter(is.na(release_year) & 
                 !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') 
        ) %>% 
        count('Release Date' = release_date, name='Number of Occurences') %>% 
        slice_sample(n=5)
    })
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
      filter(!is.na(release_year) & release_year <= 2022 & release_year >= 1997) %>%
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
      filter(!is.na(release_year) & release_year <= 2022 & release_year >= 1997) %>%
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
  
  v <- reactiveValues(vertices_filtered = NULL,
                      relations_filtered = NULL)
  
  observeEvent(input$draw_network, {
    v$vertices_filtered <- apps_count_by_genre_year %>% 
      filter(release_year >= input$release_years[1] & release_year <= input$release_years[2]) %>% 
      group_by(genres) %>% 
      summarize(num_apps=sum(num_apps)) %>% 
      arrange(desc(num_apps))
  })
  
  observeEvent(input$draw_network, {
    v$relations_filtered <- genres_relations_df %>% 
      filter(release_year >= input$release_years[1] & release_year <= input$release_years[2])  %>% 
      group_by(from, to) %>% 
      summarize(num_connections=sum(num_connections)) %>% 
      ungroup()
  })
  
  output$genres_network <- renderPlotly({
    if (is.null(v$vertices_filtered) | is.null(v$relations_filtered)) return()
    
    g <- graph_from_data_frame(v$relations_filtered, directed=FALSE, vertices=v$vertices_filtered )
    
    G <- upgrade_graph(g)
    L <- layout_nicely(G)
    
    vs <- V(G)
    es <- as.data.frame(get.edgelist(G))
    Nv <- length(vs)
    Ne <- length(es[1]$V1)
    
    Xn <- L[,1]
    Yn <- L[,2]
    
    unique_names <- unique(v$vertices_filtered$genres)
    names(Xn) <- unique_names
    names(Yn) <- unique_names
    
    network <- plot_ly(x = ~Xn, 
                       y = ~Yn, 
                       mode = 'markers', 
                       marker = list(size = 100* (v$vertices_filtered %>% pull(num_apps)) / (v$vertices_filtered %>% select(num_apps) %>% sum()),
                                     opacity = 0.7),
                       text = paste(unique_names, '<br>Games count:', v$vertices_filtered$num_apps),
                       texttemplate = if_else(100* (v$vertices_filtered %>% pull(num_apps)) / (v$vertices_filtered %>% select(num_apps) %>% sum()) >10, unique_names, ''),
                       hoverinfo = 'text'
    ) %>%
      add_text(textposition= 'right')
    
    edge_shapes <- list()
    
    for(i in 1:Ne) {
      v0 <- es[i,]$V1
      v1 <- es[i,]$V2
      edge_shape = list(
        type = 'line',
        line = list(color = "#030303",
                    width = 30 * (v$relations_filtered %>% pull(num_connections))[i] / (v$relations_filtered %>% select('num_connections') %>% sum())
        ),
        x0 = Xn[v0],
        y0 = Yn[v0],
        x1 = Xn[v1],
        y1 = Yn[v1]
      )
      edge_shapes[[i]] <- edge_shape
    }
    
    axis <- list(title = '', showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    
    fig <- layout(
      network,
      title = 'Genres Network',
      shapes = edge_shapes,
      xaxis = axis,
      yaxis = axis
    )
    
    fig
  })
  
})
