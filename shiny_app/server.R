

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reactive values
  v <- reactiveValues(vertices_filtered = NULL,
                      relations_filtered = NULL,
                      steam_games_release = steam_games_df,
                      graph_colors = c('#1f77b4', '#9467bd'))
  
  
  observe({
    if(input$check_released) {
      v$steam_games_release <- steam_games_df %>% 
        filter(!is.na(release_year) & release_year <= 2022 & release_year >= 1997)
      
      v$graph_colors <- c('#1f77b4')
    } else {
      v$steam_games_release <- steam_games_df
      
      v$graph_colors <- c('#1f77b4', '#9467bd')
    }
  })
  
  # ====== OVERVIEW ========
  output$num_games <- renderValueBox({
    valueBox(
      v$steam_games_release %>% 
        count() %>% 
        pull(),
      'Number of Games',
      icon = icon("gamepad"),
      color = "purple"
    )
  })
  
  output$num_publishers <- renderValueBox({
    valueBox(
      v$steam_games_release %>% 
        separate_rows(publishers, sep = ';') %>% 
        summarize(n_distinct(publishers, na.rm=TRUE)) %>% 
        pull(),
      'Number of Publishers',
      icon = icon("person-dots-from-line"),
      color = "purple"
    )
  })
  
  output$num_developers <- renderValueBox({
    valueBox(
      v$steam_games_release %>% 
        separate_rows(developers, sep = ';') %>% 
        summarize(n_distinct(developers, na.rm=TRUE)) %>% 
        pull(),
      'Number of Developers',
      icon = icon("person-digging"),
      color = "purple"
    )
  })
  
  output$perc_free <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_release %>% 
                      summarize(100*sum(is_free)/n()) %>% 
                      pull(), digits=3), '%'),
      'Free games',
      icon = icon("comments-dollar"),
      color = "blue"
    )
  })
  
  output$perc_dlc <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_release %>% 
                      summarize(100*sum(!is.na(dlc))/n()) %>% 
                      pull(), digits=3), '%'),
      'Games with dlc (downloadable content)',
      icon = icon("download"),
      color = "blue"
    )
  })
  
  output$perc_achievements <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_release %>% 
                      summarize(100*sum(!is.na(achievements))/n()) %>% 
                      pull(), digits=3), '%'),
      'Games with achievements',
      icon = icon("trophy"),
      color = "blue"
    )
  })
  
  
  
  output$games_by_year <- renderPlotly({
    v$steam_games_release %>%
      mutate(release_year=replace_na(release_year, '3333'),
             release_type=if_else((release_year >= 1997 & release_year <=2022), 'Released', 'Unreleased')) %>%
      count(release_year, release_type, name='num_games') %>% 
      plot_ly(
        x = ~release_year,
        y = ~num_games,
        text = ~num_games,
        textposition = 'outside',
        texttemplate = ~if_else(num_games >= 1000, '%{y:.2s}', '%{y:.1s}'),
        hovertemplate = ~paste('Year:', release_year, '<br>N. Games:', num_games),
        name = ~release_type,
        type = 'bar',
        color = ~release_type,
        colors = v$graph_colors
      ) %>% 
      layout(title = 'Video Game Releases over Years',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Number of Video Games'),
             legend = list(x = 0.1, y = 0.9))
  })
  
  
  # table showing games
  output$games_list <- renderDataTable({
    steam_games_df %>% 
      select(ID, 'Game Name' = name_app, 'Release Date' = release_date)
  },     options = list(
    pageLength = 10))
  
  
  # ===== GAMES OVER TIME (TRENDS)
  
  output$top5_genres <- renderPlotly({
    steam_games_df %>% 
      filter(!is.na(genres) &
               !is.na(release_year) &
               release_year >= input$release_years_filter[1] &
               release_year <= input$release_years_filter[2]) %>%
      separate_rows(genres, sep = ';') %>% 
      group_by(release_year) %>% 
      mutate(games_per_year=n_distinct(name_app)) %>% 
      group_by(release_year, genres) %>% 
      summarize(genres_by_year=n()/min(games_per_year)) %>% 
      slice_max(order_by = genres_by_year, n=5) %>% 
      ungroup() %>% 
      plot_ly(
        x = ~release_year,
        y = ~genres_by_year,
        color= ~genres,
        name = ~genres,
        type = 'scatter',
        mode= 'lines'
      ) %>% 
      layout(title = 'Game Genres',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games'))
  })
  
  output$platforms_percentage <- renderPlotly({
    steam_games_df %>% 
      filter(!is.na(windows_mac_linux) &
               !is.na(release_year) &
               release_year >= input$release_years_filter[1] &
               release_year <= input$release_years_filter[2]) %>%
      separate(windows_mac_linux, c('win', 'mac', 'linux'), sep = ';') %>% 
      group_by(release_year) %>% 
      summarize(total=n(), num_win=sum(as.logical(win)), num_mac=sum(as.logical(mac)), num_linux=sum(as.logical(linux))) %>% 
      mutate(perc_win=num_win/total, perc_mac=num_mac/total, perc_linux=num_linux/total) %>% 
      plot_ly(
        x = ~release_year,
        y = ~perc_win,
        name = "Windows",
        type = "scatter",
        mode= 'lines'
      ) %>%
      add_trace(y = ~perc_mac, name = 'Linux') %>% 
      add_trace(y = ~perc_linux, name = 'Linux') %>% 
      layout(title = 'Operating Systems',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games'))
  })
  
  # ============ GENRES tab
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
  
  # ============= FUN STUFF tab
  observeEvent(input$random_five, {
    updateActionButton(session, 'random_five', label = sample(random_phrases, 1), icon= icon(sample(random_icons, 1)))
    
    output$fun_times <- renderTable({
      
      steam_games_df %>% 
        filter(is.na(release_year) & 
                 !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') 
        ) %>% 
        select(name_app, release_date) %>% 
        count('Release Date' = release_date, name='Number of Occurences') %>% 
        slice_sample(n=5)
    })
  })
  
})
