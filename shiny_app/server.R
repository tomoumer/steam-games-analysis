

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reactive values
  v <- reactiveValues(vertices_filtered = NULL,
                      relations_filtered = NULL,
                      steam_games_filtered = NULL,
                      graph_colors = c('#367fa9', '#1b2838'))
  
  
  observe({
    if(input$show_hide_games %% 2 == 0) {
      v$steam_games_filtered <- steam_games_df %>% 
        filter(release_year != 'unknown' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2])
      
      v$vertices_filtered <- v$steam_games_filtered %>% 
        separate_rows(genres, sep = ';') %>% 
        count(genres, sort=TRUE, name='num_apps')
      
      v$relations_filtered <- genres_relations_df %>% 
        filter(release_year != 'unknown' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2]) %>% 
        group_by(from, to) %>% 
        summarize(num_connections=sum(num_connections)) %>% 
        ungroup()
      
      v$graph_colors <- c('#367fa9')
      
      updateActionButton(session, 'show_hide_games', label = 'Show Unreleased', icon=icon('eye'))
    } else {
      # this first filter is a little convoluted ...
      # it's checking for the unreleased games, or, otherwise based on year filter
      v$steam_games_filtered <- steam_games_df %>% 
        filter(release_year == 'unknown' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2]))
      
      v$vertices_filtered <- v$steam_games_filtered %>% 
        separate_rows(genres, sep = ';') %>% 
        count(genres, sort=TRUE, name='num_apps')
      
      v$relations_filtered <- genres_relations_df %>% 
        filter(release_year == 'unknown' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2])) %>% 
        group_by(from, to) %>% 
        summarize(num_connections=sum(num_connections)) %>% 
        ungroup()
      
      v$graph_colors <- c('#367fa9', '#1b2838')
      
      updateActionButton(session, 'show_hide_games', label = 'Hide Unreleased', icon=icon('eye-slash'))
    }
  })
  
  # ====== OVERVIEW ========
  output$num_games <- renderValueBox({
    valueBox(
      format(v$steam_games_filtered %>%
               count() %>% pull(),
             big.mark   = ','),
      'Number of Games',
      icon = icon("gamepad"),
      color = "black"
    )
  })
  
  output$num_publishers <- renderValueBox({
    valueBox(
      format(v$steam_games_filtered %>% 
        separate_rows(publishers, sep = ';') %>% 
        summarize(n_distinct(publishers, na.rm=TRUE)) %>% 
        pull(),
        big.mark   = ','),
      'Number of Publishers',
      icon = icon("person-dots-from-line"),
      color = "black"
    )
  })
  
  output$num_developers <- renderValueBox({
    valueBox(
      format(v$steam_games_filtered %>% 
        separate_rows(developers, sep = ';') %>% 
        summarize(n_distinct(developers, na.rm=TRUE)) %>% 
        pull(),
        big.mark   = ','),
      'Number of Developers',
      icon = icon("person-digging"),
      color = "black"
    )
  })
  
  output$perc_free <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_filtered %>% 
                      summarize(100*sum(is_free)/n()) %>% 
                      pull(), digits=3), '%'),
      'Free games',
      icon = icon("comments-dollar"),
      color = "blue"
    )
  })
  
  output$perc_dlc <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_filtered %>% 
                      summarize(100*sum(!is.na(dlc))/n()) %>% 
                      pull(), digits=3), '%'),
      'Games with dlc (downloadable content)',
      icon = icon("download"),
      color = "blue"
    )
  })
  
  output$perc_achievements <- renderValueBox({
    valueBox(
      paste0(format(v$steam_games_filtered %>% 
                      summarize(100*sum(!is.na(achievements))/n()) %>% 
                      pull(), digits=3), '%'),
      'Games with achievements',
      icon = icon("trophy"),
      color = "blue"
    )
  })
  
  
  
  output$games_by_year <- renderPlotly({
    v$steam_games_filtered %>%
      mutate(release_type=if_else(release_year == 'unknown' | release_year == 2023, 'Unreleased', 'Released')) %>%
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
             legend = list(x = 0.1, y = 0.9)
      ) %>% 
      config(displayModeBar = FALSE)
    # if wanting to just hide some of the controls but not all:
    #config(displaylogo = FALSE,
    #       modeBarButtonsToRemove = c('toImage','zoomIn2d','zoomOut2d','zoom2d','pan2d','select2d','lasso2d','autoScale2d'))
  })
  
  
  
  # ===== GAMES OVER TIME (TRENDS)
  
  observe({
    if(input$num_or_perc %% 2 == 0) {
      
      updateActionButton(session, 'num_or_perc', label = 'Display Total Values', icon=icon('calculator'))
    } else {
      
      updateActionButton(session, 'num_or_perc', label = 'Display Percentage', icon=icon('percent'))
    }
  })
  
  
  output$top_genres <- renderPlotly({
    v$steam_games_filtered %>% 
      separate_rows(genres, sep = ';') %>% 
      filter(genres %in% top_genres_list) %>% 
      group_by(release_year) %>% 
      mutate(games_per_year=n_distinct(name_app)) %>% 
      group_by(release_year, genres) %>% 
      summarize(genres_by_year = if_else(input$num_or_perc %% 2 == 0,
                                         100*n()/min(games_per_year),
                                         as.double(n())
      )
      ) %>% 
      #slice_max(order_by = genres_by_year, n=7) %>% 
      ungroup() %>% 
      plot_ly(
        x = ~release_year,
        y = ~genres_by_year,
        color= ~genres,
        name = ~genres,
        colors = 'Paired',
        type = 'scatter',
        mode= 'lines'
      ) %>% 
      layout(title = 'Top 12 Game Genres',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$top_categories <- renderPlotly({
    v$steam_games_filtered %>% 
      separate_rows(categories, sep = ';') %>% 
      filter(categories %in% top_categories_list) %>% 
      group_by(release_year) %>% 
      mutate(games_per_year=n_distinct(name_app)) %>% 
      group_by(release_year, categories) %>% 
      summarize(categories_by_year=if_else(input$num_or_perc %% 2 == 0,
                                           100*n()/min(games_per_year),
                                           as.double(n())
      )) %>% 
      ungroup() %>% 
      plot_ly(
        x = ~release_year,
        y = ~categories_by_year,
        color= ~categories,
        name = ~categories,
        colors = 'Paired',
        type = 'scatter',
        mode= 'lines'
      ) %>% 
      layout(title = 'Top 12 Game Categories by Year',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$platforms_percentage <- renderPlotly({
    v$steam_games_filtered %>% 
      separate(windows_mac_linux, c('win', 'mac', 'linux'), sep = ';') %>% 
      group_by(release_year) %>% 
      summarize(perc_win= if_else(input$num_or_perc %% 2 == 0, 100*sum(as.logical(win))/n(), as.double(sum(as.logical(win)))),
                perc_mac= if_else(input$num_or_perc %% 2 == 0, 100*sum(as.logical(mac))/n(), as.double(sum(as.logical(mac)))),
                perc_linux= if_else(input$num_or_perc %% 2 == 0, 100*sum(as.logical(linux))/n(), as.double(sum(as.logical(linux))))
      )%>% 
      plot_ly(
        x = ~release_year,
        y = ~perc_win,
        name = "Windows",
        type = "scatter",
        mode= 'lines'
      ) %>%
      add_trace(y = ~perc_mac, name = 'Mac OS') %>% 
      add_trace(y = ~perc_linux, name = 'Linux') %>% 
      layout(title = 'Operating Systems',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$ratings <- renderPlotly({
    v$steam_games_filtered %>% 
      group_by(release_year) %>% 
      summarize(perc_has_metacritic= if_else(input$num_or_perc %% 2 == 0, 100*sum(!is.na(metacritic))/n(), as.double(sum(!is.na(metacritic)))),
                perc_has_recommended= if_else(input$num_or_perc %% 2 == 0, 100*sum(!is.na(recommended))/n(), as.double(sum(!is.na(recommended))))
      ) %>% 
      plot_ly(
        x = ~release_year,
        y = ~perc_has_metacritic,
        name ='has Metacritic Scores',
        type = "scatter",
        mode= 'lines'
      ) %>% 
      add_trace(y = ~perc_has_recommended, name = 'has Recommendations') %>%
      layout(title = 'Games With Metacritic or User Ratings',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games')) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$other_stats <- renderPlotly({
    v$steam_games_filtered %>% 
      group_by(release_year) %>% 
      summarize(perc_is_free= if_else(input$num_or_perc %% 2 == 0, 100*sum(is_free)/n(), as.double(sum(is_free))),
                perc_has_dlc= if_else(input$num_or_perc %% 2 == 0, 100*sum(!is.na(dlc))/n(), as.double(sum(!is.na(dlc)))),
                perc_has_achievements= if_else(input$num_or_perc %% 2 == 0, 100*sum(!is.na(achievements))/n(), as.double(sum(!is.na(achievements))))
      ) %>% 
      plot_ly(
        x = ~release_year,
        y = ~perc_has_achievements,
        name ='has Achievements',
        type = "scatter",
        mode= 'lines'
      ) %>% 
      add_trace(y = ~perc_has_dlc, name = 'has DLC') %>%
      add_trace(y = ~perc_is_free, name = 'is Free') %>% 
      layout(title = 'Games With Given Attributes',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Percentage of Video Games')) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$screenhots_trailers <- renderPlotly({
    v$steam_games_filtered %>% 
      mutate(n_publishers = str_count(publishers, ';') + 1,
             n_developers = str_count(developers, ';') + 1) %>% 
      mutate(n_publishers = replace_na(n_publishers, 0),
             n_developers = replace_na(n_developers, 0)) %>% 
      group_by(release_year) %>% 
      summarize(secreenshots_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_screenshots)/n(), as.double(sum(n_screenshots))),
                trailers_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_trailers)/n(), as.double(sum(n_trailers)))#,
                # decided not to use devs and publishers
                #publishers_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_publishers)/n(), as.double(sum(n_publishers))),
                #developers_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_developers)/n(), as.double(sum(n_developers)))
      ) %>% 
      plot_ly(
        x = ~release_year,
        y = ~secreenshots_per_game,
        name ='Screenshots',
        type = "scatter",
        mode= 'lines'
      ) %>% 
      add_trace(y = ~trailers_per_game, name = 'Trailers') %>%
      #add_trace(y = ~publishers_per_game, name = 'Publishers') %>%
      #add_trace(y = ~developers_per_game, name = 'Developers') %>%
      layout(title = 'Number of Trailers or Screenshots per game',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Average')) %>% 
      config(displayModeBar = FALSE)
  })
  
  # ============ GENRES tab
  
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
                                     opacity = 0.7,color='#367fa9'),
                       text = paste(unique_names, '<br>Games count:', v$vertices_filtered$num_apps),
                       texttemplate = if_else(100* (v$vertices_filtered %>% pull(num_apps)) / (v$vertices_filtered %>% select(num_apps) %>% sum()) >10, unique_names, ''),
                       hoverinfo = 'text'
    ) %>%
      add_text(textposition= 'right') %>% 
      config(displayModeBar = FALSE)
    
    edge_shapes <- list()
    
    for(i in 1:Ne) {
      v0 <- es[i,]$V1
      v1 <- es[i,]$V2
      edge_shape = list(
        type = 'line',
        line = list(color = "#1b2838",
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
        filter(release_year=='unknown' & 
                 !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') &
                 !str_detect(release_date, '\\d{4}') 
        ) %>% 
        select(name_app, release_date) %>% 
        count('Release Date' = release_date, name='Number of Occurences') %>% 
        slice_sample(n=5)
    })
  })
  
  # table showing games
  output$games_list <- renderDataTable({
    steam_games_df %>% 
      select(ID, 'Game Name' = name_app, 'Release Date' = release_date)
  },     options = list(
    pageLength = 10))
  
})
