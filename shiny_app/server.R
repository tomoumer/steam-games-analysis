

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # reactive values

  steam_games_filtered <- reactive({
    if(input$show_hide_games %% 2 == 0) {
      steam_games_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2])
    } else {
      # the opposite of filter above
      steam_games_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2]))
    }
  })
  
  games_genres_filtered <- reactive ({
    if(input$show_hide_games %% 2 == 0) {
      games_genres_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2]) 
      
    } else {
      # the opposite of filter above
      games_genres_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2])) 
    }
  })
  
  games_genres_grouped <- reactive({
    games_genres_filtered() %>% 
      group_by(genres) %>% 
      summarize(num_apps = sum(num_apps)) %>% 
      arrange(desc(num_apps))
  })
  
  games_categories_grouped <- reactive({
    games_categories_filtered() %>% 
      group_by(categories) %>% 
      summarize(num_apps = sum(num_apps)) %>% 
      arrange(desc(num_apps))
  })
  
  genres_relations_filtered <- reactive({
    if(input$show_hide_games %% 2 == 0) {
      genres_relations_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2]) %>% 
        group_by(from, to) %>% 
        summarize(num_connections=sum(num_connections)) %>% 
        ungroup()
    } else {
      genres_relations_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2])) %>% 
        group_by(from, to) %>% 
        summarize(num_connections=sum(num_connections)) %>% 
        ungroup()
    }
  })
  
  games_categories_filtered <- reactive ({
    if(input$show_hide_games %% 2 == 0) {
      games_categories_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2])
      
    } else {
      games_categories_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2]))
    }
  })
  
  games_developers_filtered <- reactive ({
    if(input$show_hide_games %% 2 == 0) {
      games_developers_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2])
      
    } else {
      games_developers_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2]))
    }
  })
  
  games_publishers_filtered <- reactive ({
    if(input$show_hide_games %% 2 == 0) {
      games_publishers_df %>% 
        filter(release_year != '20XY' &
                 release_year != '2023' &
                 release_year >= input$release_years_filter[1] &
                 release_year <= input$release_years_filter[2]) 
      
    } else {
      # the opposite of filter above
      games_publishers_df %>% 
        filter(release_year == '20XY' |
                 release_year == '2023' |
                 (release_year >= input$release_years_filter[1] &
                    release_year <= input$release_years_filter[2]))
    }
  })
  
  # 
  observe({
    if(input$show_hide_games %% 2 == 0) {
      updateActionButton(session, 'show_hide_games', label = 'Show Unreleased', icon=icon('eye'))
    } else {
      updateActionButton(session, 'show_hide_games', label = 'Hide Unreleased', icon=icon('eye-slash'))
    }
  })
  
  # ====== OVERVIEW ========
  output$num_games <- renderValueBox({
    valueBox(
      format(steam_games_filtered() %>%
               count() %>% pull(),
             big.mark   = ','),
      'Number of Games',
      icon = icon("gamepad"),
      color = "black"
    )
  })
  
  output$num_publishers <- renderValueBox({
    valueBox(
      format(games_publishers_filtered() %>% 
               summarize(n_distinct(publishers)) %>% 
               pull(),
             big.mark   = ','),
      'Number of Publishers',
      icon = icon("person-dots-from-line"),
      color = "black"
    )
  })
  
  output$num_developers <- renderValueBox({
    valueBox(
      format(games_developers_filtered() %>% 
               summarize(n_distinct(developers)) %>% 
               pull(),
             big.mark   = ','),
      'Number of Developers',
      icon = icon("person-digging"),
      color = "black"
    )
  })
  
  
  
  output$games_by_year <- renderPlot({
    steam_games_filtered() %>%
      mutate(release_type=if_else(release_year == '20XY' | release_year == 2023, 'Unreleased', 'Released')) %>%
      count(release_year, release_type, name='num_games') %>% 
      ggplot(aes(x=release_year, y=num_games, fill=release_type, label=format(num_games, big.mark=',') )) +
      ggtitle('Video Game Releases by Year') +
      scale_fill_manual(values=c('#367fa9', '#1b2838')) +
      scale_color_manual(values=c('#367fa9', '#1b2838')) +
      geom_col() +
      geom_text(size=2.5, vjust=-0.5, aes(color=release_type)) +
      labs(x ='Release Year', y = 'Number of Video Games', fill='Game Status', color='Game Status') +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0), legend.position=c(0.15, 0.8))
  })
  
  
  
  # ===== GAMES OVER TIME (TRENDS)
  
  observe({
    if(input$num_or_perc %% 2 == 0) {
      
      updateActionButton(session, 'num_or_perc', label = 'Display Total Values', icon=icon('calculator'))
    } else {
      
      updateActionButton(session, 'num_or_perc', label = 'Display Percentage', icon=icon('percent'))
    }
  })
  
  # ==== TRENDS-genres subtab
  output$n1_genre <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_genres_grouped() %>% slice(1) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_genres_grouped() %>% slice(1) %>% pull(num_apps), big.mark = ',')),
      paste(games_genres_grouped() %>% slice(1) %>% pull(genres), 'Games'),
      icon = icon('1'),
      color = "black"
    )
  })
  
  output$n2_genre <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_genres_grouped() %>% slice(2) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_genres_grouped() %>% slice(2) %>% pull(num_apps), big.mark = ',')),
      paste(games_genres_grouped() %>% slice(2) %>% pull(genres), 'Games'),
      icon = icon('2'),
      color = "black"
    )
  })
  
  output$n3_genre <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_genres_grouped() %>% slice(3) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_genres_grouped() %>% slice(3) %>% pull(num_apps), big.mark = ',')),
      paste(games_genres_grouped() %>% slice(3) %>% pull(genres), 'Games'),
      icon = icon('3'),
      color = "black"
    )
  })
  
  output$top_genres <- renderPlot({
    games_genres_filtered() %>% 
      filter(genres %in% (games_genres_grouped() %>%
                            head(5) %>% 
                            pull(genres))
             ) %>% 
      left_join(steam_games_df %>% count(release_year, name='games_per_year'), by='release_year') %>% 
      group_by(release_year, genres) %>% 
      summarize(perc_or_num_apps=if_else(input$num_or_perc %% 2 == 0,
                                 100*num_apps/games_per_year,
                                 as.double(num_apps)),
                .groups = 'keep') %>% 
      ggplot(aes(x=release_year, y=perc_or_num_apps, color=genres, group=genres)) +
      geom_line() +
      ggtitle('Top 5 Game Genres by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Percentage of Video Games', 'Number of Video Games')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  # ==== TRENDS-categories subtab
  output$n1_category <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_categories_grouped() %>% slice(1) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_categories_grouped() %>% slice(1) %>% pull(num_apps), big.mark = ',')),
      paste(games_categories_grouped() %>% slice(1) %>% pull(categories), 'Games'),
      icon = icon('1'),
      color = "black"
    )
  })
  
  output$n2_category <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_categories_grouped() %>% slice(2) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_categories_grouped() %>% slice(2) %>% pull(num_apps), big.mark = ',')),
      paste(games_categories_grouped() %>% slice(2) %>% pull(categories), 'Games'),
      icon = icon('2'),
      color = "black"
    )
  })
  
  output$n3_category <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* games_categories_grouped() %>% slice(3) %>% pull(num_apps) / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(games_categories_grouped() %>% slice(3) %>% pull(num_apps), big.mark = ',')),
      paste(games_categories_grouped() %>% slice(3) %>% pull(categories), 'Games'),
      icon = icon('3'),
      color = "black"
    )
  })
  
  output$top_categories <- renderPlot({
    games_categories_filtered() %>% 
      filter(categories %in% (games_categories_grouped() %>%
                                head(5) %>%
                                pull(categories) )
             ) %>% 
      left_join(steam_games_df %>% count(release_year, name='games_per_year'), by='release_year') %>% 
      group_by(release_year, categories) %>% 
      summarize(perc_or_num_apps=if_else(input$num_or_perc %% 2 == 0,
                                          100*num_apps/games_per_year,
                                          as.double(num_apps)),
                .groups = 'keep') %>% 
      ggplot(aes(x=release_year, y=perc_or_num_apps, color=categories, group=categories)) +
      geom_line() +
      ggtitle('Top 5 Game Categories by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Percentage of Video Games', 'Number of Video Games')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  
  # ==== TRENDS-platforms subtab
  output$n_win <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(count_win= sum(is_win)) %>% pull(count_win) /
                              steam_games_filtered() %>% count() %>% pull(), digits=4), '%'),
              format(steam_games_filtered() %>% summarize(count_win= sum(is_win)) %>% pull(count_win), big.mark = ',')),
      'Windows Games',
      icon = icon('windows'),
      color = "black"
    )
  })
  
  output$n_mac <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(count_mac= sum(is_mac)) %>% pull(count_mac) /
                              steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(count_mac= sum(is_mac)) %>% pull(count_mac), big.mark = ',')),
      'MacOS Games',
      icon = icon('apple'),
      color = "black"
    )
  })
  
  output$n_linux <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(count_linux= sum(is_linux)) %>% pull(count_linux) /
                              steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(count_linux= sum(is_linux)) %>% pull(count_linux), big.mark = ',')),
      'Linux Games',
      icon = icon('linux'),
      color = "black"
    )
  })
  
  output$platforms_percentage <- renderPlot({
    steam_games_filtered() %>% 
      group_by(release_year) %>% 
      summarize(perc_win= if_else(input$num_or_perc %% 2 == 0, 100*sum(is_win)/n(), as.double(sum(is_win))),
                perc_mac= if_else(input$num_or_perc %% 2 == 0, 100*sum(is_mac)/n(), as.double(sum(is_mac))),
                perc_linux= if_else(input$num_or_perc %% 2 == 0, 100*sum(is_linux)/n(), as.double(sum(is_linux)))
      )%>% 
      ggplot(aes(x=release_year)) +
      geom_line(aes(y=perc_win, group=1, color='Windows')) +
      geom_line(aes(y=perc_mac, group=1, color='MacOS')) +
      geom_line(aes(y=perc_linux, group=1, color='Linux')) +
      scale_color_manual(name = 'Operating System', values = c('Windows' = '#F8766D', 'MacOS' = '#00BA38', 'Linux' = '#619CFF')) +
      ggtitle('Availability on Operating Systems by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Percentage of Video Games', 'Number of Video Games')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  # ==== TRENDS-ratings subtab
  output$n_metacritic <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(sum(has_metacritic)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(sum(has_metacritic)) %>% pull() , big.mark = ',')),
      'Has Metacritic Score',
      icon = icon('comments'),
      color = "black"
    )
  })
  
  output$n_recommended <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(sum(has_recommended)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(sum(has_recommended)) %>% pull() , big.mark = ',')),
      'People Recommended It',
      icon = icon('thumbs-up'),
      color = "black"
    )
  })
  
  output$ratings <- renderPlot({
    steam_games_filtered() %>% 
      group_by(release_year) %>% 
      summarize(perc_has_metacritic= if_else(input$num_or_perc %% 2 == 0, 100*sum(has_metacritic)/n(), as.double(sum(has_metacritic))),
                perc_has_recommended= if_else(input$num_or_perc %% 2 == 0, 100*sum(has_recommended)/n(), as.double(sum(has_recommended)))
      ) %>% 
      ggplot(aes(x=release_year)) +
      geom_line(aes(y=perc_has_metacritic, group=1, color='has Metacritic Scores')) +
      geom_line(aes(y=perc_has_recommended, group=1, color='has Recommendations')) +
      scale_color_manual(name = 'Legend', values = c('has Metacritic Scores' = '#F8766D', 'has Recommendations' = '#00BA38')) +
      ggtitle('Games with Metacritic or User Ratings by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Percentage of Video Games', 'Number of Video Games')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  # ==== TRENDS-other_stats subtab
  output$n_free <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(sum(is_free)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(sum(is_free)) %>% pull() , big.mark = ',')),
      'Free Games',
      icon = icon("comments-dollar"),
      color = "black"
    )
  })
  
  output$n_dlc <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(sum(has_dlc)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(sum(has_dlc)) %>% pull() , big.mark = ',')),
      'Games with DLC (downloadable content)',
      icon = icon("download"),
      color = "black"
    )
  })
  
  output$n_achievements <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              paste0(format(100* steam_games_filtered() %>% summarize(sum(has_achievements)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3), '%'),
              format(steam_games_filtered() %>% summarize(sum(has_achievements)) %>% pull() , big.mark = ',')),
      'Games with Achievements',
      icon = icon("trophy"),
      color = "black"
    )
  })
  
  
  output$other_stats <- renderPlot({
    steam_games_filtered() %>% 
      group_by(release_year) %>% 
      summarize(perc_is_free= if_else(input$num_or_perc %% 2 == 0, 100*sum(is_free)/n(), as.double(sum(is_free))),
                perc_has_dlc= if_else(input$num_or_perc %% 2 == 0, 100*sum(has_dlc)/n(), as.double(sum(has_dlc))),
                perc_has_achievements= if_else(input$num_or_perc %% 2 == 0, 100*sum(has_achievements)/n(), as.double(sum(has_achievements)))
      ) %>% 
      ggplot(aes(x=release_year)) +
      geom_line(aes(y=perc_is_free, group=1, color='has Achievementss')) +
      geom_line(aes(y=perc_has_dlc, group=1, color='has DLC')) +
      geom_line(aes(y=perc_has_achievements, group=1, color='is Free')) +
      scale_color_manual(name = 'Legend', values = c('has Achievements' = '#F8766D', 'has DLC' = '#00BA38', 'is Free' = '#619CFF')) +
      ggtitle('Games with Given Attributes by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Percentage of Video Games', 'Number of Video Games')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  # ==== TRENDS-screenshots & trailers subtab
  output$nr_screenshots <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              format(steam_games_filtered() %>% summarize(sum(n_screenshots)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3),
              format(steam_games_filtered() %>% summarize(sum(n_screenshots)) %>% pull() , big.mark = ',')),
      if_else(input$num_or_perc %% 2 == 0,
              'Screenshots Per Game',
              'Screenshots'),
      icon = icon("file-image"),
      color = "black"
    )
  })
  
  output$nr_trailers <- renderValueBox({
    valueBox(
      if_else(input$num_or_perc %% 2 == 0,
              format(steam_games_filtered() %>% summarize(sum(n_trailers)) %>% pull() / steam_games_filtered() %>% count() %>% pull(), digits=3),
              format(steam_games_filtered() %>% summarize(sum(n_trailers)) %>% pull() , big.mark = ',')),
      if_else(input$num_or_perc %% 2 == 0,
              'Trailers Per Game',
              'Trailers'),
      icon = icon("file-video"),
      color = "black"
    )
  })
  
  output$screenhots_trailers <- renderPlot({
    steam_games_filtered() %>% 
      group_by(release_year) %>% 
      summarize(secreenshots_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_screenshots)/n(), as.double(sum(n_screenshots))),
                trailers_per_game= if_else(input$num_or_perc %% 2 == 0, sum(n_trailers)/n(), as.double(sum(n_trailers)))
      ) %>% 
      ggplot(aes(x=release_year)) +
      geom_line(aes(y=secreenshots_per_game, group=1, color='Screenshots')) +
      geom_line(aes(y=trailers_per_game, group=1, color='Trailers')) +
      scale_color_manual(name = 'Legend', values = c('Screenshots' = '#F8766D', 'Trailers' = '#00BA38')) +
      ggtitle('Trailers and Screenshots by Year') +
      labs(x ='Release Year', y = if_else(input$num_or_perc %% 2 == 0,'Average Trailers/Screenshots per Game', 'Total Number of Trailers/Screenshots')) +
      theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0))
  })
  
  # ============ GENRES tab
  
  output$genres_network <- renderPlotly({
    #if (is.null(games_genres_filtered()) | is.null(genres_relations_filtered())) return()
    
    g <- graph_from_data_frame(genres_relations_filtered(), directed=FALSE, vertices=games_genres_grouped() )
    
    G <- upgrade_graph(g)
    
    if (input$select_graph == '1') {
      L <- layout_as_star(G)
    } else if (input$select_graph == '2') {
      L <- layout_as_tree(G)
    } else if (input$select_graph == '3') {
      L <- layout_in_circle(G)
    } else if (input$select_graph == '4') {
      L <- layout_nicely(G)
    } else if (input$select_graph == '5') {
      L <- layout_on_grid(G)
    } else if (input$select_graph == '6') {
      L <- layout_on_sphere(G)
    } else if (input$select_graph == '7') {
      L <- layout_randomly(G)
    } else if (input$select_graph == '8') {
      L <- layout_with_dh(G)
    }
    else if (input$select_graph == '9') {
      L <- layout_with_fr(G)
    }
    else if (input$select_graph == '10') {
      L <- layout_with_gem(G)
    }
    else if (input$select_graph == '11') {
      L <- layout_with_graphopt(G)
    }
    else if (input$select_graph == '12') {
      L <- layout_with_kk(G)
    }
    else if (input$select_graph == '13') {
      L <- layout_with_lgl(G)
    }
    else if (input$select_graph == '14') {
      L <- layout_with_mds(G)
    }
    
    vs <- V(G)
    es <- as.data.frame(get.edgelist(G))
    Nv <- length(vs)
    Ne <- length(es[1]$V1)
    
    Xn <- L[,1]
    Yn <- L[,2]
    
    unique_names <- unique(games_genres_grouped()$genres)
    names(Xn) <- unique_names
    names(Yn) <- unique_names
    
    network <- plot_ly(x = ~Xn, 
                       y = ~Yn, 
                       mode = 'markers', 
                       marker = list(size = 100* (games_genres_grouped() %>% pull(num_apps)) / (games_genres_grouped() %>% select(num_apps) %>% sum()),
                                     opacity = 0.7,color='#367fa9'),
                       text = paste(unique_names, '<br>Games count:', games_genres_grouped()$num_apps),
                       texttemplate = if_else(100* (games_genres_grouped() %>% pull(num_apps)) / (games_genres_grouped() %>% select(num_apps) %>% sum()) >10, unique_names, ''),
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
                    width = 30 * (genres_relations_filtered() %>% pull(num_connections))[i] / (genres_relations_filtered() %>% select('num_connections') %>% sum())
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
  
  output$connections_table <- DT::renderDataTable({
    genres_relations_filtered() %>% 
      group_by(from, to) %>% 
      summarize(total_connections=sum(num_connections)) %>% 
      ungroup() %>% 
      arrange(desc(total_connections)) %>% 
      left_join(games_genres_grouped(),
                by=c('from'='genres'),
                suffix=c('_from', '_to')) %>% 
      left_join(games_genres_grouped(),
                by=c('to'='genres'),
                suffix=c('_from', '_to')) %>% 
      mutate(possible_connections= paste0(format(100*total_connections / pmin(num_apps_from, num_apps_to), digits=1), '%')) %>% 
      select('Genre 1 Name'=from, 'Genre 2 Name'=to, 'Genre 1 Count'=num_apps_from, 'Genre 2 Count'=num_apps_to, 'Relations Count'=total_connections, '% of Possible Relations'=possible_connections)
  },  options = list(pageLength = 10))
  
  
  # ============= FUN STUFF tab
  observeEvent(input$random_five, {
    updateActionButton(session, 'random_five', label = sample(random_phrases, 1), icon= icon(sample(random_icons, 1)))
    
    output$fun_times <- renderTable({
      
      steam_games_df %>% 
        filter(release_year=='20XY' & 
                 !str_detect(release_date, '[wW][iI][sS][hH][lL][iI][sS][tT]') &
                 !str_detect(release_date, '\\d{4}') 
        ) %>% 
        select(name_app, release_date) %>% 
        count('Release Date' = release_date, name='Number of Occurences') %>% 
        slice_sample(n=5)
    })
  })
  
  # table showing games
  output$games_list <- DT::renderDataTable({
    steam_games_df %>% 
      select(ID, 'Game Name' = name_app, 'Release Date' = release_date)
  },  options = list(pageLength = 10))
  
})
