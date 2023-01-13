

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  # skin
  skin = 'purple',
  
  # Application title
  dashboardHeader(title='Steam Games Analysis',
                  tags$li(a(href = 'https://store.steampowered.com',
                            icon('steam'),
                            title = 'To Steam Webpage'),
                          class = 'dropdown'),
                  tags$li(a(href = 'https://github.com/tomoumer/steam-games-analysis',
                            icon('github'),
                            title= 'To My Github'),
                          class = 'dropdown')
                  
  ),
  
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('Navigation', tabName = 'navigation', icon = icon('map-location-dot'), badgeLabel = "start here", badgeColor = "green"),
      menuItem('Overview',tabName = 'overview', icon = icon('database')),
      menuItem('Platforms',tabName = 'platforms', icon = icon('linux')),
      menuItem('Genres',tabName = 'genres', icon = icon('dice-d20'))
    )
  ),
  
  # Main body
  dashboardBody(
    tabItems(
      tabItem(
        tabName= 'navigation',
        h2('Analysis of Steam Games'),
        p('To navigate the site use the tabs on the left side.'),
        p('The sidebar pannel can also be hidden by pressing', icon('bars'), 'on top of the page.'),
        p('In the top right corner, you can access the Steam store', icon('steam'), 'my github where this code is hosted', icon('github'))
      ),
      
      
      # The overview (general) tab
      tabItem(tabName = 'overview',
              h2('Steam Games Releases'),
              tabBox(
                #title = "Steam Games Releases",
                width = 12,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                #height = "250px",
                tabPanel('With Release Year',
                         plotlyOutput('games_by_year')
                ),
                
                tabPanel('No Release Year',
                         p('In the dataset there are 8,932 games with no explicit year of release. I removed the "wishlisht" ones and chose all distinct ones.'),
                         p('Below is a random selection of 5 such release dates:'),
                         box(
                           width= 12,
                           background = 'light-blue',
                           #align='center',
                           span(tableOutput('fun_times'), style='color:white'), 
                           p(''),
                           actionButton('random_five', label = 'Show me five random games!', icon= icon('dice')),
                         ),
                         p('TIP: if interested in which game(s) the release date corresponds to, search the table below'),
                )
              ),
              h2('Steam Games Table'),
              dataTableOutput('games_list')
      ),
      
      #
      tabItem(tabName = 'platforms',
              h2('Availability on Platforms'),
              fluidRow(
                plotlyOutput('platforms_availability')
              ),
              fluidRow(
                plotlyOutput('platforms_percentage')
              )
      
      #fluidRow(
      #  h2('More Fitting'),
      #  column(
      #    width=6,
      #    h4('Test 3')
      #    dataTableOutput('pubs_by_year')
      #  ),
      #  column(
      #    width=6,
      #    h4('Test 4')
      #    dataTableOutput('devs_by_year')
      #  )
      #)
      
    ),
    
    tabItem(tabName = 'genres',
            h2('Video Game Genres'),
            sliderInput('release_years',
                        label = h3('Years Selection'),
                        min = steam_games_df %>%
                          mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
                          filter(!is.na(release_year) & release_year <= 2023 & release_year >= 1997) %>% 
                          pull(release_year) %>% 
                          min() %>% 
                          as.numeric(),
                        max = steam_games_df %>% 
                          mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
                          filter(!is.na(release_year) & release_year <= 2023 & release_year >= 1997)  %>% 
                          pull(release_year) %>% 
                          max() %>% 
                          as.numeric(),
                        value = c(1997, 2023)
            ),
            actionButton('draw_network', label = "Draw Network"),
            plotlyOutput('genres_network')
    )
    
    
  ))
))
