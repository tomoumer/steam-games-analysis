

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
      menuItem('Trends',tabName = 'trends', icon = icon('money-bill-trend-up')),
      menuItem('Genres',tabName = 'genres', icon = icon('dragon')),
      menuItem('Fun Stuff',tabName = 'fun_stuff', icon = icon('dice-d20'))
    )
  ),
  
  # Main body
  dashboardBody(
    
    box(
      title = 'Filters',
      width=12,
      background='purple',
      collapsible = TRUE,
      sliderInput('release_years_filter',
                  label = 'Select Games Release Years',
                  sep='',
                  min = steam_games_df %>%
                    filter(!is.na(release_year) & release_year <= 2023 & release_year >= 1997) %>% 
                    pull(release_year) %>% 
                    min() %>% 
                    as.numeric(),
                  max = steam_games_df %>% 
                    filter(!is.na(release_year) & release_year <= 2023 & release_year >= 1997)  %>% 
                    pull(release_year) %>% 
                    max() %>% 
                    as.numeric(),
                  value = c(2008, 2022),
                  ticks=FALSE,
                  pre='year '
                  #animate=TRUE
      )
    ),

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
              fluidRow(
                box(
                  title = h2('Steam Games Highlights'), width=12,
                  status = 'primary', solidHeader = FALSE, collapsible = FALSE,
                  actionButton('show_hide_games', label = 'Hide unreleased games', icon=icon('eye-slash'))
                  #checkboxInput('check_released', label = 'Hide unreleased games?', value = FALSE),
                )
              ),
              fluidRow(
                valueBoxOutput('num_games'),
                valueBoxOutput('num_publishers'),
                valueBoxOutput('num_developers')
              ),
              fluidRow(
                infoBoxOutput('perc_free'),
                infoBoxOutput('perc_dlc'),
                infoBoxOutput('perc_achievements')
              ),
              plotlyOutput('games_by_year')
      ),
      
      # trends
      tabItem(tabName = 'trends',
              fluidRow(
                box(
                  title = h2('Trends over Time'), width=12,
                  status = 'primary', solidHeader = FALSE, collapsible = FALSE
                )
              ),
              tabBox(
                #title = "First tabBox",
                id = "tabset1",
                width=12,
                tabPanel('Genres', plotlyOutput('top5_genres')),
                tabPanel('OS', plotlyOutput('platforms_percentage')),
                tabPanel('Other', plotlyOutput('other_stats'))
              )
              

              
      ),
      
      tabItem(tabName = 'genres',
              fluidRow(
                box(
                  title = h2('Video Game Genres'), width=12,
                  status = 'primary', solidHeader = FALSE, collapsible = FALSE,
                  actionButton('draw_network', label = "Draw Network"),
                )
              ),
              
              plotlyOutput('genres_network')
      ),
      
      tabItem(tabName = 'fun_stuff',
              h2('Fun Release Dates'),
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
              h2('Steam Games Table'),
              dataTableOutput('games_list')
      )
      
    ))
))
