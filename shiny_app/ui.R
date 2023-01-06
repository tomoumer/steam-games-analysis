

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  # Application title
  dashboardHeader(title='Steam Games Analysis'),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      #menuItem('Dasher', tabName = 'dasher', icon = icon('people-group'), badgeLabel = "team", badgeColor = "green"),
      menuItem('Overview',tabName = 'overview', icon = icon('database'), badgeLabel = "general", badgeColor = "green"),
      menuItem('Test',tabName = 'test')
    )
  ),
  
  # Main body
  dashboardBody(
    tabItems(
      #      tabItem(
      #        tabName= 'dasher',
      #        fluidRow(
      #          h2('Team Dasher'),
      #          p('Meet the team:'),
      #          tags$ul(
      #            tags$li('Tomo Umer')
      #          )
      #        )
      #      ),
      
      
      #              sliderInput('age_range',
      #                          label = h3('house age range'),
      #                          min = steam_games_df %>%
      #                            mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      #                            filter(!is.na(release_year) & release_year < 2025 & release_year > 2005) %>% 
      #                            pull(release_date) %>% 
      #                            min(),
      #                          max = steam_games_df %>% 
      #                            mutate(release_year=str_extract(release_date, '\\d{4}')) %>% 
      #                            filter(!is.na(release_year) & release_year < 2025 & release_year > 2005) %>% 
      #                            pull(release_date) %>% 
      #                            max(),
      #                          value = c(10, 70)
      #              ),
      
      tabItem(tabName = 'overview',
              h2('Steam Games by Year'),
              plotlyOutput('games_by_year'),
              h2('Steam Games without Relese Year'),
              box(
                title = 'Random showcase',
                width = 12,
                background = 'light-blue',
                p('In the dataset there are 8,932 games with no explicit year of release. I removed some of the booring ones such as "TBD" or "coming soon", only leaving distinct entries.'),
                p('Below is a random selection of 5 such release dates:'),
                box(
                  width= 12,
                  #align='center',
                  span(tableOutput('fun_times'), style='color:black'), 
                  p(''),
                  actionButton("action", label = "that was fun, give me another five"),
                ),
                p('TIP: if interested in which game(s) the release date corresponds to, search the table below'),
              ),
              h2('Steam Games Table'),
              dataTableOutput('games_list')
      ),
      
      tabItem(tabName = 'test',
              h2('Test'),
              fluidRow(
                h2('Fitting'),
                column(
                  width=6,
                  h4('Test 1')
                ),
                column(
                  width=6,
                  h4('Test 2')
                )
              ),
              
              fluidRow(
                h2('More Fitting'),
                column(
                  width=6,
                  h4('Test 3')
                ),
                column(
                  width=6,
                  h4('Test 4')
                )
              )
              
      )
      
      
    ))
))
