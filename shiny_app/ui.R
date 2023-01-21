

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  
  # skin
  skin = 'blue',
  
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
      id='tabs',
      menuItem('Navigation', tabName = 'navigation', icon = icon('map-location-dot')),
      menuItem('Overview',tabName = 'overview', icon = icon('database')),
      menuItem('Trends',tabName = 'trends', icon = icon('money-bill-trend-up')),
      menuItem('Genres',tabName = 'genres', icon = icon('dragon')),
      menuItem('Fun Stuff',tabName = 'fun_stuff', icon = icon('dice-d20')),
      conditionalPanel(
        condition = "input.tabs != 'navigation' && input.tabs != 'fun_stuff'",
        sliderInput('release_years_filter',
                    label = 'Release Years',
                    sep='',
                    min = 1997,
                    max = 2022,
                    value = c(2008, 2022),
                    ticks=FALSE,
                    pre='year '
        ),
        actionButton('show_hide_games', label = 'Show unreleased', icon=icon('eye')),
        conditionalPanel(
          condition = "input.tabs == 'trends'",
          actionButton('num_or_perc', label = 'Display Total Values', icon=icon('calculator'))
        ),
        conditionalPanel(
          condition = "input.tabs == 'genres'",
          selectInput('select_graph', label = 'Network Graph Layout', 
                      choices = list('Star' = 1,
                                     'Tree' = 2,
                                     'Circle' = 3,
                                     'Nicely' = 4,
                                     'Grid' = 5,
                                     'Sphere' = 6,
                                     'Random' = 7,
                                     'Davidson-Harel' = 8,
                                     'Fruchterman-Reingold' = 9,
                                     'GEM algorithm' = 10,
                                     'Graphopt' = 11,
                                     'Kamada-Kawai' = 12,
                                     'Large Graph' = 13,
                                     'Multidimensional' = 14), 
                      selected = 4),
        )
        
      )
    )
  ),
  
  # ========== Main body ============
  dashboardBody(
    
    # style sheet
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css')
    ),
    
    tabItems(
      tabItem(
        tabName= 'navigation',
        fluidRow(
          box(
            width=12,
            title='Welcome to Steam Games Analysis!',
            status='primary',
            solidHeader=TRUE,
            img(src='https://nashvillesoftwareschool.com/images/NSS-logo-horizontal-small.jpg', width = "432px", height = "36px"),
            p('A midcourse Data Science project'),
            span('by: Tomo Umer, MS', style='font-size:20px')
          )
        ),

        fluidRow(
          tabBox(
            #title = "First tabBox",
            id = "tabset1",
            width=12,
            tabPanel('Navigation Info',
                     p('A couple of', strong('important'), 'navigation notes:'),
                     p(strong('Header'), '(top of the page)'),
                     tags$ul(
                       tags$li('Menu (', icon('bars'), ') to show/hide sidebar.')
                     ),
                     p(strong('Sidebar'), '(left side)'),
                     tags$ul(
                       tags$li('Explore app by clicking on tabs (', icon('map-location-dot'), icon('database'),
                               icon('money-bill-trend-up'), icon('dragon'), icon('dice-d20'), ')'),
                       tags$li('Change the filters - they only appear while relevant tabs are selected')
                     ),
                     p(strong('Main Body'), '(you are reading it)'),
                     tags$ul(
                       tags$li('This page and Trends page have additional tabs within'),
                       tags$li('Graphs are interactive - zoom in/out, or show/hide various traces')
                     )
                     
            ),
            tabPanel('Motivation',
                     tags$blockquote("We don't stop playing because we grow old;", br(),
                                     "we grow old because we stop playing.",
                                     br(),
                                     tags$cite('- George Bernard Shaw')),
                     p('Reasons for choosing video games for my project:'),
                     tags$ul(
                       tags$li('They are fun, educational and an art form'),
                       tags$li('Games mimic real life and we can learn a lot from them')
                     ),
                     p('Why Steam in particular?'),
                     tags$ul(
                       tags$li('Developed by Valve for PC gaming with great Linux support'),
                       tags$li('Only real competition Epic Games Store since end of 2018')
                     ),
                     p('Initial plan:'),
                     tags$ul(
                       tags$li('Analyze popularity of games on Steam over time (Half-Life of games on Steam)'),
                       tags$li("With Michael Holloway's help got a Python scraping program done for a site that has that data"),
                       tags$li("... unfortunatelly too slow and no good way to select a good subset")
                     ),
                     p('Alright, time to pivot:'),
                     tags$ul(
                       tags$li('Using', a(href='https://steamcommunity.com/dev', 'Steam API'), '(no support from Steam)'),
                       tags$li('With R, some fixes due to API mistakes, about ~50 hrs later, got 152,194 apps (',
                               a(href='https://www.urbandictionary.com/define.php?term=ggez', 'GG EZ') ,')'),
                       tags$li('Data collection: 12/23/2022 to 12/26/2022')
                     )
            ),
            tabPanel('Expectations and Findings',
                     p("Just like with the streaming services, Valve isn't the only game distribution service anymore."),
                     p('Between the App Store, Google Play, Epic Games, Xbox, PlayStation and Nintendo stores, as well as
                       Epic Games, the space is getting quite saturated.'),
                     p('My predictions:'),
                     tags$ul(
                       tags$li('Prevalence of Indie (independent developer)'),
                       tags$li('An increase in Multi Player games in recent years'),
                       tags$li('Over the years, more games on Linux and MacOS'),
                       tags$li('Effect of COVID-19 (less games)')
                     ),
                     p('Reality:'),
                     tags$ul(
                       tags$li('The steam games are dominated by "Indie" genre, even more so than I thought'),
                       tags$li('Single Player games dominate the category list'),
                       tags$li('Complete domination of Windows'),
                       tags$li("most variables can't keep up with the increasing number of games, or, they level off")
                     )
            ),
            tabPanel('Valve pls',
                     img(src='https://i.redd.it/xeb73opjepw91.png', width='429px', height='339px'),
                     p(a(href = 'https://www.youtube.com/watch?v=GNVAmbB8hEo', 'G-Fat, or G-Money', icon('youtube'), title = 'Cave Johnson announcer sketch') ,
                       'aka GabeN, aka Gabe Logan Newell, co-funder and president of Valve.'),
                     p('A company that is like', a(href='https://developer.valvesoftware.com/wiki/Valve_Time', '80% memes and 20% products', title='Valve Time'), '.'),
                     p('The software and hardware they release is almost always groundbreaking:'),
                     tags$ul(
                       tags$li('Half-Life (1998) - unique storytelling in video games'),
                       tags$li('Counter-Strike (2000) - eSports'),
                       tags$li('Steam (2003) predated all (most?) other app stores'),
                       tags$li('Dota 2 (2010-ongoing) - The International 10 (2021) prize pool $40,018,195'),
                       tags$li('SteamOS (2013) - Linux based OS'),
                       tags$li('Source 2 (2014) - video game engine'),
                       tags$li('Steam Controller (2015), Valve Index (2019), Steam Deck (2022) - hardware'),
                       tags$li('Half-Life: Alyx (2020) - first ambitious VR game')
                     )
            ),
            tabPanel('Gamification',
                     p('Term that refers to turnig real world problems into games to solve.'),
                     p('Some examples of practical application of games:'),
                     tags$ul(
                       tags$li(a(href='https://en.wikipedia.org/wiki/List_of_Doom_ports', 'Porting of Doom to Windows'),
                               'in 1996 is what fused productivity and fun and helped Microsoft dominate the market.'),
                       tags$li('Thanks to a bug in World Of Warcraft in 2005,', a(href='https://www.washingtonpost.com/video-games/2020/04/09/world-warcraft-experienced-pandemic-2005-that-experience-may-help-coronavirus-researchers/',
                                                                                  'researches studied spread of viruses (yep, including COVID-19)')),
                       tags$li(a(href='https://sites.google.com/view/vanderbilt-accre-analysis/home', 'ACCRE GPU cluster'),
                               '(and others) are possible due to gaming driving down GPU prices'),
                       tags$li('OpenAI that developed the', a(href='https://openai.com/blog/chatgpt/', 'impressive DALL-E and chatGPT'),
                               'was first trained and perfected by competing against people in', a(href='https://openai.com/five/', 'Dota 2 (Valve game)')),
                       tags$li('Researchers use help from gamers via', a(href='https://fold.it', 'foldit'), 'who are finding innovative ways
                               to fold proteins and help speed up research'),
                       tags$li('Games and simulations used to train both people and AI - there are plenty of examples,
                               and it it easy to imagine  byjust looking at tech demos for', a(href='https://www.unrealengine.com/en-US', 'Unreal Engine 5'))
                     )
            )
          )
        )
      ),
      
      
      # ===== The overview (general) tab ======
      tabItem(tabName = 'overview',
              fluidRow(
                box(
                  width=12,
                  title='Steam Games Highlights',
                  status='primary',
                  solidHeader=TRUE,
                  p('Some numbers from the dataset with currently selected filters:'),
                  fluidRow(
                    valueBoxOutput('num_games'),
                    valueBoxOutput('num_publishers'),
                    valueBoxOutput('num_developers')
                  )
                )
              ),
              plotlyOutput('games_by_year')
      ),
      
      # ===== Trends tab ======
      tabItem(tabName = 'trends',
              fluidRow(
                box(
                  width=12,
                  title='Trends Over Time',
                  status='primary',
                  solidHeader=TRUE,
                  p('Graphs displaying trends of different variables.'),
                  conditionalPanel(
                    condition = "input.tabset2 == 'Genres'",
                    fluidRow(
                      infoBoxOutput('n1_genre'),
                      infoBoxOutput('n2_genre'),
                      infoBoxOutput('n3_genre')
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tabset2 == 'Categories'",
                    fluidRow(
                      infoBoxOutput('n1_category'),
                      infoBoxOutput('n2_category'),
                      infoBoxOutput('n3_category')
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tabset2 == 'OS'",
                    fluidRow(
                      infoBoxOutput('n_win'),
                      infoBoxOutput('n_mac'),
                      infoBoxOutput('n_linux')
                    )
                  ),
                  conditionalPanel(
                  condition = "input.tabset2 == 'Ratings'",
                  fluidRow(
                    infoBoxOutput('n_metacritic'),
                    infoBoxOutput('n_recommended')
                  )
                ),
                  conditionalPanel(
                    condition = "input.tabset2 == 'Other'",
                  fluidRow(
                    infoBoxOutput('n_free'),
                    infoBoxOutput('n_dlc'),
                    infoBoxOutput('n_achievements')
                  )
                  ),
                conditionalPanel(
                  condition = "input.tabset2 == 'Trailers and Screenshots'",
                  fluidRow(
                    infoBoxOutput('nr_screenshots'),
                    infoBoxOutput('nr_trailers')
                  )
                )
                )
              ),
              fluidRow(
              tabBox(
                id = "tabset2",
                width=12,
                tabPanel('Genres', plotlyOutput('top_genres')),
                tabPanel('Categories', plotlyOutput('top_categories')),
                tabPanel('OS', plotlyOutput('platforms_percentage')),
                tabPanel('Ratings', plotlyOutput('ratings')),
                tabPanel('Other', plotlyOutput('other_stats')),
                tabPanel('Trailers and Screenshots', plotlyOutput('screenhots_trailers'))
              )
              )
              
              
              
      ),
      
      # ======= genres tab ========
      tabItem(tabName = 'genres',
              fluidRow(
                box(
                  width=12,
                  title='Video Game Genres Relations',
                  status='primary',
                  solidHeader=TRUE,
                  p('Showcasing how frequently the genres are paired together.')
                )
              ),
              fluidRow(
              tabBox(
                id = "tabset3",
                width=12,
              tabPanel('Relations Network Graph', plotlyOutput('genres_network')),
              tabPanel('Relations Table',
                  p('The percentage measures how many relations two genres have out of theoretically possible ones.'),
                  p('It is evaluated by taking the total number of relations and dividing by the lesser of the two pairs it is connecting.'),
                  DT::dataTableOutput('connections_table')
                )
              )
              )
      ),
      
      # ====== fun stuff tab ======
      tabItem(tabName = 'fun_stuff',
              fluidRow(
                box(
                  width=12,
                  title='Fun Release Dates',
                  status='primary',
                  solidHeader=TRUE,
                  p('In the dataset there are 8,932 games with no explicit year of release. I removed the "wishlisht" ones and chose all distinct ones.'),
                  p('Try seeing 5 of the random release dates by clicking the button below.')
                )
              ),
              box(
                id='random_box',
                width= 12,
                background = 'black',
                span(tableOutput('fun_times'), style='color:white'), 
                p(''),
                actionButton('random_five', label = 'Show me five random games!', icon= icon('dice')),
                p(''),
                span(em('TIP: if interested in which game(s) the release date corresponds to, search the table below'), style='color:white')
              ),
              fluidRow(
              box(
                width=12,
                title='Steam Games Table',
                status='primary',
                solidHeader=TRUE,
                DT::dataTableOutput('games_list')
              )
              )
              
      )
      
    ))
))
