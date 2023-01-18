

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
      id='tabs',
      menuItem('Navigation', tabName = 'navigation', icon = icon('map-location-dot'), badgeLabel = "start here", badgeColor = "green"),
      menuItem('Overview',tabName = 'overview', icon = icon('database')),
      menuItem('Trends',tabName = 'trends', icon = icon('money-bill-trend-up')),
      menuItem('Genres',tabName = 'genres', icon = icon('dragon')),
      menuItem('Fun Stuff',tabName = 'fun_stuff', icon = icon('dice-d20')),
      conditionalPanel(
        condition = "input.tabs != 'navigation' && input.tabs != 'fun_stuff'",
        actionButton('show_hide_games', label = 'Show unreleased', icon=icon('eye')),
        sliderInput('release_years_filter',
                    label = 'Release Years',
                    sep='',
                    min = 1997,
                    max = 2022,
                    value = c(2008, 2022),
                    ticks=FALSE,
                    pre='year '
        ),
        conditionalPanel(
          condition = "input.tabs == 'trends'",
          actionButton('num_or_perc', label = 'Show Values', icon=icon('calculator'))
          )
        
      )
    )
  ),
  
  # ========== Main body ============
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName= 'navigation',
        fluidRow(
          box(
            width=12,
            background = 'purple',
            h2('Welcome to my Steam Game Analysis!'),
            p('A midcourse Data Science project'),
            span('Tomo Umer, MS', style='font-size:20px')
          )
        ),
        #fluidRow(
        #  tags$img(src='https://nashvillesoftwareschool.com/images/NSS-logo-horizontal-small.jpg', width = "100px", height = "20px")
        #),
        fluidRow(
          tabBox(
            #title = "First tabBox",
            id = "tabset1",
            width=12,
            tabPanel('Navigation Info',
                     p('To navigate the shiny app you can click or tap on various elements.'),
                     p('The header on top of the page:'),
                     tags$ul(
                       tags$li('Menu (', icon('bars'), ') to show/hide sidebar.'),
                       tags$li('Link to the Steam Store (', icon('steam'),')'),
                       tags$li('Link to my Github (', icon('github'), ')')
                     ),
                     p('The sidebar contains tabs and main filters:'),
                     tags$ul(
                       tags$li('Navigation (', icon('map-location-dot'), ') - current page'),
                       tags$li('Overview (', icon('database'), ') - general overview of the data'),
                       tags$li('Trends (', icon('money-bill-trend-up'), ') - some trends over (release) years'),
                       tags$li('Genres (', icon('dragon'), ') - network graph genre connections'),
                       tags$li('Fun Stuff (', icon('dice-d20'), ') - random fun stuff!'),
                       tags$li('First filter shows or hides unreleased games (2023 and beyond)'),
                       tags$li('Second filter is on the years games were released')
                     ),
                     p('The bulk of the content is where this writing is:'),
                     tags$ul(
                       tags$li('This page and Trends have additional tabs within'),
                       tags$li('Trends and Fun Stuff have more filters/buttons'),
                       tags$li('All graphs can be interacted with')
                     )
            ),
            tabPanel('Background',
                     tags$blockquote("We don't stop playing because we grow old; we grow old because we stop playing.",
                                     br(),
                                     tags$cite('- George Bernard Shaw')),
                     p('Video games are a natural extension of the games people have played for thousands of years.
                       We often think of games as just entertainment or art (depending on who you ask),
                       but here are some examples of practical application of games:'),
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
            ),
            tabPanel('Why Steam?',
                     img(src='https://i.redd.it/xeb73opjepw91.png', width='429px', height='339px'),
                     p(a(href = 'https://www.youtube.com/watch?v=GNVAmbB8hEo', 'G-Fat, or G-Money', icon('youtube'), title = 'Cave Johnson announcer sketch') ,
                       'aka GabeN, but actually, Gabe Logan Newell, co-funder and president of Valve. A company that
                       is like', a(href='https://developer.valvesoftware.com/wiki/Valve_Time', '80% memes and 20% products', title='Valve Time'), '.'),
                     p('But seriously, the software and hardware they release is almost always groundbreaking. Some highlights:'),
                     tags$ul(
                       tags$li('Half-Life (1998) showed the unique capabilities of storytelling in video games'),
                       tags$li('Counter-Strike (2000) was instrumental in popularizing eSports'),
                       tags$li('Steam (2003) was the first of its kind, a "digital software distribution channel"'),
                       tags$li('Source (2004) game engine, other hit games, Half-Life 2, Portal, etc..'),
                       tags$li('Dota 2 (2013), biggest eSports prize pool: The International 10 (2021) - $40,018,195'),
                       tags$li('SteamOS (2013) a linux-based OS, helping develop linux based apps!'),
                       tags$li('Source 2 (2014) making it easier to make games'),
                       tags$li('Steam Controller (2015), Valve Index (2019), Steam Deck (2022)'),
                       tags$li('Half-Life: Alyx (2020) first full-fledged big VR video game')
                     )
            ),
            tabPanel('Methodology',
                     p('Initial plan / first draft:'),
                     tags$ul(
                       tags$li('Analyze popularity of games on steam over time'),
                       tags$li('Checking steam APIs and store -> no such info available!'),
                       tags$li('Plan b: try to scrape the info from another website'),
                       tags$li("With Michael Holloway's help got it working... slowly."),
                       tags$li("Only to realize I didn't have any good measure on how to pick the games"),
                       tags$li("I won't upload the Python file out of respect for the website")
                     ),
                     p('Alright, time to pivot:'),
                     tags$ul(
                       tags$li('Find out if I can get enough interesting variables from Steam API'),
                       tags$li('Write R code to get as many API calls as the system allows'),
                       tags$li('~50 hrs later (I let it run over nights), and some fixes due to API mistakes, got 152,194 apps'),
                       tags$li('Focussed on just the games, 80,625'),
                       tags$li('Come to find out certain variables like metacritic or recommended, are lackluster'),
                       tags$li('Explored the data and found other interesting things (see Key Findings tab)!')
                     )
            ),
            tabPanel('Key Findings',
                     p('Overall quite a few interesting details and quirks of the data.'),
                     p('In particular, I want to highlight:'),
                     tags$ul(
                       tags$li('The steam games are dominated by "Indie" (independent developer) genre'),
                       tags$li('Single Player games dominate the category list'),
                       tags$li('Complete domination of Windows'),
                       tags$li("most variables can't keep up with the increasing number of games, or, they level off")
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
                  background = 'purple',
                  h2('Steam Games Highlights')
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
      
      # ===== Trends tab ======
      tabItem(tabName = 'trends',
              fluidRow(
                box(
                  width=12,
                  background = 'purple',
                  h2('Trends over Time')
                )
              ),
              tabBox(
                #title = "First tabBox",
                id = "tabset1",
                width=12,
                tabPanel('Genres', plotlyOutput('top_genres')),
                tabPanel('Categories', plotlyOutput('top_categories')),
                tabPanel('OS', plotlyOutput('platforms_percentage')),
                tabPanel('Other', plotlyOutput('other_stats')),
                tabPanel('Stats Per Game', plotlyOutput('screenhots_trailers'))
              )
              
              
              
      ),
      
      # ======= genres tab ========
      tabItem(tabName = 'genres',
              fluidRow(
                box(
                  width=12,
                  background = 'purple',
                  h2('Video Game Genres')
                )
              ),
              
              plotlyOutput('genres_network')
      ),
      
      # ====== fun stuff tab ======
      tabItem(tabName = 'fun_stuff',
              fluidRow(
                box(
                  width=12,
                  background = 'purple',
                  h2('Fun Release Dates')
                )
              ),
              p('In the dataset there are 8,932 games with no explicit year of release. I removed the "wishlisht" ones and chose all distinct ones.'),
              p('Try it out, by clicking the button below:'),
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
