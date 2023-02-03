# Steam Games Analysis
An analysis of the games available on Steam, developed by VALVE based in Bellevue, Washington.

## Executive Summary
Video games are more popular than ever and one of the most successful platforms that contains them is Steam. Their servers process [large amounts of Terabytes of data](https://store.steampowered.com/stats/content/) and include tens of thousands of games from different publishers and teams - independent developers and huge development studios alike - engaging millions of players across the globe (around 33 million as of January 2023).

Valve is a private company, so there is lots of data that is not easily available or accessible. As an example, [Steam Database](https://steamdb.info) is constructed by consistently pulling data from Steam and storing it in their own database for later analysis. My initial idea was to scrape the webpage with the [currently 100 most played games](https://store.steampowered.com/charts/mostplayed), or perhaps the 6000 or so present on SteamDB, but then that task turned to be a dead end.

I then pivoted to using the [Steam API](https://steamcommunity.com/dev) and exploring the data for interesting patterns.

The resulting Shiny App is available on [Shinyapps.io](https://tomoumer.shinyapps.io/steam_games_analysis/).

## Motivation
I often joke about Valve being the only company where I could put on my resume 5k hours of Dota 2 played as a valid skill. I am partial to Steam as opposed to newer alternatives, like the [Epic Games](https://www.epicgames.com/) or the various [Xbox](https://www.xbox.com/en-US/games?xr=shellnav&expId=EnableStoreHomePage-c) and [PlayStation](https://www.playstation.com/en-us/) stores, because Valve made [Half-Life](https://store.steampowered.com/franchise/Half-Life) (whose protagonist is a physicist, like me, and whose storytelling ability redefined gaming as a narrative medium).

Furthermore, Gabe Newell, their CEO publicly decried Microsoft in the past for not supporting gaming enough, despite being successfull because of games. Valve then signalled a significant shift towards Linux - they even made their own SteamOS based on it and various hardware to support it. And anyone who loves [Unix](https://en.wikipedia.org/wiki/Unix) operating systems has my vote!

## Data Question
Ultimately, I was curious to see what patterns I could uncover based on the data from Steam. I started with various questions such us: what is the actual amount of Independently Developed games on Steam? Has the availability on platforms other than Windows (MacOS / Linux) become better over the years? What trends might emerge looking ad game genres?

## Minimum Viable Product (MVP)
Shiny app showcasing interesting findings about the data, with filtering based on the release year.

## Data Sources
Using the [Steam APIs](https://steamcommunity.com/dev)

## Known Issues and Challenges
As stated previously, Valve is a private company so I wasn't sure if I would be able to uncover interesting trends. Right from the start, I was trying to obtain some historic data and realized that even with the help of some other websites, that would not be feasible.

Secondly, the documentation for the Steam APIs is questionable and their support nonexistent - I even emailed the team to confirm that - just like Valve’s support for Dota+ (which, unlike the APIs, costs $5/month). 

The APIs also don't consistently work well - there are games that clearly have data available on the [steam store](https://store.steampowered.com/), and yet the API doesn't return it.

Lastly, lots of successfull games get updated. My analysis was based on a snapshot in time that could change. I had to consider this when deciding what to focus my analysis on - for example, I didn't want to look at game prices as those are volatile and subject to change at each sale.