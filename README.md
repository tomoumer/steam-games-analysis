# Steam Games Analysis
An analysis of the games available on Steam, developed by VALVE based in Bellevue, Washington.

## Executive Summary
Video games are more popular than ever and one of the most successful platforms that contains them is Steam. Their servers process [large amounts of Terabytes of data](https://store.steampowered.com/stats/content/) and include tens of thousands of games from different publishers and teams - small and AAA alike - engaging millions of players across the globe. Valve is a private company, so there is lots of data that is not easily available or accessible. As an example, [Steam Database](https://steamdb.info) is constructed by consistently pulling data from Steam and storing it in their own database for later analysis. My initial idea was to scrape the webpage with the [currently 100 most played games](https://store.steampowered.com/charts/mostplayed), changing then to the 6000 or so present on SteamDB, but then that task turned to be a dead end.

I then pivoted to using the [Steam API](https://steamcommunity.com/dev) looking at publishers (and/or) developers, which countries they represent and the breakdown of games per country.

## Motivation
I often joke about Valve being the only company where I could put on my resume 5k hours of Dota 2 played as a valid skill. I am partial to them as opposed to newer alternatives, like the [Epic Games](https://www.epicgames.com/) or the various [Xbox](https://www.xbox.com/en-US/games?xr=shellnav&expId=EnableStoreHomePage-c) and [PlayStation](https://www.playstation.com/en-us/) stores, because Valve made [Half-Life](https://store.steampowered.com/franchise/Half-Life) (whose protagonist is a physicist, like me, and whose storytelling ability redefined gaming as a narrative medium). Furthermore, Gabe Newell, their CEO publicly decried Microsoft in the past for not supporting gaming as they should, signaling a significant shift towards Linux - they even made their own SteamOS based on it. And anyone who supports [Unix](https://en.wikipedia.org/wiki/Unix) operating systems has my vote!

## Data Question
What trends emerge overall and specifically, per developer (publisher) country? Looking at required age, price (free or not), platforms availability (win/mac/linux), average metacritic score, categories (single/multi player), genres, screenshots & trailers uploaded, number of recommends and number of achievements.

## Minimum Viable Product (MVP)
Shiny app displaying a simple count of the different variables. Further, showing the comparison (correlation) between average metacritic score and number of recommends. Displaying the relationship between number of games released in a certain genre and the number of recommends.
The app will allow to filter per publisher/developer, year of release, genre.
        
## Schedule (through 1/21/2023)
1. Get the Data (12/28/2022)
2. Clean & Explore the Data (1/3/2022)
3. Create Presentation and Shiny App (1/12/2022)
4. Internal Demos (1/17/2023)
5. Midcourse Project Presentations (1/23/2023)

## Data Sources
Using the [Steam APIs](https://steamcommunity.com/dev)
Wikipedia for the list of game [publishers](https://en.wikipedia.org/wiki/List_of_video_game_publishers) and [developers](https://en.wikipedia.org/wiki/List_of_video_game_developers)

## Known Issues and Challenges
Before I embarked on the current road, I was struggling with getting any information in regards to historical data for games. The documentation for the Steam APIs is questionable, just like Valve’s support for Dota+ (which costs $5/month). The APIs also don't consistently work well - there are games that clearly have data available on the [steam store](https://store.steampowered.com/), and yet the API doesn't return it. I’m also expecting that because some games have strange behaviors due to random updates and whatnot, I will need to be careful in the analysis and how to present the data.