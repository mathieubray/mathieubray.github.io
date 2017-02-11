---
title: "Hello World!"
author: "Mathieu Bray"
date: "2016-12-23"
tags: [R, hockey]
layout: post
---
*This post was updated on 2017-02-07*


I plan on using this space to post my musings on statistics that, shall we say, fall outside of the hallowed halls of academia. For the most part, this will mean lots of R, and lots of hockey and Twitter data. I hope to share something interesting every few weeks, though with my ultra hectic (wink) grad student life, we'll see how long I can keep that promise...

For now here's a few snippets of code to show I can make my way around R. 

### A Quick Example - Working with Hockey Data

Say we were interested in figuring out how many goals were scored against the [Montreal Canadiens](https://www.nhl.com/canadiens) in a given season. You see, the thought in Montreal is that players from the province of Quebec seem to elevate their play when facing their hometown team. Here I'll demonstrate some of the data wrangling needed to investigate this particular problem (though perhaps there is more to this question to think about for the future...)

Let's start by loading a few handy libraries.



```r
library(XML) # Web scraping
library(dplyr) # Lifeblood of data management
library(lubridate) # For fixing dates
```

First we require a list of goals scored against the Canadiens. This can be obtained using the `nhlscrapr` package (available from [CRAN](https://cran.r-project.org/web/packages/nhlscrapr/index.html)). In the interest of time, I will focus here on the 2014-2015 NHL season. Why 2014-2015? Because I really want to get a decent test page online? (Really, it's the most recent season for which near-complete data is available, last I checked anyway, and where the reference dates work properly.)

The loaded data is split across three individual datasets. We will make use of `nhlscrapr.20142015.roster` and `nhlscrapr.20142015.pbp`. The play-by-play dataset has as each row every individual event logged by the NHL in their official game sheets for that season, such as goals, shots, penalties, and the like, along with information about each event, such as the player directly involved, his team, the home and away teams, period, etc. The roster dataset has for each row the name and some identifying information (though not the birthplace!) for each player who played at least one game in the given season. 

For the roster, we require only the first and last name of the player, along with his identifying code in the play-by-play data. For the play-by-play, we first filter out all non-goal events and all shootout goals (which is marked as occurring in period 5). We then merge in the player name based on the identifying code. The game date is obtained as the number of days elapsed since January 1, 2002. We also extract the team that was scored on, and, because it might be interesting, whether or not the game occurred in Montreal. Finally, we retain only the goals where Montreal ("MTL") is specified as the opposing team.


```r
load("data/nhlscrapr20142015.RData")

# Get player names and identifying codes 
roster <- nhlscrapr.20142015.roster %>% 
  select(firstlast,index) %>%
  rename(Player = firstlast,PlayerCode=index)

# Retrieve all goals scored against Montreal
goals <- nhlscrapr.20142015.pbp %>% 
  filter(etype=="GOAL", period <= 4) %>% # Consider only non-shootout goals
  select(ev.team,ev.player.1,awayteam,hometeam,refdate) %>%
  rename(Team=ev.team,PlayerCode=ev.player.1,Away=awayteam,Home=hometeam,Date=refdate) %>%
  inner_join(roster,by="PlayerCode") %>% # Merge in player names
  mutate(Date = ymd("2002-01-01") + days(Date)) %>% # Get correct date
  select(-PlayerCode) %>%
  rowwise() %>%
  mutate(Opponent = ifelse(Team==Away,Home,Away), # Get opponent of team that scored
         InMTL = Home=="MTL") %>% # Did the game occur in Montreal? T/F
  select(-Home,-Away) %>%
  filter(Opponent == "MTL") %>% # Consider only goals against Montreal
  ungroup() %>%
  arrange(Date)

head(goals)
```

```
## # A tibble: 6 ? 5
##    Team       Date           Player Opponent InMTL
##   <chr>     <dttm>            <chr>    <chr> <lgl>
## 1   TOR 2014-10-08      NAZEM KADRI      MTL FALSE
## 2   TOR 2014-10-08      TYLER BOZAK      MTL FALSE
## 3   TOR 2014-10-08    MORGAN RIELLY      MTL FALSE
## 4   WSH 2014-10-09 ANDRE BURAKOVSKY      MTL FALSE
## 5   PHI 2014-10-11    MICHAEL RAFFL      MTL FALSE
## 6   PHI 2014-10-11   WAYNE SIMMONDS      MTL FALSE
```

Some of you may have noticed I use capital letters for my column names. Hopefully my blatant disregard for standard convention doesn't offend anyone too much (or my frequent use of ellipses for that matter...)

To get the player birthplaces, I will cross-reference the above information with a list of birthplaces for NHL players. Conveniently, [Hockey-Reference](http://www.hockey-reference.com/) has tables of players birthplaces divided by region (listed under the ["frivolities"](http://www.hockey-reference.com/friv/) section). The table for players born in Quebec can be found [here](http://www.hockey-reference.com/friv/birthplaces.cgi?country=CA&province=QC&state=). As a straightforward HTML page, this information can be easily retrieved using the `readHTMLTable` function from the XML package.



```r
url <- c("http://www.hockey-reference.com/friv/birthplaces.cgi?country=CA&province=QC&state=")

# Only 1 Table in list, column 12 repeats GP for goaltenders, remove it as the column name is the same as the previous and causes issues
quebec.players <- readHTMLTable(url,stringsAsFactors=F)[[1]][,-12] %>% 
  filter(Player != "Player") # Remove rows not corresponding to any player

names(quebec.players) <- c("Rk","Player","Start.Year","End.Date",
                           "Position","GP","G","A","PTS","Plus-Minus",
                           "PIM","W","L","TO","SV","GAA","Birthplace","Birth.Date","Death.Date")
```



Since we're only interested in goal scorers, we can remove goalies, and since we're only observing the 2014-2015 season here, we can exclude all players who retired prior to 2014. Luckily, here, there is no need for much string editing; a simple conversion of the player names in this dataset to upper case will match up names with columns from the `nhlscrapr` data.


```r
relevant.quebec.players <- quebec.players %>% 
  mutate(End.Date = as.numeric(End.Date)) %>%
  filter(End.Date >= 2014,
         Position != "G") %>% # Consider only shooters who played up to to 2014-2015 season
  mutate(Player = toupper(Player)) %>%
  select(Player,Birthplace)

head(relevant.quebec.players)
```

```
##                Player       Birthplace
## 1       MARK BARBERIO         Montreal
## 2 FRANCOIS BEAUCHEMIN            Sorel
## 3 ANTHONY BEAUVILLIER      Sorel-Tracy
## 4    PATRICE BERGERON Ancienne-Lorette
## 5       STEVE BERNIER      Quebec City
## 6          ALEX BIEGA         Montreal
```

Now it's a simple matter of merging the two tables. The inner join will make it such that only values appearing in both tables will be included in the final table, thus stripping our table down to only those Quebecois players with goals against the Habs that year.


```r
quebec.against.mtl <- inner_join(goals,relevant.quebec.players,by="Player") %>%
  select(Team,Date,Player,InMTL,Birthplace)

quebec.against.mtl
```

```
## # A tibble: 10 ? 5
##     Team       Date              Player InMTL  Birthplace
##    <chr>     <dttm>               <chr> <lgl>       <chr>
## 1    BOS 2014-10-16         SIMON GAGNE  TRUE    Ste. Foy
## 2    COL 2014-10-18        ALEX TANGUAY  TRUE Ste-Justine
## 3    MIN 2014-11-08    JASON POMINVILLE  TRUE  Repentigny
## 4    NYR 2014-11-23    MARTIN ST. LOUIS FALSE       Laval
## 5    COL 2014-12-01       DANIEL BRIERE FALSE    Gatineau
## 6    MIN 2014-12-03    JASON POMINVILLE FALSE  Repentigny
## 7    PIT 2015-01-03        DAVID PERRON FALSE  Sherbrooke
## 8    NSH 2015-01-20        MIKE RIBEIRO  TRUE    Montreal
## 9    ANA 2015-03-04 FRANCOIS BEAUCHEMIN FALSE       Sorel
## 10   T.B 2015-03-30     JONATHAN DROUIN  TRUE  Ste-Agathe
```

This was all pretty easy. The final tally appears to be 10 goals by Quebecers, with 5 occurring in Montreal. This seems... well, quite low! I'll note that this doesn't take into account francophone players not born in the province of Quebec ([Claude Giroux](https://www.nhl.com/player/claude-giroux-8473512), for example, is from Ontario). 

So is this what we should expect from Quebec born players in games against Montreal? Or do francophone players play better in their hometown? Admittedly, this is a very surface-level impression of the problem. Let's think about this a little more... next time ;)


