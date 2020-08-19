library(Lahman)
library(dplyr)
library(lubridate)
library(tidyr)

#select desired player info from Master table of Lahman data
players <- subset(Master, select=c(playerID, birthYear, birthCountry,
                                          birthState, birthCity, deathYear,
                                          nameFirst, nameLast, nameGiven,
                                          weight, height, bats, throws,
                                          debut, finalGame, deathDate,
                                          birthDate))

#add column with count of AllStar game appearances by player
allstarcount <- as.data.frame(table(AllstarFull$playerID))
allstarcount = setNames(allstarcount,c('playerID','AllstarAppear'))
players <- merge(players, allstarcount, by="playerID", all = T)

#create logical column to indicate if player ever appeared
#in allstar game
players$Allstar = players$AllstarAppear
players$Allstar  <- as.logical(players$Allstar)

#replace NAs in new columns
players[["AllstarAppear"]][is.na(players[["AllstarAppear"]])] <- 0
players[["Allstar"]][is.na(players[["Allstar"]])] <- FALSE

#add column with number of player Awards received in career
awardscount <- as.data.frame(table(AwardsPlayers$playerID))
awardscount = setNames(awardscount,c('playerID','AwardsReceived'))
players <- merge(players, awardscount, by="playerID", all = T)

#create logical column to indicate if player ever received
#an award
players$Awarded = players$AwardsReceived
players$Awarded  <- as.logical(players$Awarded)

#replace NAs in new columns
players[["AwardsReceived"]][is.na(players[["AwardsReceived"]])] <- 0
players[["Awarded"]][is.na(players[["Awarded"]])] <- FALSE

#add column with number of years of college play
collegecount <- as.data.frame(table(CollegePlaying$playerID))
collegecount = setNames(collegecount,c('playerID','collegeyears'))
players <- merge(players, collegecount, by="playerID", all = T)

#create logical column to indicate if player attended college
players$collegeattend = players$collegeyears
players$collegeattend  <- as.logical(players$collegeattend)

#replace NAs in CollegePlay with 0 and F
players[["collegeyears"]][is.na(players[["collegeyears"]])] <- 0
players[["collegeattend"]][is.na(players[["collegeattend"]])] <- FALSE

#manage NAs in players data
NAcount <- players %>% summarise_all(funs(sum(is.na(.))))
View(NAcount)

#remove records with NA in specified fields
players <- players %>%
  drop_na(weight) %>%
  drop_na(height) %>%
  drop_na(debut) %>%
  drop_na(birthYear) %>%
  drop_na(birthCity) %>%
  drop_na(birthCountry)


#colum debut & finalGame should be Date format - converted here
players$debut <- as.Date(players$debut)
players$finalGame <- as.Date(players$finalGame)

#insert missing birth dates with first day of birth year
#players[["birthDate"]][is.na(players[["birthDate"]])] <- as.Date(ISOdate(players$birthYear, 1, 1))

#remove any duplicates
#players <- distinct(players)

#add new values

#add time period groupings - these are timeframes established by MLB
#hall of fame in 2016 and corresponds with their 4 committees 
players$timeperiod[players$debut<= '1949-12-31'] <- "Early Baseball (1871 to 1949)"
players$timeperiod[players$debut>'1950-01-01' & players$debut<= '1969-12-31'] <- "Golden Days (1950 to 1969)"
players$timeperiod[players$debut>'1970-01-01' & players$debut<= '1987-12-31'] <- "Modern Baseball (1970 to 1987)"
players$timeperiod[players$debut>'1988-01-01'] <- "Today's Game (1988 and later)"

#add age of player for the specific year & length of career
players$debutAge <- year(players$debut) - players$birthYear
players$lengthcareer <- players$finalGame - players$debut
players$lengthcareer <- as.numeric(players$lengthcareer)
players$lengthcareer <- players$lengthcareer/365

#add height in feet
players$heightft <- players$height/12

#cleanup environment
rm(allstarcount, awardscount, collegecount, NAcount)

#review dataframe
str(players)
View(players)


#-----------------------------------------------------------

#Lahman package doesnt contain batting statistics (offense)
#but provides a function that calculates key stats in a dataframe
battingcalcdata <- battingStats(data = Lahman::Batting,
                            idvars = c("playerID", "yearID",
                                       "stint", "teamID", 
                                       "lgID"), cbind = TRUE)
battingRelevant <- subset(battingcalcdata, select=c(playerID, yearID, teamID,
                                                    lgID, G, AB, R, H, X2B, X3B,
                                                    HR, RBI, SB, BB, SO, HBP, SH,
                                                    BA, PA, TB, SlugPct, OBP, OPS))

#select desired player fielding stats (defensive) from Fielding data
fieldingRelevant <- subset(Fielding, select=c(playerID, yearID, 
                                              PO, A , E , ZR)) %>%
                    group_by(playerID, yearID) %>%
                    summarise_all(funs(sum))
#some played multiple field positions which requires
#combining defensive stats for the year
                    

#combine fielding & batting data for players stats by year
playerstats <- merge(fieldingRelevant, battingRelevant)

#ensure there are no duplicates
playerstats <- distinct(playerstats)


#data cleanup

#look for NaNs
NaNcount <- playerstats %>% summarise_all(funs(sum(is.nan(.))))
View(NaNcount)#1 column with NaNs found (On Base Percentage)
#replace NaN in OBP with 0
playerstats[["OBP"]][is.nan(playerstats[["OBP"]])] <- 0
#confirm no NaNs
NaNcount <- playerstats %>% summarise_all(funs(sum(is.nan(.))))
View(NaNcount)

#How many null values?
nullcount <- playerstats %>% summarise_all(funs(sum(is.null(.))))
View(nullcount)#NONE FOUND

#look for NAs
NAcount <- playerstats %>% summarise_all(funs(sum(is.na(.))))
View(NAcount)

#remove records with NA for batting average
playerstats <- playerstats %>% drop_na(BA)

#replace remaining NAs with 0
playerstats[is.na(playerstats)] <- 0

#confirm there are no remaining NAs
NAcount <- playerstats %>% summarise_all(funs(sum(is.na(.))))
View(NAcount)

#clean up enviroment
rm(battingcalcdata, battingRelevant, fieldingRelevant,
   NAcount, NaNcount, nullcount)

#add timeperiod groupings to playerstats

#add time period groupings - these are timeframes established by MLB
#hall of fame in 2016 and corresponds with their 4 committees 
playerstats$timeperiod[playerstats$yearID<= '1949'] <- "Early Baseball (1871 to 1949)"
playerstats$timeperiod[playerstats$yearID>'1949' & playerstats$yearID<= '1969'] <- "Golden Days (1950 to 1969)"
playerstats$timeperiod[playerstats$yearID>'1969' & playerstats$yearID<= '1987'] <- "Modern Baseball (1970 to 1987)"
playerstats$timeperiod[playerstats$yearID>'1987' & playerstats$yearID<= '2018'] <- "Today's Game (1988 and later)"

#review dataframe
str(playerstats)
View(playerstats)
