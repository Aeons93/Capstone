colnames(PUBG_Player_Statistics)


levels(PUBG_Player_Statistics$player_name)
subset(PUBG_Player_Statistics, select = "player_name")

colnames(PUBG_Player_Statistics[3])

# Separate the main dataset between Solo, Duo, and Squad sets. 
library(dplyr)
# Separate the solo set
PUBG_Player_Statistics_solo<-select(PUBG_Player_Statistics,starts_with("player"),starts_with("solo"))
# removing unneeded columns
# Revivals are only possible with teammates, considering these are solo stats, then the revive statistic is unneeded.
PUBG_Player_Statistics_solo$solo_Revives = NULL
PUBG_Player_Statistics_solo$solo_RevivesPg = NULL
# Unsure what this statistic is, it is 0 for all sets, solo or team. 
PUBG_Player_Statistics_solo$solo_WeaponAcquired = NULL
# In PUBG, DBNO stands for "down but not out"
# However, the status of being "downed" only occurs while in teams, so this is unneeded for solo stats 
PUBG_Player_Statistics_solo$solo_DBNOs = NULL

#Separate the Duo set
PUBG_Player_Statistics_duo<-select(PUBG_Player_Statistics,starts_with("player"),starts_with("duo"))
# All statistics in this set should be relevant to an extent, except weapon acquired
PUBG_Player_Statistics_duo$duo_WeaponAcquired = NULL

#Separate the Squad set
PUBG_Player_Statistics_squad<-select(PUBG_Player_Statistics,starts_with("player"),starts_with("squad"))
PUBG_Player_Statistics_squad$squad_WeaponAcquired = NULL


write.csv(PUBG_Player_Statistics_solo,"PUBG_Player_Statistics_solo.csv")
write.csv(PUBG_Player_Statistics_duo,"PUBG_Player_Statistics_duo.csv")
write.csv(PUBG_Player_Statistics_squad,"PUBG_Player_Statistics_squad.csv")


solo<- PUBG_Player_Statistics_solo[ which(PUBG_Player_Statistics_solo$solo_RoundsPlayed > 200),]

library(ggplot2)
ggplot(solo, aes(
  x = solo$solo_Wins, y = solo$solo_KillDeathRatio, colour = solo$solo_WinRatio)) +
  geom_point() + 
  geom_line()

# This plot seems to be a suitable presentation for aggression tactics vs victory ratios
# Plots: K/D Vs Win Ratios, colored by most kills for a round in a gradient. 
# the most kills per round stat may indicate information regarding how aggressive a player is during rounds compared to how often they win and how often they actually die (K/D)
ggplot(solo, aes(
  x = solo$solo_KillDeathRatio, y = solo$solo_WinRatio)) +
  geom_point(aes(colour=solo$solo_RoundMostKills)) +
  scale_colour_gradient(limits = c(0,15))
# Need to clear out players with less than around 200 games, there are too many of them with 100% win rates which seems to throw off the data.
# There also seems to be a high concentration of players closer to the low win ratio, hopefully clearing out the players who clearly do not play as much will help
# Further analysis for team play would be next, following a similar logic. 

#Histograms seem to portray the concentration of user stats very well. It is visually helpful as well as reasonable looking.
qplot(solo$solo_KillDeathRatio, 
      geom = "histogram",
      binwidth = .01, 
      fill=I("blue"), 
      col =I("red"))

solo <- read.csv("PUBG_Player_Statistics_solo.csv")

solo <- solo[order(solo$solo_RoundsPlayed),]
solo2 <- subset(solo, as.numeric(rownames(solo))>70000)
# [c(70000:87898)]

# subset the players with more than 100 rounds played to analyze players with greater experience
solo2 <- solo[which(solo$solo_RoundsPlayed > 100),]

# Reading the csv files in r markdown does not appear to be working, so I will save them to a loadable Rdata file
save(PUBG_Player_Statistics_solo, file="PUBG_Player_Statistics_solo.RData")
save(PUBG_Player_Statistics_duo, file="PUBG_Player_Statistics_duo.RData")
save(PUBG_Player_Statistics_squad, file="PUBG_Player_Statistics_squad.RData")

head(solo[order(-solo$solo_RoundsPlayed),])

#Solo K/D histogram
qplot(solo$solo_Kills, 
      xlab="Solo K/D ratio",
      geom="histogram",
      binwidth=100, 
      fill=I("blue"), 
      col=I("red"))

#Duo K/D histogram
qplot(duo$duo_Kills, 
      xlab="Duo K/D ratio",
      geom="histogram",
      binwidth=100, 
      fill=I("blue"), 
      col=I("red"))

#Squad K/D histogram
qplot(team$squad_Kills, 
      xlab="Squad K/D ratio",
      geom="histogram",
      binwidth=100, 
      fill=I("blue"), 
      col=I("red"))

rownames_to_column()
## change the rownames to player names to make all data numeric for k-means and clustering 
##rownames(solo)<-NULL
rownames(solo)<-solo$player_name
##solo$player_name <- NULL
rownames(duo)<-duo$player_name
##duo$player_name <- NULL

rownames(team)<-team$player_name
##team$player_name<- NULL
## duplicated rows detected, removal via duplicated function
team<-unique(team) #removed duplicate
team[duplicated(team),]
squad<-team ##renamed to be consistent

## save datasets as new solo, duo, squad sets

save(solo, file="solo.RData")
save(duo, file="duo.RData")
save(squad, file="squad.RData")

load(file="solo.RData")
load(file="duo.RData")
load(file="squad.RData")

library("cluster")
solo_km<-kmeans(solo, centers = 3)
clusplot(solo, solo_km$cluster, labels = 1)

duo_km<-kmeans(duo, centers = 3)
clusplot(duo, duo_km$cluster, labels = 1)

squad_km<-kmeans(squad, centers = 3)
clusplot(squad, squad_km$cluster, labels = 1)

solo[c("Benny-_-"),]

solo[c("TwitchTV_Gudybay",
       "Proficient6",
       "Shiv",
       "MrGrimmmz",
       "LeeGo222",
       "ufo8mycow",
       "Youzi"
),
c("solo_KillDeathRatio",
  "solo_KillsPg",
  "solo_WinRatio",
  "solo_Wins", 
  "solo_RoundsPlayed")]


# subset of only focused columns to recheck clustering

#solo_focused<-solo_focused[,c("player_name",
#                              "solo_KillDeathRatio",
#                              "solo_KillsPg",
#                              "solo_WinRatio",
#                              "solo_Wins", 
#                              "solo_RoundsPlayed")]

solo_focused<-solo[,c("solo_KillDeathRatio",
                      "solo_KillsPg",
                      "solo_WinRatio",
                      "solo_Wins", 
                      "solo_RoundsPlayed")]
solo_focused$player_name<-rownames(solo)

solo_focused<-solo_focused[which(solo_focused$solo_KillDeathRatio > 2),]
solo_focused<-solo_focused[which(solo_focused$solo_WinRatio > 5),]
solo_focused<-solo_focused[which(solo_focused$solo_Wins > 30),]

rownames(solo_focused)<-solo_focused$player_name
solo_focused$player_name <- NULL
wssplot(solo_focused)
solo_focused_km<-kmeans(solo_focused,centers=4)
clusplot(solo_focused,solo_focused_km$cluster, labels=1)
solo_focused["RelliK2k",]



# doing the same for the other team comps
#duo

duo_focused<-duo[,c("duo_KillDeathRatio",
                    "duo_KillsPg",
                    "duo_WinRatio",
                    "duo_Wins", 
                    "duo_RoundsPlayed")]
duo_focused$player_name<-rownames(duo)

duo_focused<-duo_focused[which(duo_focused$duo_KillDeathRatio > 2.5),]
duo_focused<-duo_focused[which(duo_focused$duo_WinRatio > 10),]
duo_focused<-duo_focused[which(duo_focused$duo_Wins > 30),]

rownames(duo_focused)<-duo_focused$player_name
duo_focused$player_name <- NULL
wssplot(duo_focused)
duo_focused_km<-kmeans(duo_focused,centers=5)
clusplot(duo_focused,duo_focused_km$cluster, labels=1)

duo_focused["DTM-V",]


# squad
squad_focused<-squad[,c("squad_KillDeathRatio",
                        "squad_KillsPg",
                        "squad_WinRatio",
                        "squad_Wins", 
                        "squad_RoundsPlayed")]
squad_focused$player_name<-rownames(squad)

squad_focused<-squad_focused[which(squad_focused$squad_KillDeathRatio > 2.5),]
squad_focused<-squad_focused[which(squad_focused$squad_WinRatio > 20),]
squad_focused<-squad_focused[which(squad_focused$squad_Wins > 30),]

rownames(squad_focused)<-squad_focused$player_name
squad_focused$player_name <- NULL
wssplot(squad_focused)
squad_focused_km<-kmeans(squad_focused,centers=4)
clusplot(squad_focused,squad_focused_km$cluster, labels=1)

squad_focused["Zoidm8",]


save(solo_focused, file="solo_focused.RData")
save(duo_focused, file="duo_focused.RData")
save(squad_focused, file="squad_focused.RData")





#second table for a more stable reference to player_name
solo_focused_named<-solo[,c("solo_KillDeathRatio",
                            "solo_KillsPg",
                            "solo_WinRatio",
                            "solo_Wins", 
                            "solo_RoundsPlayed")]
solo_focused_named$player_name<-rownames(solo)

solo_focused_named<-solo_focused_named[which(solo_focused_named$solo_KillDeathRatio > 2),]
solo_focused_named<-solo_focused_named[which(solo_focused_named$solo_WinRatio > 5),]
solo_focused_named<-solo_focused_named[which(solo_focused_named$solo_Wins > 30),]

solo_focused_named<-solo_focused_named[,c( "player_name",
                                           "solo_KillDeathRatio",
                                           "solo_KillsPg",
                                           "solo_WinRatio",
                                           "solo_Wins", 
                                           "solo_RoundsPlayed")]
rownames(solo_focused_named)<-solo_focused_named$player_name

duo_focused_named<-duo[,c("duo_KillDeathRatio",
                          "duo_KillsPg",
                          "duo_WinRatio",
                          "duo_Wins", 
                          "duo_RoundsPlayed")]
duo_focused_named$player_name<-rownames(duo)

duo_focused_named<-duo_focused_named[which(duo_focused_named$duo_KillDeathRatio > 2.5),]
duo_focused_named<-duo_focused_named[which(duo_focused_named$duo_WinRatio > 10),]
duo_focused_named<-duo_focused_named[which(duo_focused_named$duo_Wins > 30),]

duo_focused_named<-duo_focused_named[,c("player_name",
                                           "duo_KillDeathRatio",
                                           "duo_KillsPg",
                                           "duo_WinRatio",
                                           "duo_Wins", 
                                           "duo_RoundsPlayed")]
rownames(duo_focused_named)<-duo_focused_named$player_name

squad_focused_named<-squad[,c("squad_KillDeathRatio",
                        "squad_KillsPg",
                        "squad_WinRatio",
                        "squad_Wins", 
                        "squad_RoundsPlayed")]
squad_focused_named$player_name<-rownames(squad)

squad_focused_named<-squad_focused_named[which(squad_focused_named$squad_KillDeathRatio > 2.5),]
squad_focused_named<-squad_focused_named[which(squad_focused_named$squad_WinRatio > 20),]
squad_focused_named<-squad_focused_named[which(squad_focused_named$squad_Wins > 30),]

squad_focused_named<-squad_focused_named[,c( "player_name",
                                           "squad_KillDeathRatio",
                                           "squad_KillsPg",
                                           "squad_WinRatio",
                                           "squad_Wins", 
                                           "squad_RoundsPlayed")]
rownames(squad_focused_named)<-squad_focused_named$player_name



save(solo_focused_named, file="solo_focused_named.RData")
save(duo_focused_named, file="duo_focused_named.RData")
save(squad_focused_named, file="squad_focused_named.RData")