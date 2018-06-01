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

# Reading the csv files in r markdown does not appear to be working, so I will save them to a loadable Rdata file
save(PUBG_Player_Statistics_solo, file="PUBG_Player_Statistics_solo.RData")
save(PUBG_Player_Statistics_duo, file="PUBG_Player_Statistics_duo.RData")
save(PUBG_Player_Statistics_squad, file="PUBG_Player_Statistics_squad.RData")

