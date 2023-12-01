library(Lahman)
library(tidyverse)
# Load the Hall of Fame ballots data
data(HallOfFame)
# Load the Batting Data
data(Batting)
# Load the Fielding Data
data(Fielding)
# Load the Pitching Data
data(Pitching)
# Append the batting statistics for players included on Hall
# of Fame ballots
hof_batting <- HallOfFame %>% 
        # Only those voted into Hall of Fame as players
        filter(category == "Player") %>% 
        distinct(playerID) %>% # Some players are on ballot more than once
        inner_join(Batting, by = "playerID") %>% 
        group_by(playerID) %>% 
        summarise(across(-c(1:5), sum)) # Summarise for Career Stats
# Append the pitching statistics for players on Hall of Fame
# ballots
HallOfFame %>% 
        filter(category)