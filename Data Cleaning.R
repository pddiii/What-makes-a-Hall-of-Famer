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
# Load the Player Personal Information
data(People)
# Load the Season Awards Data
data(AwardsPlayers)
# Players who have appeared on the Hall of Fame Ballots
# and received votes
hof_players <- HallOfFame %>% 
  mutate(pct_vote = round((votes / ballots), 4), .after = votes) %>% 
  filter(category == "Player", !is.na(pct_vote))
# Attach the HOF players to their respective Player Information
hof_players <- hof_players %>% 
  inner_join(People, by = "playerID") %>% 
  select(-c(11:22), -deathDate, -birthDate)
# Awards for Consideration
awards <- c("Triple Crown", "Most Valuable Player", "World Series MVP", 
            "Cy Young Award", "Rolaids Relief Man Award", 
            "Outstanding DH Award", "Reliever of the Year Award")
# 
AwardsPlayers %>% 
  # Fix Spelling error in the Awards Data
  mutate(awardID = ifelse(awardID == "SIlver Slugger", "Silver Slugger", 
                           awardID)) %>%
  filter(awardID %in% awards) %>% 
  group_by(playerID, awardID) %>% 
  summarise(num_awards = n()) %>% 
  arrange(desc(num_awards))
  
  
Fielding

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