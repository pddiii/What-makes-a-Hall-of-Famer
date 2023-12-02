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

# Calculate career Pitching Stats
career_pitching <- Pitching %>% 
  select(-yearID, -stint, -ERA, -BAOpp) %>% 
  mutate(IP = round(IPouts / 3, 1),
         .before = IPouts) %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(BAOpp = H / BFP,
         ERA = ER * 9 / IP,
         .after = SO) %>% 
  mutate(POS = ifelse(GS / G >= 0.7, "SP", "RP"),
         .before = W)
# Pitching stats for the Hall of Fame players
hof_pitching_stats <- career_pitching %>% 
  semi_join(hof_players, by = "playerID")

People %>% filter(playerID == "aardsda01")
# Attach the HOF players (batters) to their primary position
# First Subset the Fielding Data to our desired parameters
Fielding <- Fielding %>% 
  select(-c(2:5), -c(14:18)) %>% 
  filter(POS != "P") # Pitchers have separate Data Set, no need to include

# Redefine the numeric variable as Career Position Stats
career_fielding <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise_if(is.numeric, sum)

career_fielding %>% 
  filter(playerID %in% unique(hof_players$playerID),
         G / sum(G) >= 0.5)

# Awards for Consideration
awards <- c("Triple Crown", "Most Valuable Player", "World Series MVP", 
            "Cy Young Award", "Rolaids Relief Man Award", "Gold Glove",
            "Outstanding DH Award", "Reliever of the Year Award", 
            "Silver Slugger")

# Fix Spelling error in the Awards Data
AwardsPlayers <- AwardsPlayers %>% 
  mutate(awardID = ifelse(awardID == "SIlver Slugger", "Silver Slugger", 
                           awardID))

# Number of Awards by player, and awardID
num_awards <- AwardsPlayers %>% 
  filter(awardID %in% awards) %>% 
  group_by(playerID, awardID) %>% 
  summarise(num_awards = n())
  
pivot_wider(data = num_awards,
            id_cols = playerID,
            names_from = awardID,
            values_from = num_awards,
            values_fill = 0)

# Append the batting statistics for players included on Hall
# of Fame ballots
hof_batting <- HallOfFame %>% 
        # Only those voted into Hall of Fame as players
        filter(category == "Player") %>% 
        distinct(playerID) %>% # Some players are on ballot more than once
        inner_join(Batting, by = "playerID") %>% 
        group_by(playerID) %>% 
        summarise(across(-c(1:5), sum)) # Summarise for Career Stats

# Additions From Nelson

# HOF Pitching

#Get career mean of pitching averages statistics
hof_pitching_averages <- 
  HallOfFame %>% 
  filter(category == "Player") %>% 
  distinct(playerID) %>%
  inner_join(Pitching, by = "playerID") %>% 
  group_by(playerID) %>% 
  summarise(across(c(18,19), mean))

# Append the pitching totals for players included on Hall of Fame ballots
hof_pitching <- 
  HallOfFame %>% 
  filter(category == "Player") %>% 
  distinct(playerID) %>%
  inner_join(Pitching, by = "playerID") %>% 
  group_by(playerID) %>% 
  summarise(across(-c(1:4,18,19), sum))

# Join averages and totals together
hof_pitching <- 
  hof_pitching %>%
  inner_join(hof_pitching_averages, by = "playerID")

# Note Columns SH, SF, GIDP has many missing values
