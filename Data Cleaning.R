library(Lahman)
library(tidyverse)
library(missForest)
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

# Attach the HOF players (batters) to their primary position
# First Subset the Fielding Data to batters only
batters_fielding <- Fielding %>% 
  select(-c(2:5), -c(14:18)) %>% 
  filter(POS != "P") # Pitchers have separate Data Set, no need to include

# Redefine the numeric variable as Career Position Stats
batters_fielding <- batters_fielding %>%
  group_by(playerID, POS) %>%
  summarise_if(is.numeric, sum) %>% 
  # Subset to only the rows with matches in hof_players
  semi_join(hof_players, by = "playerID") %>% 
  # Reassign position by most frequently played position
  mutate(POS = POS[which.max(G)]) %>% 
  group_by(playerID, POS) %>% 
  # Career totals of the fielding data
  summarise_if(is.numeric, sum) %>% 
  filter(G >= 175) # Only players with a substantial amount of fielding

# Redefine the outfielder positions for their primary outfield 
data("FieldingOFsplit")
# Get the positions Left Field (LF), Center Field (CF), Right Field (RF)
of_fielding <- FieldingOFsplit %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Subset it to only those included in hof_players
  semi_join(hof_players, by = "playerID") %>% 
  # Update position to most frequently played position
  mutate(POS = POS[which.max(G)]) %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(G >= 175)

batters_fielding <- batters_fielding %>% 
  # Join together with previous fielding stats
  left_join(of_fielding, by = "playerID") %>% 
  # Change the position variable to of_fielding$POS
  mutate(POS.x = ifelse(!is.na(POS.y), POS.y, POS.x)) %>% 
  # Remove the variables from of_fielding
  select(-c(POS.y:DP.y)) %>% 
  # Remove ".x" endings from the variables
  rename_with(~gsub("\\.x", "", .), contains(".x"))

# Subset Fielding Data to pitchers only
pitchers_fielding <- Fielding %>% 
  select(-c(2:5), -c(14:18)) %>% 
  filter(POS == "P")

# Hall Of Fame pitchers fielding data
pitchers_fielding <- pitchers_fielding %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  semi_join(hof_players, by = "playerID") %>% 
  filter(G >= 40)

# Calculate career Pitching Stats
career_pitching <- Pitching %>% 
  select(-yearID, -stint, -ERA, -BAOpp) %>% 
  mutate(IP = round(IPouts / 3, 1), # Innings Pitched
         .before = IPouts) %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Calculate career stats for this
  mutate(BAOpp = H / BFP, # Opponent Batting Average
         ERA = ER * 9 / IP, # Earned Run Average
         .after = SO) %>% 
  # Differentiate between Starting Pitchers and Relief Pitchers
  mutate(POS = ifelse(GS / G >= 0.7, "SP", "RP"),
         .before = W) %>%
  # Only include players with substantial amount of pitching data 500 IP
  filter(IP >= 500)

# Pitching stats for the Hall of Fame players
hof_pitching_stats <- career_pitching %>% 
  semi_join(hof_players, by = "playerID")

# Join together the pitching stats with their fielding stats
hof_pitching_stats <- hof_pitching_stats %>% 
  full_join(pitchers_fielding %>% 
              select(-G, -GS), by = "playerID") %>% 
  # Removes players who are not primarily pitchers
  filter(!is.na(POS)) 

# Calculate career Batting Statistics
career_batting <- Batting %>% 
  select(-yearID, -stint, -teamID, -lgID) %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Players with substantial amount of batting appearances
  filter(G >= 100, AB >= 500)

# Career Stats for Hall of Fame ballot hitters
hof_batting_stats <- career_batting %>% 
  semi_join(hof_players, by = "playerID")

# Join together the batting stats with their fielding stats
hof_batting_stats <- hof_batting_stats %>% 
  full_join(batters_fielding %>% 
              select(-G), by = "playerID") %>% 
  # Excludes the pitchers batting stats, we are not interested in those
  filter(!is.na(POS))

# Awards for Consideration
awards <- c("Triple Crown", "Most Valuable Player", "World Series MVP", 
            "Cy Young Award", "Rolaids Relief Man Award", "Gold Glove",
            "Reliever of the Year Award", "Silver Slugger", 
            "Pitching Triple Crown")

# Fix Spelling error in the Awards Data
AwardsPlayers <- AwardsPlayers %>% 
  mutate(awardID = ifelse(awardID == "SIlver Slugger", "Silver Slugger", 
                           awardID))

# Number of Awards by player, and awardID
num_awards <- AwardsPlayers %>% 
  filter(awardID %in% awards) %>% 
  group_by(playerID, awardID) %>% 
  summarise(num_awards = n())

# Pivot the data longer for easier formatting to add variables to the data.  
num_awards <- pivot_wider(data = num_awards,
            id_cols = playerID,
            names_from = awardID,
            values_from = num_awards)
# Certain awards have limitations
# Outstanding DH Award is an example as the DH position did not exist
# in the MLB prior to 1973
no_DH <- hof_players %>% 
  mutate(finalGame = as.Date(finalGame)) %>% 
  filter(year(finalGame) < 1973)

# Add the Award count for each of the batters
hof_batting_stats <- hof_batting_stats %>% 
  full_join(num_awards, by = "playerID") %>%
  # Make sure it is only players previously included in batting data
  filter(playerID %in% batters_fielding$playerID) %>% 
  # Remove the pitcher related season Awards
  select(-`Rolaids Relief Man Award`, -`Cy Young Award`, 
         -`Reliever of the Year Award`, -`Pitching Triple Crown`) %>% 
  # Triple Crowns have always been tracked, so if it's missing then the
  # player never achieved a batting triple crown
  mutate(`Triple Crown` = ifelse(is.na(`Triple Crown`), 0, `Triple Crown`))

# Add the Award count for each of the pitchers
# Remove batting awards, we're only concerned with their pitching awards
hof_pitching_stats <- hof_pitching_stats %>% 
  full_join(num_awards, by = "playerID") %>% 
  # Make sure it only includes players from previous batting data
  filter(playerID %in% pitchers_fielding$playerID) %>% 
  # Remove batting related season awards
  select(-`Triple Crown`, -`Silver Slugger`) %>% 
  # Same logic as for batting triple crown
  mutate(`Pitching Triple Crown` = ifelse(is.na(`Pitching Triple Crown`), 0, 
                                                `Pitching Triple Crown`) ) 

# Add major batting milestone indicator variables
hof_batting_stats <- hof_batting_stats %>% 
  mutate(`500_hr` = ifelse(HR >= 500, "Yes", "No"),
         `3000_hit` = ifelse(H >= 3000, "Yes", "No")) 

# Add major pitching milestone indicator variables
hof_pitching_stats <- hof_pitching_stats %>% 
  mutate(`300_wins` = ifelse(W >= 300, "Yes", "No"),
         `3000_so` = ifelse(SO >= 3000, "Yes", "No"),
         `300_sv` = ifelse(SV >= 300, "Yes", "No"))

# Character vector of players accused/caught using Performance Enhancing Drugs
# (PEDs)
ped_use <- c("Rafael Palmeiro", "Manny Ramirez", "Alex Rodriguez", "Barry Bonds", 
"Mark McGwire", "Roger Clemens", "Jose Canseco", "Bartolo Colon", "Nelson Cruz",
"Ryan Braun", "Miguel Tejada", "Melky Cabrera", "Yasmani Grandal", 
"Antonio Bastardo", "Francisco Cervelli", "Cameron Maybin", "Troy Patton", 
"Chris Davis", "David Rollins", "Ervin Santana", 
"Andrew McKirahan", "Cody Stanley", "Abraham Almonte", "Daniel Stumpf", 
"Chris Colabello", "Dee Strange-Gordon", "Josh Ravin", "Adalberto Mondesi", 
"Alec Asher",
"Starling Marte", "David Paulino", "Jorge Bonifacio", "Jorge Polanco", 
"Robinson Cano", "Welington Castillo", "Steven Wright", "Francis Martes", 
"Frankie Montas", "Tim Beckham", "Michael Pineda", "Paul Campbell", 
"Gregory Santos", "Hector Santiago", "Ramon Laureano", "Pedro Severino", 
"Jenrry Mejia", "Fernando Tatis", "Jason Giambi", "David Ortiz", 
"Ivan Rodriguez", "Gary Sheffield", "Sammy Sosa", "Juan Gonzalez", 
"Mike Piazza", "Jeff Bagwell")

# Getting the player information (particularly player ids) for these PED
# players
ped_players <- People %>%
  arrange(nameFirst) %>% 
  # Create a Full Name vector for easier filtering
  mutate(nameFull = str_c(nameFirst, nameLast, sep = " ")) %>%
  # Extract only those players whose names are featured in ped_use
  filter(nameFull %in% ped_use)  %>%
  # Remove players with repeat names (select only ones we want)
  filter(!(
    playerID %in% c("braunry01", "campbpa02", "cruzne01",
                    "tatisfe01")
  ))

# Add ped_use indicator to the batting stats
hof_batting_stats <- hof_batting_stats %>% 
  mutate(ped_use = ifelse(playerID %in% ped_players$playerID, "Yes", "No")) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(!is.na(G)) # Remove non-batters

# Add ped_use indicator to the pitching stats
hof_pitching_stats <- hof_pitching_stats %>% 
  mutate(ped_use = ifelse(playerID %in% ped_players$playerID, "Yes", "No")) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(!is.na(G))# Remove non-pitchers

# get the Active Players for later prediction
# Active Player = 10 years played, but not 5 years retired
# For Hall Of Fame eligibility one must have a career length of 10 years
# and the must have been retired for at least 5 years.
active_players <- People %>% 
  anti_join(hof_players, by = "playerID") %>% 
  mutate(finalGame = as.Date(finalGame),
         debut = as.Date(debut)) %>% 
  mutate(length_career = (year(finalGame) - year(debut) ) ) %>% 
  filter(length_career >= 10 & year(finalGame) >= 2017)
