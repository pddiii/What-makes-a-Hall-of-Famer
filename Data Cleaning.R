library(Lahman)
library(tidyverse)
# Load the Hall of Fame ballots data
data(HallOfFame)
# Load the Batting Data
data(Batting)
# Load the Fielding Data
data(Fielding)
# Load the Fielding Splits (Left Field / Center Field / Right Field)
data(FieldingOFsplit)
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
  mutate(inducted = ifelse(inducted == "Y", 1, 0)) %>% 
  filter(category == "Player")

# Attach the HOF players to their respective Player Information
hof_players <- hof_players %>% 
  inner_join(People, by = "playerID") %>% 
  select(-c(11:22), -deathDate, -birthDate) %>% 
  # Add indicator variable play_era to distinguish time frames of MLB play
  mutate(debut = year(as.Date(debut)),
         finalGame = year(as.Date(finalGame)),
         play_era = as.factor(ifelse(finalGame < 1920, "Dead Ball", 
               ifelse(finalGame >= 1920 & finalGame < 1941, "Live Ball", 
               ifelse(finalGame >= 1941 & finalGame < 1961, "Integration",
               ifelse(finalGame >= 1961 & finalGame < 1977, "Expansion",
               ifelse(finalGame >= 1977 & finalGame < 1994, "Free Agency",
               ifelse(finalGame >= 1994 & finalGame < 2006, "Steroids",
                      "Modern") ) ) ) ) )) )

# Attach the HOF players (batters) to their primary position
# First Subset the Fielding Data to batters only
batters_fielding <- Fielding %>% 
  select(-c(2:5)) %>% 
  filter(POS != "P") %>% # Pitchers have separate Data Set, no need to include
  replace(is.na(.), 0) 

# Redefine the numeric variable as Career Position Stats
hof_batters_fielding <- batters_fielding %>%
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

# Get the positions Left Field (LF), Center Field (CF), Right Field (RF)
hof_of_fielding <- FieldingOFsplit %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Subset it to only those included in hof_players
  semi_join(hof_players, by = "playerID") %>% 
  # Update position to most frequently played position
  mutate(POS = POS[which.max(G)]) %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(G >= 175)

hof_batters_fielding <- hof_batters_fielding %>% 
  # Join together with previous fielding stats
  left_join(hof_of_fielding, by = "playerID") %>% 
  # Change the position variable to of_fielding$POS
  mutate(POS.x = ifelse(!is.na(POS.y), POS.y, POS.x)) %>% 
  # Remove the variables from of_fielding
  select(-c(POS.y:DP.y)) %>% 
  # Remove ".x" endings from the variables
  rename_with(~gsub("\\.x", "", .), contains(".x"))

# Calculate career Batting Statistics
career_batting <- Batting %>% 
  select(-yearID, -stint, -teamID, -lgID)  %>% 
  replace(is.na(.), 0) %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Players with substantial amount of batting appearances
  filter(G >= 100, AB >= 500)

# Career Stats for Hall of Fame ballot hitters
hof_batting_stats <- career_batting %>% 
  semi_join(hof_players, by = "playerID")

# Join together the batting stats with their fielding stats
hof_batting_stats <- hof_batting_stats %>% 
  full_join(hof_batters_fielding %>% 
              select(-G), by = "playerID") %>% 
  # Excludes the pitchers batting stats, we are not interested in those
  filter(!is.na(POS)) %>% 
  mutate(PA = AB + BB + HBP + SF, # plate appearances
         BA = H / AB, # batting average
         `1B` = H - X2B - X3B - HR, # singles
         SLG = (`1B` + 2 * X2B + 3 * X3B + 4 * HR) / AB, # Slugging %
         OBP = (H + BB + HBP) / PA, # On Base Percentage
         OPS = SLG + OBP, # On Base plus Slugging Percentage
         `SO%` = SO / PA, # Strikeout Percentage
         `BB%` = BB / PA, # Base-on-Balls (Walk) percentage
         `BB:SO` = BB / SO, # Walk to Strikeout Ratio
         Range = (A + PO) / G )   # Range Factor for fielders 
# Subset Fielding Data to pitchers only
pitchers_fielding <- Fielding %>% 
  select(-c(2:5), -c(14:18)) %>% 
  filter(POS == "P")

# Hall Of Fame pitchers fielding data
hof_pitchers_fielding <- pitchers_fielding %>% 
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
         `SO%` = SO / BFP, # Strikeout Percentage
         `BB%` = BB / BFP, # Walk Percentage
         `SO:BB` = SO / BB, # Strikeout to Walk Ratio
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
  full_join(hof_pitchers_fielding %>% 
              select(-G, -GS), by = "playerID") %>% 
  # Removes players who are not primarily pitchers
  filter(!is.na(POS)) 

# Awards for Consideration
awards <- c("Triple Crown", "Pitching Triple Crown")

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

# Add the Award count for each of the batters
hof_batting_stats <- hof_batting_stats %>% 
  full_join(num_awards, by = "playerID") %>%
  # Make sure it is only players previously included in batting data
  filter(playerID %in% hof_batters_fielding$playerID) %>% 
  # Remove the pitcher related season Awards
  select(-`Pitching Triple Crown`) %>% 
  # Triple Crowns have always been tracked, so if it's missing then the
  # player never achieved a batting triple crown
  mutate(`Triple Crown` = as.factor(ifelse(is.na(`Triple Crown`), 0, 
                                           `Triple Crown`)) )

# Add the Award count for each of the pitchers
# Remove batting awards, we're only concerned with their pitching awards
hof_pitching_stats <- hof_pitching_stats %>%
  full_join(num_awards, by = "playerID") %>%
  # Make sure it only includes players from previous batting data
  filter(playerID %in% hof_pitchers_fielding$playerID) %>%
  # Remove batting related season awards
  select(-`Triple Crown`) %>%
  # Same logic as for batting triple crown
  mutate(`Pitching Triple Crown` = as.factor(ifelse(
                                  is.na(`Pitching Triple Crown`),
                                  0, `Pitching Triple Crown`
                                  ) ) )

# Add major batting milestone indicator variables
hof_batting_stats <- hof_batting_stats %>% 
  mutate(`500_hr` = ifelse(HR >= 500, "Yes", "No"),
         `3000_hit` = ifelse(H >= 3000, "Yes", "No"),
         `1500_runs` = ifelse(R >= 1500, "Yes", "No"),
         `1500_rbi` = ifelse(RBI >= 1500, "Yes", "No"),
         `2500_gp` = ifelse(GP >= 2500, "Yes", "No"))

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

hof_induction <- hof_players %>% 
  select(playerID, inducted, play_era) %>% 
  group_by(playerID) %>%
  summarise(inducted = sum(inducted), play_era = first(play_era)) %>% 
  mutate(inducted = as.factor(inducted))
  
# Add the batter's Hall of Fame Induction status
hof_batting_stats <- hof_batting_stats %>% 
  inner_join(hof_induction, by = "playerID") %>% 
  distinct(playerID, .keep_all = TRUE) %>% 
  relocate(c(inducted, play_era), .before = G) %>% 
  relocate(POS, .after = playerID) %>% 
  rename(SB = SB.x, CS = CS.x, SB_against = SB.y, CS_for = CS.y)

# Add the Pitcher's Hall of Fame Induction Status
hof_pitching_stats <- hof_pitching_stats %>% 
  inner_join(hof_induction, by = "playerID") %>% 
  distinct(playerID, .keep_all = TRUE) %>% 
  relocate(c(inducted, play_era), .before = G)

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

active_batters_fielding <- batters_fielding %>% 
  group_by(playerID, POS) %>%
  summarise_if(is.numeric, sum) %>% 
  # Subset to only the rows with matches in active_players
  semi_join(active_players, by = "playerID") %>% 
  # Reassign position by most frequently played position
  mutate(POS = POS[which.max(G)]) %>% 
  group_by(playerID, POS) %>% 
  # Career totals of the fielding data
  summarise_if(is.numeric, sum) %>% 
  filter(G >= 175) # Only players with a substantial amount of fielding

active_of_fielding <- FieldingOFsplit %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  # Subset it to only those included in hof_players
  semi_join(active_players, by = "playerID") %>% 
  # Update position to most frequently played position
  mutate(POS = POS[which.max(G)]) %>% 
  group_by(playerID, POS) %>% 
  summarise_if(is.numeric, sum) %>% 
  filter(G >= 175) 

active_batters_fielding <- active_batters_fielding %>% 
  # Join together with previous fielding stats
  left_join(active_of_fielding, by = "playerID") %>% 
  mutate(POS.x = ifelse(!is.na(POS.y), POS.y, POS.x)) %>% 
  # Remove the variables from active_of_fielding
  select(-c(POS.y:DP.y)) %>% 
  # Remove ".x" endings from the variables
  rename_with(~gsub("\\.x", "", .), contains(".x"))

# Career Stats for Hall of Fame ballot hitters
active_batting_stats <- career_batting %>% 
  semi_join(active_players, by = "playerID") %>% 
  # Replace the missing statistics values with 0
  replace(is.na(.), 0) %>% 
  # Add batting rate stats
  mutate(PA = AB + BB + HBP + SF, # plate appearances
         BA = H / AB, # batting average
         `1B` = H - X2B - X3B - HR, # singles
         SLG = (`1B` + 2 * X2B + 3 * X3B + 4 * HR) / AB, # Slugging %
         OBP = (H + BB + HBP) / PA, # On Base Percentage
         OPS = SLG + OBP, # On Base plus Slugging Percentage
         `SO%` = SO / PA, # Strikeout Percentage
         `BB%` = BB / PA, # Base-on-Balls (Walk) percentage
         `BB:SO` = BB / SO ) # Walk to Strikeout Ratio

# Join together the batting stats with their fielding stats
active_batting_stats <- active_batting_stats %>% 
  full_join(active_batters_fielding %>% 
              select(-G), by = "playerID") %>% 
  # Excludes the pitchers batting stats, we are not interested in those
  filter(!is.na(POS))

active_pitchers_fielding <- pitchers_fielding %>% 
  group_by(playerID) %>% 
  summarise_if(is.numeric, sum) %>% 
  semi_join(active_players, by = "playerID") %>% 
  filter(G >= 40)

# Pitching stats for the Hall of Fame players
active_pitching_stats <- career_pitching %>% 
  semi_join(active_players, by = "playerID")

# Join together the pitching stats with their fielding stats
active_pitching_stats <- active_pitching_stats %>% 
  full_join(active_pitchers_fielding %>% 
              select(-G, -GS), by = "playerID") %>% 
  # Removes players who are not primarily pitchers
  filter(!is.na(POS)) 

# Add the Award count for each of the batters
active_batting_stats <- active_batting_stats %>% 
  full_join(num_awards, by = "playerID") %>%
  # Make sure it is only players previously included in batting data
  filter(playerID %in% active_batters_fielding$playerID) %>% 
  # Remove the pitcher related season Awards
  select(-`Pitching Triple Crown`) %>% 
  replace(is.na(.), 0)

# Add the Award count for each of the pitchers
# Remove batting awards, we're only concerned with their pitching awards
active_pitching_stats <- active_pitching_stats %>% 
  full_join(num_awards, by = "playerID") %>% 
  # Make sure it only includes players from previous batting data
  filter(playerID %in% active_pitchers_fielding$playerID) %>% 
  # Remove batting related season awards
  select(-`Triple Crown`) %>% 
  # Replace NA Values in the data, only award columns
  replace(is.na(.), 0)

# Add major batting milestone indicator variables
active_batting_stats <- active_batting_stats %>% 
  mutate(`500_hr` = ifelse(HR >= 500, "Yes", "No"),
         `3000_hit` = ifelse(H >= 3000, "Yes", "No"),
         `1500_runs` = ifelse(R >= 1500, "Yes", "No"),
         `1500_rbi` = ifelse(RBI >= 1500, "Yes", "No"),
         `2500_G` = ifelse(G >= 2500, "Yes", "No"))

# Add major pitching milestone indicator variables
active_pitching_stats <- active_pitching_stats %>% 
  mutate(`300_wins` = ifelse(W >= 300, "Yes", "No"),
         `3000_so` = ifelse(SO >= 3000, "Yes", "No"),
         `300_sv` = ifelse(SV >= 300, "Yes", "No") )

# Add ped_use indicator to the batting stats
active_batting_stats <- active_batting_stats %>% 
  mutate(ped_use = ifelse(playerID %in% ped_players$playerID, "Yes", "No"),
         `Triple Crown` = as.factor(`Triple Crown`),
         inducted = c(NA),
         play_era = "Modern") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Range = (PO + A) / G, .after = `BB:SO`) %>% 
  rename(SB = SB.x, CS = CS.x, SB_against = SB.y, CS_for = CS.y) %>% 
  mutate(PB = ifelse(POS == "C", PB, 0))

# Add ped_use indicator to the pitching stats
active_pitching_stats <- active_pitching_stats %>% 
  mutate(ped_use = ifelse(playerID %in% ped_players$playerID, "Yes", "No"),
         `Pitching Triple Crown` = as.factor(`Pitching Triple Crown`),
         inducted = c(NA),
         play_era = "Modern") %>% 
  mutate_if(is.character, as.factor)

# Remove the variables for sourcing the file for model purposes
rm(Batting, Fielding, AwardsPlayers, active_batters_fielding, active_of_fielding,
   active_pitchers_fielding, active_players, batters_fielding, career_batting,
   career_pitching, FieldingOFsplit, HallOfFame, hof_batters_fielding,
   hof_of_fielding, hof_induction, hof_pitchers_fielding, hof_players,
   num_awards, ped_players, People, pitchers_fielding, Pitching,
   awards, ped_use)
