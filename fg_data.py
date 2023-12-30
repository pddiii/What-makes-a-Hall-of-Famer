from pybaseball import batting_stats, pitching_stats
# Grab the career batting and pitching stats for all of MLB history 1871-2022
batting_data = batting_stats(1871, 2022, ind = 0)
pitching_data = pitching_stats(1871, 2022, ind = 0)
# Create .csv files of the FanGraphs data
batting_data.to_csv('fg_batting_data.csv', index=False)
pitching_data.to_csv('fg_pitching_data.csv', index=False)
