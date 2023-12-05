# Table of Batter Variables

| Variable  | Description                                      | Type of Variable | Calculation | Example |
|-----------|--------------------------------------------------|------------------|-------------|-------------|
| playerID  | Unique identifier for each player.              | -                 | -           |"CF"            |
| POS       | Player's primary fielding position.             | -                 | -           | -           |
| inducted  | Hall of Fame induction status (1 for Yes, 0 for No). | -              | -           | -           |
| play_era  | Player's era or era range during active play.   | -                 | -           | -           |
| G         | Games played.                                    | Numeric           | -           | -           |
| AB        | At-bats.                                         | Numeric           | -           | -           |
| R         | Runs scored.                                    | Numeric           | -           | -           |
| H         | Hits.                                           | Numeric           | -           | -           |
| X2B       | Doubles.                                        | Numeric           | -           | -           |
| X3B       | Triples.                                        | Numeric           | -           | -           |
| HR        | Home runs.                                      | Numeric           | -           | -           |
| RBI       | Runs batted in.                                 | Numeric           | -           | -           |
| SB        | Stolen bases.                                   | Numeric           | -           | -           |
| CS        | Caught stealing.                                | Numeric           | -           | -           |
| BB        | Walks (Base on Balls).                          | Numeric           | -           | -           |
| SO        | Strikeouts.                                     | Numeric           | -           | -           |
| IBB       | Intentional walks.                              | Numeric           | -           | -           |
| HBP       | Hit by pitch.                                   | Numeric           | -           | -           |
| SH        | Sacrifice hits.                                 | Numeric           | -           | -           |
| SF        | Sacrifice flies.                                | Numeric           | -           | -           |
| GIDP      | Ground into double play.                        | Numeric           | -           | -           |
| GS        | Games started.                                  | Numeric           | -           | -           |
| InnOuts   | Innings played (outs recorded).                 | Numeric           | -           | -           |
| PO        | Putouts.                                        | Numeric           | -           | -           |
| A         | Assists.                                        | Numeric           | -           | -           |
| E         | Errors.                                         | Numeric           | -           | -           |
| DP        | Double plays turned.                            | Numeric           | -           | -           |
| PB        | Passed balls (catcher statistic).               | Numeric           | -           | -           |
| WP        | Wild pitches (catcher statistic).               | Numeric           | -           | -           |
| SB_against | Stolen bases allowed by a catcher.             | Numeric           | -           | -           |
| CS_for    | Runners caught stealing by a catcher.           | Numeric           | -           | -           |
| ZR        | Zone rating (fielding statistic).               | Numeric           | -           | -           |
| PA        | Plate appearances.                              | Numeric           | -           | -           |
| BA        | Batting average.                                | Numeric           | -           | $AB + BB + HBP + SF$           |
| 1B        | Singles.                                        | Numeric           | -           | -           |
| SLG       | Slugging percentage.                            | Numeric           | -           | -           |
| OBP       | On-base percentage.                             | Numeric           | -           | -           |
| OPS       | On-base plus slugging.                          | Numeric           | -           | -           |
| SO%       | Strikeout percentage.                           | Numeric           | -           | -           |
| BB%       | Walk percentage.                                | Numeric           | -           | -           |
| BB:SO     | Walk-to-strikeout ratio.                        | Numeric           | -           | -           |
| Range     | Defensive range.                                | Numeric           | -           | -           |
| Triple Crown | Achievement of leading the league in batting average, home runs, and RBIs in the same season. | Factor (0/1/2) | - | - |
| 500_hr    | 500 or more career home runs. (0 for No, 1 for Yes)                   | Binary (0/1)      | -           | -           |
| 3000_hit  | 3000 or more career hits. (0 for No, 1 for Yes)                       | Binary (0/1)      | -           | -           |
| 1500_runs | 1500 or more career runs. (0 for No, 1 for Yes)                      | Binary (0/1)      | -           | -           |
| 1500_rbi  | 1500 or more career runs batted in. (0 for No, 1 for Yes)           | Binary (0/1)      | -           | -           |
| 2500_G    | 2500 or more career games played. (0 for No, 1 for Yes)             | Binary (0/1)      | -           | -           |
| ped_use   | Indicates whether the player has been linked to the use of performance-enhancing drugs (PEDs). (0 for No, 1 for Yes) | Binary (0/1) | - | - |
