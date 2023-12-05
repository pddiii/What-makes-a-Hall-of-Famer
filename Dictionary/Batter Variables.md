# Table of Batter Variables

| Variable  | Description                                      | Type of Variable | Calculation | Example |
|-----------|--------------------------------------------------|------------------|-------------|-------------|
| playerID  | Unique identifier for each player.              | -                 | -           |"bondsba01"            |
| POS       | Player's primary fielding position.             | -                 | -           | "CF"           |
| inducted  | Hall of Fame induction status (1 for Yes, 0 for No). | -              | -           | -           |
| play_era  | Player's era or era range during active play.   | -                 | -           | "Dead Ball"           |
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
| BA        | Batting average.                                | Numeric           | $AB + BB + HBP + SF$           | -           |
| 1B        | Singles.                                        | Numeric           | $\frac{H}{AB}$           | -           |
| SLG       | Slugging percentage.                            | Numeric           | $\frac{1B + 2 * X2B + 3 * X3B + 4 * HR}{AB}$           | -           |
| OBP       | On-base percentage.                             | Numeric           | $\frac{H + BB + HBP}{PA}$           | -          |
| OPS       | On-base plus slugging.                          | Numeric           | $OBP + SLG$           | -           |
| SO%       | Strikeout percentage.                           | Numeric           | $\frac{SO}{PA}$           | -           |
| BB%       | Walk percentage.                                | Numeric           | $\frac{BB}{PA}$           | -           |
| BB:SO     | Walk-to-strikeout ratio.                        | Numeric           | $\frac{BB}{SO}$           | -           |
| Range     | Defensive range.                                | Numeric           | $\frac{A + PO}{G}$           | -           |
| Triple Crown | Achievement of leading the league in batting average, home runs, and RBIs in the same season. | Factor (0/1/2) | - | - |
| 500_hr    | 500 or more career home runs. (0 for No, 1 for Yes)                   | Binary (0/1)      | -           | -           |
| 3000_hit  | 3000 or more career hits. (0 for No, 1 for Yes)                       | Binary (0/1)      | -           | -           |
| 1500_runs | 1500 or more career runs. (0 for No, 1 for Yes)                      | Binary (0/1)      | -           | -           |
| 1500_rbi  | 1500 or more career runs batted in. (0 for No, 1 for Yes)           | Binary (0/1)      | -           | -           |
| 2500_G    | 2500 or more career games played. (0 for No, 1 for Yes)             | Binary (0/1)      | -           | -           |
| ped_use   | Indicates whether the player has been linked to the use of performance-enhancing drugs (PEDs). (0 for No, 1 for Yes) | Binary (0/1) | - | - |
