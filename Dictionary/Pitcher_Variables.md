# Pitcher Variables

| Variable              | Description                                     | Type of Variable | Calculation | Example |
|-----------------------|-------------------------------------------------|-------------------|-------------|-------------|
| playerID              | Unique identifier for each player.              | -                 | -           | "alexape01"           |
| POS                   | Player's primary fielding position. (SP or RP)            | -                 | -           | "SP"           |
| W                     | Wins (pitcher statistic).                        | Numeric           | -           | -           |
| L                     | Losses (pitcher statistic).                      | Numeric           | -           | -           |
| inducted              | Hall of Fame induction status (1 for Yes, 0 for No). | -              | -           | -           |
| play_era              | Player's era or era range during active play.   | -                 | -           | "Dead Ball"          |
| G                     | Games pitched.                                  | Numeric           | -           | -           |
| GS                    | Games started (pitcher statistic).               | Numeric           | -           | -           |
| CG                    | Complete games. (Starter pitches all 9 innings)                                 | Numeric           | -           | -           |
| SHO                   | Shutouts. (Pitcher allows 0 runs over 9 innings)                                       | Numeric           | -           | -           |
| SV                    | Saves (pitcher statistic).                       | Numeric           | -           | -           |
| IP                    | Innings pitched.                                | Numeric           | -           | -           |
| IPouts                | Outs recorded while pitching.                   | Numeric           | -           | -           |
| H                     | Hits allowed.                                   | Numeric           | -           | -           |
| ER                    | Earned runs.                                    | Numeric           | -           | -           |
| HR                    | Home runs allowed.                              | Numeric           | -           | -           |
| BB                    | Walks allowed.                                  | Numeric           | -           | -           |
| SO                    | Strikeouts (pitcher statistic).                 | Numeric           | -           | -           |
| BAOpp                 | Opponent batting average.                       | Numeric           | $\frac{H}{BFP}$           | -           |
| ERA                   | Earned run average.                             | Numeric           | $9 * \frac{ER}{IP}$          | -           |
| SO%                   | Strikeout percentage.                           | Numeric           | $\frac{SO}{BFP}$           | -           |
| BB%                   | Walk percentage.                                | Numeric           | $\frac{BB}{BFP}$           | -           |
| SO:BB                 | Strikeout-to-walk ratio.                        | Numeric           | $\frac{SO}{BB}$           | -           |
| IBB                   | Intentional walks.                              | Numeric           | -           | -           |
| WP                    | Wild pitches (pitcher statistic).               | Numeric           | -           | -           |
| HBP                   | Hit by pitch. (batters hit by pitcher)                                   | Numeric           | -           | -           |
| BK                    | Balks (pitcher statistic).                      | Numeric           | -           | -           |
| BFP                   | Batters faced by a pitcher.                     | Numeric           | -           | -           |
| GF                    | Games finished (pitcher statistic).             | Numeric           | -           | -           |
| R                     | Runs allowed.                                  | Numeric           | -           | -           |
| SH                    | Sacrifice hits allowed. (sacrifice bunts)                        | Numeric           | -           | -           |
| SF                    | Sacrifice flies allowed.                        | Numeric           | -           | -           |
| GIDP                  | Ground into double play allowed.                | Numeric           | -           | -           |
| InnOuts               | Innings pitched (outs recorded).                | Numeric           | -           | -           |
| PO                    | Putouts (fielding statistic).                    | Numeric           | -           | -           |
| A                     | Assists (fielding statistic).                    | Numeric           | -           | -           |
| E                     | Errors (fielding statistic).                     | Numeric           | -           | -           |
| DP                    | Double plays turned (fielding statistic).        | Numeric           | -           | -           |
| Pitching Triple Crown | Achievement of leading the league in wins, strikeouts, and ERA in the same season. (0/1/2/3)| - | - | - |
| 300_wins              | 300 or more career wins.                        | Binary (0/1)      | -           | -           |
| 3000_so               | 3000 or more career strikeouts.                | Binary (0/1)      | -           | -           |
| 300_sv               | 300 or more career saves.                      | Binary (0/1)      | -           | -           |
| ped_use               | Indicates whether the player has been linked to the use of performance-enhancing drugs (PEDs). | Binary (0/1) | - | - |
