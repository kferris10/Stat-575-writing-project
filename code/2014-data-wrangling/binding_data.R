setwd("E:/Kevin_Ferris")
require(dplyr)
load("clean_on_09fx.Rdata")
load("clean_on_10fx.Rdata")
load("clean_on_11fx.Rdata")
load("clean_12_13fx.Rdata")
cbind(names(pit_on09), names(pit_on11))

pit <- select(pit_on, 1:11, s, b_height, batter, batter_name, inning_side, o, b, 
	away_team_runs, stand, p_throws, inning, pitcher_name, event, home_team_runs, 
	pitcher, inter, rowID, how_1b, resp_pit_1b, how_2b, resp_pit_2b, 
	resp_pit_3b, how_3b, 35:43)
pit11 <- select(pit_on11, 1:11, s, b_height, batter, batter_name, inning_side, o, b, 
	away_team_runs, stand, p_throws, inning, pitcher_name, event, home_team_runs, 
	pitcher, inter, rowID, how_1b, resp_pit_1b, how_2b, resp_pit_2b, 
	resp_pit_3b, how_3b, 35:43)
pit_on09$inter <- as.character(pit_on09$inter)
pit_on10$inter <- as.character(pit_on10$inter)
pit11$inter <- as.character(pit11$inter)
pit$inter <- as.character(pit$inter)
pit_on <- rbind_list(pit_on09, pit_on10, pit11, pit)
pit_on <- tbl_df(pit_on)


# save(pit_on, file = "clean_09_13fx.Rdata")


