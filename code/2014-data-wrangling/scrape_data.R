setwd("e:/Kevin_Ferris")
require(pitchRx)
library(dplyr)
db <- src_sqlite("regseason09.sqlite3", create=TRUE)
tim1 <- Sys.time()
scrape(start="2009-04-05", end="2010-01-01", connect=db$con)
# db <- src_sqlite("regseason13.sqlite3")


locations2 <- select(tbl(db, "pitch"), des, start_speed, px, pz, num, zone, on_1b, on_2b, on_3b, count, gameday_link)
names2 <- select(tbl(db, "atbat"), pitcher, pitcher_name, batter, batter_name, 
	num, b, s, o, stand, b_height, p_throws, event, 
	home_team_runs, away_team_runs, inning_side, inning, gameday_link)
pitchfx2 <- collect(locations2)
pitchfx3 <- collect(names2)
pitch_full <- inner_join(pitchfx2, pitchfx3, by = c("num", "gameday_link"))
pit_df <- arrange(pitch_full, gameday_link, num, count)

save(pit_df, file = "raw_09fx.Rdata")
tim2 <- Sys.time()
tim2-tim1
rm(list=ls())
q("no")

