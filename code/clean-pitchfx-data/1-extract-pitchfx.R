
# setup ----------------------------------------------
library(dplyr)

# pitchfx data (obtained from pitchRx package) are stored in a database
db <- src_sqlite("data/pitchfx14.sqlite3")

# pulling data from database ------------------------

# only using regular season games
game <- tbl(db, "game") %>% 
  filter(game_type == "R") %>% 
  select(gameday_link) %>% 
  collect() %>% 
  mutate(gameday_link = paste0("gid_", gameday_link))

# selecting pithc locations where we have data
pit_locs <- tbl(db, "pitch") %>% 
  filter(gameday_link %in% game$gameday_link, 
         !is.na(px), !is.na(pz)) %>% 
  select(id, px, pz, num, on_1b, on_2b, on_3b, gameday_link)

# selecting data pertinent to each atbat
ab_info <- tbl(db, "atbat") %>% 
  filter(gameday_link %in% game$gameday_link) %>% 
  select(gameday_link, num, inning, inning_side, pitcher, batter, event, 
         home_team_runs, away_team_runs, p_throws, stand)

# combining and saving ------------------------------

# combining into one data.frame
pit_dat <- inner_join(ab_info, pit_locs, by = c("gameday_link", "num")) %>% 
  arrange(gameday_link, num, id) %>% 
  select(gameday_link, num, id, inning, inning_side, pitcher, batter, home_team_runs, 
         event, px, pz, on_1b, on_2b, on_3b, p_throws, stand) %>% 
  collect()

# saving
save(pit_dat, file = "data/2014-pitchfx-data/extracted-pitchfx-data.RData")

