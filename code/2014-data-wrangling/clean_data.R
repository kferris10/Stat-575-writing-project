
require(pitchRx)
require(compare)
require(dplyr)
setwd("E:/Kevin_Ferris")
# note, pitchRx masks pitches
load("raw_09fx.Rdata")

time3 <- Sys.time()
####### NOW with dplyr ##################
# setting up dplyr
pitches_df <- tbl_df(pit_df)
pitches_df$inter <- with(pit_df, interaction(gameday_link, inning, inning_side))

## removes all innings where a player cam up more than once
by_num <- pitches_df %.%
  group_by(gameday_link, inning, inning_side, num) %.%
  summarise(b_id = unique(batter))
bat_num_1 <- by_num %.%
  group_by(gameday_link, inning, inning_side, b_id) %.%
  summarise(nrow = n()) %.%
  filter(nrow > 1)
bat_num_1$inter <- with(bat_num_1, interaction(gameday_link, inning, inning_side))
remove_inns <- factor(unique(bat_num_1$inter))
# innings with errors where batter didn't hit twice
pit2 <- filter(pitches_df, !(inter %in% remove_inns))
pit2$rowID <- 1:nrow(pit2)
pit2_df <- tbl_df(pit2)

## think this is working, but should do a better check
## won't work if batter appears twice in the same inning
ab_info <- pit2_df %.% 
  select(batter, event, inning, num, gameday_link, pitcher, inning_side) %.%
  group_by(batter, inning_side, inning, gameday_link, num) %.%
  summarise(outcome = unique(event), resp_pit = last(pitcher))
on_1b <- select(filter(pit2_df, !is.na(on_1b)), on_1b, inning_side, inning, num, gameday_link)
on_2b <- select(filter(pit2_df, !is.na(on_2b)), on_2b, inning_side, inning, num, gameday_link)
on_3b <- select(filter(pit2_df, !is.na(on_3b)), on_3b, inning_side, inning, num, gameday_link)

# removing num since each batter only came to the plate once per inning
ab_info <- select(ab_info, -num)

## adding info for how 1st, 2nd, 3rd

# have to cheat since names must be matching for plyr
ab_info_1b <- ab_info; names(ab_info_1b)[1] <- "on_1b"
ab_info_2b <- ab_info; names(ab_info_2b)[1] <- "on_2b"
ab_info_3b <- ab_info; names(ab_info_3b)[1] <- "on_3b"


# getting info for each batter
info_1b <- inner_join(on_1b, ab_info_1b)
info_2b <- inner_join(on_2b, ab_info_2b)
info_3b <- inner_join(on_3b, ab_info_3b)

# reducing to just one obs per group for joining
info_1b_sum <- info_1b %.%
  group_by(on_1b, inning_side, inning, num, gameday_link) %.%
  summarise(how_1b = unique(outcome), 
            resp_pit_1b = unique(resp_pit))
info_2b_sum <- info_2b %.%
  group_by(on_2b, inning_side, inning, num, gameday_link) %.%
  summarise(how_2b = unique(outcome), 
            resp_pit_2b = unique(resp_pit))
info_3b_sum <- info_3b %.%
  group_by(on_3b, inning_side, inning, num, gameday_link) %.%
  summarise(how_3b = unique(outcome), 
            resp_pit_3b = unique(resp_pit))

# merging
pit_full2 <- left_join(pit2_df, info_1b_sum)
pit_full2 <- left_join(pit_full2, info_2b_sum)
pit_full2 <- left_join(pit_full2, info_3b_sum)

# selecting only rows where somebody is on base
pit_on <- filter(pit_full2, !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b))

# changing to numeric
pit_on$px <- as.numeric(pit_on$px)
pit_on$pz <- as.numeric(pit_on$pz)
pit_on$home_team_runs <- as.integer(pit_on$home_team_runs)
pit_on$away_team_runs <- as.integer(pit_on$away_team_runs)
pit_on$start_speed <- as.numeric(pit_on$start_speed)
pit_on$b <- as.integer(pit_on$b)
pit_on$s <- as.integer(pit_on$s)
pit_on$o <- as.integer(pit_on$o)

# interaction for pitcher/batter handedness
pit_on$pb_hand <- interaction(pit_on$stand, pit_on$p_throws)

# getting rid of NAs when nobody is on base
temp_1b <- pit_on$how_1b
temp_1b[is.na(temp_1b)] <- "none"
temp_2b <- pit_on$how_2b
temp_2b[is.na(temp_2b)] <- "none"
temp_3b <- pit_on$how_3b
temp_3b[is.na(temp_3b)] <- "none"
# problems - usually a pinch runner
bad <- select(filter(pit_on, temp_1b == "none" & temp_2b == "none" & temp_3b == "none" & (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b))), batter, pitcher, num, inning, on_1b, on_2b, on_3b, how_1b, how_2b, how_3b, gameday_link, inning_side)
bad$inter <- with(bad, interaction(gameday_link, inning, inning_side))


# how much pitcher's team is up
pit_on$p_diff <- with(pit_on, ifelse(inning_side == "top", 
                                     home_team_runs - away_team_runs, 
                                     away_team_runs - home_team_runs))

# whether or not the pitcher is ahead in the count
pit_on$p_ahead <- 
  with(pit_on, factor(ifelse(b > s, "behind", 
                             ifelse(b == s, "even", "ahead"))))

# distance to center
pit_on$dist <- with(pit_on, sqrt((px-0)^2 + (pz - 2.3)^2))

# zone - upper left/upper right/ lower left/ lower right
pit_on$zone <- factor(with(pit_on, 
                           ifelse(px < 0, 
                                  ifelse(pz < 2.5, "LL", "LR"), 
                                  ifelse(pz < 2.5, "UL", "UR"))))

# up/down and left/right
pit_on$height <- factor(ifelse(pit_on$pz > 2.3, "high", "low"))
pit_on$side <- factor(ifelse(pit_on$px < 0, "left", "right"))

# whether or not pitcher pitching put the guy on base
pit_is_resp_1b <- ifelse(pit_on$pitcher == pit_on$resp_pit_1b, "resp", "not")
pit_is_resp_1b[is.na(pit_is_resp_1b)] <- "none"
pit_is_resp_2b <- ifelse(pit_on$pitcher == pit_on$resp_pit_2b, "resp", "not")
pit_is_resp_2b[is.na(pit_is_resp_2b)] <- "none"
pit_is_resp_3b <- ifelse(pit_on$pitcher == pit_on$resp_pit_3b, "resp", "not")
pit_is_resp_3b[is.na(pit_is_resp_3b)] <- "none"


# whether or not pitcher is responsible for everyone on base
pit_on$resp_pit <- factor(
  with(pit_on, ifelse((how_1b == "Field Error" | pit_is_resp_1b != "resp") & 
                   (how_2b == "Field Error" | pit_is_resp_2b != "resp") & 
                   (how_3b == "Field Error" | pit_is_resp_3b != "resp"), 
                 "not", "resp")))

# whether or not the batter swung
pit_on$swing <- factor(
  ifelse(pit_on$des %in% c("Ball", "Ball In Dirt", "Called Strike", "Hit By Pitch", "Intent Ball", "Pitchout"), 
         "no", "yes"))

# remove innings with pinch runner
pit_on <- filter(pit_on, !(inter %in% bad$inter))

# adding year info
pit_on09 <- pit_on
pit_on09$year <- 2009
time4 <- Sys.time()

# save(pit_on09, file = "clean_on_09fx.Rdata")

