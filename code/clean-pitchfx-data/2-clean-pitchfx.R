
# packages
library(compare)
library(dplyr)

# load data
load("data/2014-pitchfx-data/extracted-pitchfx-data.RData")

# removing some data ---------------------------------------

# innings where the batter hit more than once
pit_clean <- pit_dat %>% 
  group_by(gameday_link, inning, inning_side, batter) %>% 
  mutate(b_trips = n_distinct(num)) %>% 
  group_by(gameday_link, inning, inning_side) %>% 
  mutate(batted_around = ifelse(2 %in% b_trips, T, F)) %>% 
  ungroup() %>% 
  filter(!batted_around) %>% 
  select(-b_trips, -batted_around)

# how runners got on base ----------------------------------
# this won't work if batters appear twice in an inning

# getting the batter, pitcher, and outcome for each at bat
ab_info <- pit_clean %>% 
  select(gameday_link, inning, inning_side, num, batter, pitcher, event) %>% 
  group_by(gameday_link, inning, inning_side, num, batter) %>% 
  summarise(outcome = unique(event), 
            resp_pit = last(pitcher)) %>% 
  ungroup() %>% 
  select(-num)

# determining how batters got on first base
on_1b <- pit_clean %>%
  filter(!is.na(on_1b)) %>% 
  select(gameday_link, inning, inning_side, num, on_1b) %>% 
  distinct()
how_1b <- ab_info %>% 
  rename(on_1b = batter) %>% 
  inner_join(on_1b) %>% 
  select(gameday_link, num, on_1b, outcome, resp_pit) %>% 
  distinct() %>% 
  rename(how_1b = outcome, 
         resp_pit_1b = resp_pit)
# determining how batters got on second base
on_2b <- pit_clean %>%
  filter(!is.na(on_2b)) %>% 
  select(gameday_link, inning, inning_side, num, on_2b) %>% 
  distinct()
how_2b <- ab_info %>% 
  rename(on_2b = batter) %>% 
  inner_join(on_2b) %>% 
  select(gameday_link, num, on_2b, outcome, resp_pit) %>% 
  distinct() %>% 
  rename(how_2b = outcome, 
         resp_pit_2b = resp_pit)
# determining how batters got on third base
on_3b <- pit_clean %>%
  filter(!is.na(on_3b)) %>% 
  select(gameday_link, inning, inning_side, num, on_3b) %>% 
  distinct()
how_3b <- ab_info %>% 
  rename(on_3b = batter) %>% 
  inner_join(on_3b) %>% 
  select(gameday_link, num, on_3b, outcome, resp_pit) %>% 
  distinct() %>% 
  rename(how_3b = outcome, 
         resp_pit_3b = resp_pit)

pit_on <- pit_clean %>% 
  left_join(how_1b) %>% 
  left_join(how_2b) %>% 
  left_join(how_3b) %>% 
  filter(!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b))  %>% 
  mutate_each(funs(on = ifelse(is.na(.), "none", .)), on_1b, on_2b, on_3b) %>% 
  mutate(
    is_resp_1b = 
      ifelse(pitcher == resp_pit_1b | how_1b == "Field Error", "resp", "not"), 
    is_resp_2b = 
      ifelse(pitcher == resp_pit_2b | how_2b == "Field Error", "resp", "not"), 
    is_resp_3b = 
      ifelse(pitcher == resp_pit_3b | how_3b == "Field Error", "resp", "not")
  )

# check - no idea how this could happen
pit_on %>% filter(on_1b == "none", on_2b == "none", on_3b == "none")

# determining whether the pitcher is responsible for each pitch
resp_dat <- pit_on %>% 
  mutate_each(funs(x = ifelse(is.na(.), "none", .)), 
              is_resp_1b, is_resp_2b, is_resp_3b) %>% 
  mutate(
    resp_pit = ifelse(is_resp_1b != "resp" & 
                        is_resp_2b != "resp" &  
                        is_resp_3b != "resp", 
                      "not", 
                      "resp")
  ) %>% 
  select(gameday_link, num, p_throws, stand, id, resp_pit, px, pz)

# selecting only left vs left
lvl_resp <- resp_dat %>% filter(p_throws == "L", stand == "L")

# saving ---------------------------------------------------------

# save all
save(resp_dat, file = "data/2014-pitchfx-data/resp-pitchers.RData")

# save left vs left
save(lvl_resp, file = "data/2014-pitchfx-data/lvl-resp.RData")





