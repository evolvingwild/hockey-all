#####################################################################################
#####     Adjusted Penalty Differential    ||             07/10/17              #####
#####################################################################################

library(tidyr); library(dplyr)

options(scipen = 999)

# Load
pbp_1718x <- readRDS("pbp_1718x.rds")
player_position <- readRDS("skater_position.rds")
scoring_rates <- readRDS("scoring_rates.rds")
pen_score_adj <- data.frame(readRDS("penalty_adj_state.RDS"))

## Modify
pbp_1718x <- pbp_1718x %>% 
  mutate(scradj = home_score - away_score, 
         home_lead = ifelse(scradj >= 3, 3, 
                            ifelse(scradj <= -3, -3, scradj)),
         home_lead_state = ifelse(home_lead < 0, 1, 
                                  ifelse(home_lead == 0, 2, 
                                         ifelse(home_lead > 0, 3, home_lead))), 
         home_lead = home_lead + 4, 
         event_length = ifelse(is.na(event_length), 0, event_length))

fun.pen_GF <- function() {
  
  penrate_GF <- data.frame(matrix(ncol = 10, nrow = 9))
  penrate_GF$X1 <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(penrate_GF) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  penrate_GF[1, ] <- c("5v5", 0, 0, 0, scoring_rates[4, 4] - scoring_rates[1, 4], 0, 0, 0, 0, 0)
  penrate_GF[2, ] <- c("4v4", 0, 0, 0, 0, 0, scoring_rates[6, 4] - scoring_rates[2, 4], 0, 0, 0)                      
  penrate_GF[3, ] <- c("3v3", 0, 0, 0, 0, 0, scoring_rates[6, 4] - scoring_rates[3, 4], 0, 0, 0) 
  penrate_GF[4, ] <- c("5v4", 0, scoring_rates[2, 4] - scoring_rates[4, 4], 0, 0, scoring_rates[5, 4] - scoring_rates[4, 4], 0, 0, 0, 0) 
  penrate_GF[5, ] <- c("5v3", 0, 0, 0, 0, 0, scoring_rates[6, 4] - scoring_rates[5, 4], 0, 0, 0) 
  penrate_GF[6, ] <- c("4v3", 0, 0, scoring_rates[3, 4] - scoring_rates[6, 4], 0, 0, 0, 0, 0, 0) 
  penrate_GF[7, ] <- c("6v5", 0, 0, 0, scoring_rates[4, 4] - scoring_rates[7, 4], 0, 0, 0, scoring_rates[8, 4] - scoring_rates[7, 4], 0)
  penrate_GF[8, ] <- c("6v4", 0, scoring_rates[2, 4] - scoring_rates[8, 4], 0, 0, 0, 0, 0, 0, scoring_rates[9, 4] - scoring_rates[8, 4])
  penrate_GF[9, ] <- c("6v3", 0, 0, 0, 0, 0, scoring_rates[6, 4] - scoring_rates[9, 4], 0, 0, 0)
  
  df <- data.frame(sapply(penrate_GF, function(x) as.numeric(x)))
  df$x <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(df) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  return(df)
} 
penrate_GF <- fun.pen_GF()

fun.pen_GA <- function() {
  
  penrate_GA <- data.frame(matrix(ncol = 10, nrow = 9))
  penrate_GA$X1 <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(penrate_GA) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  penrate_GA[1, ] <- c("5v5", 0, 0, 0, scoring_rates[4, 6] - scoring_rates[1, 6], 0, 0, 0, 0, 0)
  penrate_GA[2, ] <- c("4v4", 0, 0, 0, 0, 0, scoring_rates[6, 6] - scoring_rates[2, 6], 0, 0, 0)                      
  penrate_GA[3, ] <- c("3v3", 0, 0, 0, 0, 0, scoring_rates[6, 6] - scoring_rates[3, 6], 0, 0, 0) 
  penrate_GA[4, ] <- c("5v4", 0, scoring_rates[2, 6] - scoring_rates[4, 6], 0, 0, scoring_rates[5, 6] - scoring_rates[4, 6], 0, 0, 0, 0) 
  penrate_GA[5, ] <- c("5v3", 0, 0, 0, 0, 0, scoring_rates[6, 6] - scoring_rates[5, 6], 0, 0, 0) 
  penrate_GA[6, ] <- c("4v3", 0, 0, scoring_rates[3, 6] - scoring_rates[6, 6], 0, 0, 0, 0, 0, 0) 
  penrate_GA[7, ] <- c("6v5",  0, 0, 0, scoring_rates[4, 6] - scoring_rates[7, 6], 0, 0, 0, scoring_rates[8, 6] - scoring_rates[7, 6], 0)
  penrate_GA[8, ] <- c("6v4", 0, scoring_rates[2, 6] - scoring_rates[8, 6], 0, 0, 0, 0, 0, 0, scoring_rates[9, 6] - scoring_rates[8, 6])
  penrate_GA[9, ] <- c("6v3", 0, 0, 0, 0, 0, scoring_rates[6, 6] - scoring_rates[9, 6], 0, 0, 0)
  
  df <- data.frame(sapply(penrate_GA, function(x) as.numeric(x)))
  df$x <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(df) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  
  return(df)
}
penrate_GA <- fun.pen_GA()




## -------------------- ##
##        Compute       ##
## -------------------- ##

# Wrangle & Sort Data
fun.pen_setup <- function(data) {
  
  # Determine next strength state
  
  #pbp_part <- data %>% 
  #  filter(event_type %in% c("PENL", "FAC"), 
  #         !(grepl("misconduct", tolower(event_description))), 
  #         !(grepl("fighting", tolower(event_description)))
  #         ) %>% 
  #  #group_by(season, game_id) %>% 
  #  mutate(next_st_state1 = ifelse(event_type == "PENL" & lead(event_type) == "FAC" & game_id == lead(game_id), lead(game_strength_state), 0)) %>% 
  #  select(game_id:game_period, game_strength_state, game_seconds:event_detail, next_st_state1) %>% 
  #  filter(event_type == "PENL") %>% 
  #  mutate(last_pen_time = ifelse(lag(game_seconds) != game_seconds & lag(game_id) == game_id, lag(game_seconds), 0)) %>%
  #  select(game_id, event_index, last_pen_time, next_st_state1)
  
  pbp_part <- data %>% 
    filter(event_type %in% c("PENL", "FAC"), 
           !(grepl("misconduct", tolower(event_description))), 
           !(grepl("fighting", tolower(event_description)))
           ) %>% 
    group_by(game_id) %>% 
    mutate(next_st_state1 = ifelse(event_type == "PENL" & lead(event_type) == "FAC", lead(game_strength_state), 0)) %>% 
    filter(event_type == "PENL") %>% 
    mutate(last_pen_time = ifelse(lag(game_seconds) != game_seconds, lag(game_seconds), 0)) %>%
    select(game_id, event_index, last_pen_time, next_st_state1) %>% 
    data.frame()
  
  pbp_part[is.na(pbp_part)] <- 0
  
  # Join next strength state
  pen_add <- left_join(data, pbp_part, by = c("game_id", "event_index"))
  
  # Identify features
  pen_source <- pen_add %>% 
    filter(event_type %in% c("PENL"), 
           !(grepl("fighting", tolower(event_description))), 
           !(grepl("misconduct", tolower(event_description)))
           ) %>% 
    mutate(# probably a better way to do this
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
           last_pen_time = ifelse(last_pen_time > 0 & lag(game_id) != game_id, 0, last_pen_time),
           
           cluster = ifelse(game_seconds == lead(game_seconds) & game_id == lead(game_id), 1, 0), 
           cluster = ifelse(cluster == 0 & lag(cluster) == 1, 1, cluster), 
           
           pen_shot = ifelse((grepl("PS-", event_description) == TRUE) == 1, 1, 0), 
           major = ifelse((grepl("maj", tolower(event_description)) == TRUE) == 1, 1, 0), 
           double = ifelse((grepl("double minor", tolower(event_description)) == TRUE) == 1, 1, 0), 
           team_pen = ifelse((grepl("bench", tolower(event_description)) == TRUE) == 1, 1, 0), 
           
           # probably a better way to do this
           next_st_state2  = ifelse(next_st_state1  == 0 & game_seconds == lead(game_seconds), lead(next_st_state1),  next_st_state1), 
           next_st_state3  = ifelse(next_st_state2  == 0 & game_seconds == lead(game_seconds), lead(next_st_state2),  next_st_state2), 
           next_st_state4  = ifelse(next_st_state3  == 0 & game_seconds == lead(game_seconds), lead(next_st_state3),  next_st_state3), 
           next_st_state5  = ifelse(next_st_state4  == 0 & game_seconds == lead(game_seconds), lead(next_st_state4),  next_st_state4), 
           next_st_state6  = ifelse(next_st_state5  == 0 & game_seconds == lead(game_seconds), lead(next_st_state5),  next_st_state5), 
           next_st_state7  = ifelse(next_st_state6  == 0 & game_seconds == lead(game_seconds), lead(next_st_state6),  next_st_state6), 
           next_st_state8  = ifelse(next_st_state7  == 0 & game_seconds == lead(game_seconds), lead(next_st_state7),  next_st_state7), 
           next_st_state9  = ifelse(next_st_state8  == 0 & game_seconds == lead(game_seconds), lead(next_st_state8),  next_st_state8), 
           next_st_state10 = ifelse(next_st_state9  == 0 & game_seconds == lead(game_seconds), lead(next_st_state9),  next_st_state9), 
           next_st_state11 = ifelse(next_st_state10 == 0 & game_seconds == lead(game_seconds), lead(next_st_state10), next_st_state10), 
           next_st_state12 = ifelse(next_st_state11 == 0 & game_seconds == lead(game_seconds), lead(next_st_state11), next_st_state11), 
           next_st_state13 = ifelse(next_st_state12 == 0 & game_seconds == lead(game_seconds), lead(next_st_state12), next_st_state12), 
           next_st_state14 = ifelse(next_st_state13 == 0 & game_seconds == lead(game_seconds), lead(next_st_state13), next_st_state13), 
           next_st_state15 = ifelse(next_st_state14 == 0 & game_seconds == lead(game_seconds), lead(next_st_state14), next_st_state14), 
           
           no_impact = ifelse((game_strength_state %in% c("5v5", "4v4", "3v3") & 
                                 next_st_state15 %in% c("5v5", "4v4", "3v3")) | 
                                (game_strength_state == next_st_state15 & 
                                   (game_strength_state != "5v3" & game_strength_state != "3v5" & 
                                      game_strength_state != "Ev3" & game_strength_state != "3vE")) |
                                (grepl("match", tolower(event_description)) == TRUE) == 1 | 
                                pen_shot == 1 | 
                                major == 1 | 
                                next_st_state15 == 0, 
                              1, 0), 
           
           # Correct simultaneous minors
           cluster = ifelse(cluster == 1 & 
                              (((game_strength_state == "5v5" & next_st_state15 %in% c("5v3", "3v5")) | 
                                 (game_strength_state == "5vE" & next_st_state15 == "3v5") | 
                                 (game_strength_state == "Ev5" & next_st_state15 == "5v3")) | 
                                 
                                 (game_strength_state == "5v4" & next_st_state15 == "3v4") | 
                                 (game_strength_state == "4v5" & next_st_state15 == "4v3")), 
                            0, cluster)
           ) %>% 
    select(game_id, event_index, season, game_date, event_type, event_description, 
           event_detail, event_team, event_player_1, event_player_2, home_team, 
           away_team, home_lead, home_lead_state, game_period, game_seconds, 
           last_pen_time, game_strength_state, next_st_state15, pen_shot, 
           major, double, team_pen, no_impact, cluster
           ) %>%  
    rename(next_st_state = next_st_state15) %>% 
    data.frame()
  
  return(pen_source)
}
pen_source <- fun.pen_setup(pbp_1718x)

# Assignments
fun.pen_assign <- function(data) {
  
  # Initial gather
  test1 <- data %>% 
    filter(no_impact == 0, cluster == 1) %>% 
    mutate(pen_id = round((as.numeric(game_id) * as.numeric(game_seconds)) / 10000000, 2)) %>% 
    select(game_id, event_index, pen_id, event_player_1, event_player_2)
  
  test1a <- test1 %>% 
    select(-c(game_id, event_index, event_player_2)) %>% 
    gather(Column, Value, -pen_id, na.rm = TRUE) %>%
    count(pen_id, Value)  %>% 
    rename(take = Value)
  
  test1b <- test1 %>% 
    select(-c(game_id, event_index, event_player_1)) %>% 
    gather(Column, Value, -pen_id, na.rm = TRUE) %>%
    count(pen_id, Value)  %>% 
    rename(draw = Value)
  
  test1c <- full_join(test1a, test1b, by = c("pen_id"))
  
  # Determine order of penalties taken / drawn
  test2a <- test1c %>% 
    select(pen_id, take, n.x) %>% 
    group_by(pen_id, take) %>% 
    summarise(take_n = as.numeric(sum(n.x))) %>% 
    rename(player = take)
  
  test2b <- test1c %>%
    select(pen_id, draw, n.y) %>% 
    group_by(pen_id, draw) %>% 
    summarise(draw_n = as.numeric(sum(n.y))) %>% 
    rename(player = draw)
  
  test2c <- test2a %>% 
    full_join(., test2b, by = c("pen_id", "player")) %>% 
    filter(!is.na(player)) %>% 
    mutate(take_n = ifelse(is.na(take_n), 0, take_n), 
           draw_n = ifelse(is.na(draw_n), 0, draw_n), 
           diff = draw_n - take_n
           ) %>% 
    arrange(pen_id, desc(diff)) %>% 
    mutate(offset1 = ifelse(pen_id == lag(pen_id) & take_n == lag(take_n) & draw_n == lag(draw_n) & diff == 0 & lag(diff) == 0, 1, 0), 
           offset1 = ifelse(is.na(offset1) & lead(offset1) == 1 & pen_id == lead(pen_id), 1, offset1), 
           offset2 = ifelse(lead(offset1) == 1 & offset1 == 0 & pen_id == lead(pen_id), 1, offset1), 
           offset3 = ifelse(is.na(offset2) & offset1 == 0, 0, offset2)
           ) %>% 
    select(-c(offset1, offset2)) %>% 
    mutate(diff_fix = ifelse(is.na(offset3) & diff == 0 & lead(diff) < diff, -lead(diff), diff), 
           offset3 = ifelse(is.na(offset3), 0, offset3)
           ) %>% 
    select(pen_id:draw_n, diff_fix, offset3) %>% 
    rename(offset = offset3)
  
  # Remove further offsetting penalties
  test2d <- test2c %>% 
    filter(offset == 1) %>% 
    select(pen_id, offset) %>% group_by(pen_id) %>% 
    summarise(offset = first(offset))  
  
  # Assign
  test3 <- test2c %>% 
    filter(offset != 1) %>% 
    select(-c(offset))
  
  test3a <- test3 %>% 
    group_by(pen_id) %>% 
    arrange(pen_id, desc(diff_fix)) %>%
    slice(c(1, n())) %>%
    mutate(Type = c("most", "least")) %>% 
    select(-c(take_n, draw_n, diff_fix)) %>%
    spread(Type, player) %>% 
    rename(take_assign = least, 
           draw_assign = most)
  
  # Correct strange double minors
  test4 <- test1 %>% 
    left_join(., test3a, by = c("pen_id")) %>% 
    group_by(pen_id) %>% 
    mutate(n = n(),  
           test = ifelse(lag(event_player_1) == event_player_2 & is.na(lag(event_player_2)), 1, 0), 
           test = ifelse(is.na(test) & lead(test) == 1, 1, test), 
           take_update = ifelse((n == 2 & lead(test == 1) & test == 1) | 
                                  (n == 2 & test == 1 & test == 1), draw_assign, take_assign), 
           draw_update = ifelse((n == 2 & lead(test == 1) & test == 1) | 
                                  (n == 2 & test == 1 & test == 1), take_assign, draw_assign), 
           take_update = ifelse(is.na(take_update), take_assign, take_update), 
           draw_update = ifelse(is.na(draw_update), draw_assign, draw_update), 
           offset = ifelse(is.na(take_update) & is.na(draw_update), 1, 0)
           ) %>% 
    arrange(game_id, event_index) %>% 
    mutate(first_assign = as.numeric(row_number()), 
           first_assign = ifelse(offset == 1, 0, first_assign)
           ) %>% 
    ungroup() %>% 
    select(-c(take_assign, draw_assign, n, test)) %>% 
    rename(take_assign = take_update, 
           draw_assign = draw_update
           ) %>% 
    select(game_id, event_index, offset, first_assign, take_assign, draw_assign)
  
  # Join Data
  pen_enhanced <- data %>% 
    left_join(. , test4, by = c("game_id", "event_index")) %>% 
    mutate(offset = ifelse(is.na(offset), 0, offset), 
           first_assign = ifelse(is.na(first_assign), 0, first_assign), 
           take_assign = ifelse(is.na(take_assign), 0, take_assign), 
           draw_assign = ifelse(is.na(draw_assign), 0, draw_assign), 
           no_impact2 = ifelse(no_impact == 1 | offset == 1 | first_assign > 1, 1, 0), 
           cluster = ifelse(is.na(cluster), 0, cluster), 
           last_pen_time = ifelse(is.na(last_pen_time), 0, last_pen_time)
           ) %>% 
    select(game_id:double, team_pen, no_impact2, cluster:draw_assign) %>% 
    rename(no_impact = no_impact2) %>% 
    data.frame()
  
  pen_enhanced$season <- as.numeric(pen_enhanced$season)
  
  return(pen_enhanced)
}
pen_enhanced <- fun.pen_assign(pen_source)

# Calculate goal value - MAIN
fun.pen_value_main <- function(pen_data, pbp_data) {
  
  # Remove Duplicates (2007 & 2009)
  pen_calc_main <- pen_data %>% 
    filter(no_impact != 1) %>% 
    mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
           event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
           duplicate = ifelse(game_id == lag(game_id) & 
                                event_player_1 == lag(event_player_1) & 
                                event_player_2 == lag(event_player_2) & 
                                event_description == lag(event_description) & 
                                home_team == lag(home_team) & 
                                away_team == lag(away_team) & 
                                event_team == lag(event_team) & 
                                game_period == lag(game_period) & 
                                game_strength_state == lag(game_strength_state), 1, 0), 
           duplicate = ifelse(is.na(duplicate), 0, duplicate)
           ) %>% 
    filter(duplicate == 0) %>% 
    select(-c(duplicate)) %>% 
    mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
           event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2)) %>% 
    
    # Calculate Goal Values for Main and Assigned penalties
    mutate(time_diff = game_seconds - last_pen_time, 
           
           # Venue Unspecific
           EV_5v5_5v4 = ifelse(game_strength_state %in% c("5v5", "Ev5", "5vE") & next_st_state %in% c("5v4", "4v5", "Ev4", "4vE"), 
                               ifelse(double == 1 & first_assign == 0, 
                                      ((penrate_GF[1,5] - penrate_GA[1,5]) * 240) / 3600, 
                                      ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                               0), 
           
           EV_4v4_4v3 = ifelse(game_strength_state %in% c("4v4", "Ev4", "4vE") & next_st_state %in% c("4v3", "3v4"),
                               ifelse(game_period == 4, 
                                      
                                      ifelse(season >= 20152016, 
                                             ((penrate_GF[3,7] - penrate_GA[3,7]) * (ifelse(3900 - game_seconds > 120, 120, 3900 - game_seconds))) / 3600, 
                                             ((penrate_GF[2,7] - penrate_GA[2,7]) * (ifelse(3900 - game_seconds > 120, 120, 3900 - game_seconds))) / 3600), 
                                      
                                      ((penrate_GF[2,7] - penrate_GA[2,7]) * (120 - (ifelse(time_diff > 120 | time_diff < 0, 60, time_diff))) / 3600) + 
                                        (((penrate_GF[1,5] - penrate_GA[1,5]) * (ifelse(time_diff > 120 | time_diff < 0, 60, time_diff))) / 3600)), 
                               0), 
           
           EV_3v3_4v3 = ifelse(game_strength_state %in% c("3v3", "Ev3", "3vE") & next_st_state %in% c("3v4", "4v3"), 
                               ((penrate_GF[3,7] - penrate_GA[3,7]) * (ifelse(3900 - game_seconds > 120, 120, (3900 - game_seconds)))) / 3600, 
                               0), 
           
           EV_5v5_5v3 = ifelse((game_strength_state %in% c("5v5", "Ev5", "5vE") & next_st_state %in% c("5v3", "3v5")), 
                               ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                               0), 
           
           # Identify Home/Away Powerplay (home = 1, away = 2)
           home_PP1_away_PP2 = ifelse(game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3"), 1, 
                                      ifelse(game_strength_state %in% c("5v3", "Ev3") & next_st_state %in% c("5v3", "Ev3"), 1, 
                                             ifelse((game_strength_state %in% c("5v4", "Ev4") & next_st_state =="4v4") | 
                                                      (game_strength_state == "5v4" & next_st_state == "4vE") | 
                                                      (game_strength_state == "5vE" & next_st_state == "4v4"), 1, 
                                                    ifelse((game_strength_state == "5v3" & next_st_state == "4v3"), 1, 
                                                           ifelse((game_strength_state == "5v4" & next_st_state == "3v4"), 1, 
                                                                  ifelse(game_strength_state %in% c("4v3") & next_st_state == "3v3", 1, 
                                                                         
                                                                         ifelse(game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE"), 2, 
                                                                                ifelse(game_strength_state %in% c("3v5", "3vE") & next_st_state %in% c("3v5", "3vE"), 2, 
                                                                                       ifelse((game_strength_state %in% c("4v5", "4vE") & next_st_state == "4v4") | 
                                                                                                (game_strength_state == "4v5" & next_st_state == "Ev4") | 
                                                                                                (game_strength_state == "Ev5" & next_st_state == "4v4"), 2, 
                                                                                              ifelse((game_strength_state == "3v5" & next_st_state == "3v4"), 2, 
                                                                                                     ifelse((game_strength_state == "4v5" & next_st_state == "4v3"), 2, 
                                                                                                            ifelse(game_strength_state %in% c("3v4") & next_st_state == "3v3", 2, 
                                                                                                                   0)))))))))))
                                      ), 
           
           # Separate to Home/Away Powerplay
           PP_5v4_5v3_h = ifelse(game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3"),
                                 ifelse(time_diff < 120, 
                                        ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                          ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                        ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                                 0), 
           PP_4v5_3v5_a = ifelse(game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE"),
                                 ifelse(time_diff < 120, 
                                        ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                          ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                        ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                                 0),  
           
           PP_5v3_5v3_h = ifelse(game_strength_state %in% c("5v3", "Ev3") & next_st_state %in% c("5v3", "Ev3"), 
                                 ((penrate_GF[4,6] - penrate_GA[4,6]) * 120) / 3600, 
                                 0), 
           PP_5v3_5v3_a = ifelse(game_strength_state %in% c("3v5", "3vE") & next_st_state %in% c("3v5", "3vE"), 
                                 ((penrate_GF[4,6] - penrate_GA[4,6]) * 120) / 3600, 
                                 0), 
           
           PP_5v4_4v4_h = ifelse((game_strength_state %in% c("5v4", "Ev4") & next_st_state =="4v4") | 
                                   (game_strength_state == "5v4" & next_st_state == "4vE") | 
                                   (game_strength_state == "5vE" & next_st_state == "4v4"), 
                                 ((penrate_GA[4,3] - penrate_GF[4,3]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                 0), 
           PP_4v5_4v4_a = ifelse((game_strength_state %in% c("4v5", "4vE") & next_st_state == "4v4") | 
                                   (game_strength_state == "4v5" & next_st_state == "Ev4") | 
                                   (game_strength_state == "Ev5" & next_st_state == "4v4"), 
                                 ((penrate_GA[4,3] - penrate_GF[4,3]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                 0), 
           
           PP_5v3_4v3_h = ifelse((game_strength_state == "5v3" & next_st_state == "4v3"), 
                                 ((penrate_GA[5,7] - penrate_GF[5,7]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GA[4,3] - penrate_GF[4,3]) * time_diff) / 3600, 
                                 0), 
           PP_3v5_3v4_a = ifelse((game_strength_state == "3v5" & next_st_state == "3v4"), 
                                 ((penrate_GA[5,7] - penrate_GF[5,7]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GA[4,3] - penrate_GF[4,3]) * time_diff) / 3600, 
                                 0), 
           
           PP_5v4_3v4_h = ifelse((game_strength_state == "5v4" & next_st_state == "3v4"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                                 0), 
           PP_4v5_4v3_a = ifelse((game_strength_state == "4v5" & next_st_state == "4v3"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                                 0), 
           
           PP_4v3_3v3_h = ifelse(game_strength_state %in% c("4v3") & next_st_state == "3v3", 
                                 ((penrate_GA[6,4] - penrate_GF[6,4]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[3,7] - penrate_GA[3,7]) * time_diff) / 3600, 
                                 0), 
           PP_3v4_3v3_a = ifelse(game_strength_state %in% c("3v4") & next_st_state == "3v3", 
                                 ((penrate_GA[6,4] - penrate_GF[6,4]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[3,7] - penrate_GA[3,7]) * time_diff) / 3600, 
                                 0), 
           
           # Sum
           sub_sum = EV_5v5_5v4 + EV_4v4_4v3 + 
                     EV_3v3_4v3 + EV_5v5_5v3 + 
                     
                     PP_5v4_5v3_h + PP_4v5_3v5_a + 
                     PP_5v3_5v3_h + PP_5v3_5v3_a + 
                     PP_5v4_4v4_h + PP_4v5_4v4_a +  
                     PP_5v3_4v3_h + PP_3v5_3v4_a + 
                     PP_5v4_3v4_h + PP_4v5_4v3_a +
                     PP_4v3_3v3_h + PP_3v4_3v3_a, 
           
           unk_pen = ifelse(sub_sum == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           pen_value = sub_sum + unk_pen)
  
  # Verify Assigned Penalties
  pen_calc_verify <- pen_data %>% 
    mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
           event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
           duplicate = ifelse(game_id == lag(game_id) & 
                                event_player_1 == lag(event_player_1) & 
                                event_player_2 == lag(event_player_2) & 
                                event_description == lag(event_description) & 
                                home_team == lag(home_team) & 
                                away_team == lag(away_team) & 
                                event_team == lag(event_team) & 
                                game_period == lag(game_period) & 
                                game_strength_state == lag(game_strength_state), 1, 0), 
           duplicate = ifelse(is.na(duplicate), 0, duplicate)
           ) %>% 
    filter(duplicate == 0, first_assign > 0) %>% 
    select(-c(duplicate)) %>% 
    mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
           event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2), 
           verified1 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 2) == event_player_1 & 
                                   lead(event_player_1, 2) == event_player_2), 
                              1, 0), 
           verified1 = ifelse(is.na(verified1), 0, verified1), 
           verified2 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 1) == event_player_1 & 
                                   lead(event_player_1, 1) == event_player_2), 
                              1, 0), 
           verified2 = ifelse(is.na(verified2), 0, verified2), 
           verified3 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 1) == lead(event_player_1, 2) & 
                                   lead(event_player_1, 1) == lead(event_player_2, 2)), 
                              1, 0), 
           verified3 = ifelse(is.na(verified3), 0, verified3), 
           assign_verify = verified1 + verified2 + verified3
           ) %>% 
    select(game_id, event_index, assign_verify)
  
  # Join verified
  pen_calc_main <- pen_calc_main %>% 
    left_join(., pen_calc_verify, by = c("game_id", "event_index")) %>% 
    mutate(assign_verify = ifelse(is.na(assign_verify), 0, assign_verify)) %>% 
    select(game_id:draw_assign, assign_verify, time_diff:pen_value)
  
  # Correct "Assigned" penalties (remove wrongly assigned)
  pen_calc_assign <- pen_calc_main %>% 
    filter(first_assign == 1) %>% 
    select(game_id, event_index, take_assign, draw_assign)
  
  pbp_players <- pbp_data %>% 
    filter(event_type == "PENL") %>% 
    select(game_id, event_index, home_on_1:away_on_6)
  
  pbp_players_join <- pen_calc_assign %>% 
    right_join(pbp_players, ., by = c("game_id", "event_index"))
  
  pbp_players_join[is.na(pbp_players_join)] <- 0
  
  pbp_players_join <- pbp_players_join %>% 
    mutate(no_impact2 = ifelse(((take_assign == home_on_1 | take_assign == home_on_2 | take_assign == home_on_3 | 
                                   take_assign == home_on_4 | take_assign == home_on_5 | take_assign == home_on_6) & 
                                  (draw_assign == home_on_1 | draw_assign == home_on_2 | draw_assign == home_on_3 | 
                                     draw_assign == home_on_4 | draw_assign == home_on_5 | draw_assign == home_on_6)) | 
                                 
                                 ((take_assign == away_on_1 | take_assign == away_on_2 | take_assign == away_on_3 | 
                                     take_assign == away_on_4 | take_assign == away_on_5 | take_assign == away_on_6) & 
                                    (draw_assign == away_on_1 | draw_assign == away_on_2 | draw_assign == away_on_3 | 
                                       draw_assign == away_on_4 | draw_assign == away_on_5 | draw_assign == away_on_6)), 
                               1, 0)
           ) %>% 
    select(-c(home_on_1:away_on_6, take_assign, draw_assign))
  
  pen_calc_main <- pen_calc_main %>% 
    left_join(., pbp_players_join, by = c("game_id", "event_index")) %>% 
    mutate(no_impact2 = ifelse(is.na(no_impact2), 0, no_impact2),  
           no_impact3 = no_impact + no_impact2
           ) %>% 
    filter(no_impact3 != 1) %>% 
    select(-c(no_impact2, no_impact3)) %>% 
    data.frame()
  
  return(pen_calc_main)
}
pen_calc_main <- fun.pen_value_main(pen_enhanced, pbp_1718x)

# Calculate goal value - Extras
fun.pen_value_xtra <- function(data) {
  
  # Calculate Penalty Shot + Major Goal Values
  pen_calc_xtras <- data %>% filter(pen_shot == 1 | major == 1) %>% 
    mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
           event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
           duplicate = ifelse(game_id == lag(game_id) & 
                                event_player_1 == lag(event_player_1) & 
                                event_player_2 == lag(event_player_2) & 
                                event_description == lag(event_description) & 
                                home_team == lag(home_team) & 
                                away_team == lag(away_team) & 
                                event_team == lag(event_team) & 
                                game_period == lag(game_period) & 
                                game_strength_state == lag(game_strength_state), 1, 0), 
           duplicate = ifelse(is.na(duplicate), 0, duplicate), 
           
           # Re-do no impact for xtras
           no_impact = ifelse((game_strength_state %in% c("5v5", "4v4", "3v3") & next_st_state %in% c("5v5", "4v4", "3v3")) | 
                                (grepl("match", tolower(event_description)) == TRUE) == 1 | 
                                (game_strength_state == next_st_state & 
                                   (game_strength_state != "5v3" & game_strength_state != "3v5" & 
                                      game_strength_state != "Ev3" & game_strength_state != "3vE")), 
                              1, 0), 
           no_impact = ifelse(pen_shot == 1, 0, no_impact)
           ) %>% 
    filter(duplicate == 0) %>% 
    select(-c(duplicate)) %>% 
    mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
           event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2)) %>%
    
    mutate(time_diff = game_seconds - last_pen_time, 
           
           # Penalty Shots
           PS_EV_take = ifelse(pen_shot == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE"), 
                          ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           
           PS_PP_take = ifelse(pen_shot == 1 & game_strength_state %in% c("5v4", "4v5", "Ev4", "4vE", "5v3", "3v5", "4v3", "3v4"), 
                          ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           
           PS_EV_draw = ifelse(pen_shot == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE"), 
                          .3194, 0), 
           
           PS_PP_draw = ifelse(pen_shot == 1 & game_strength_state %in% c("5v4", "4v5", "Ev4", "4vE", "5v3", "3v5", "4v3", "3v4"), 
                          .3194, 0), 
      
           # Majors - Even Strength
           maj_EV_take = ifelse(major == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE") & next_st_state != 0, 
                                ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600, 0), 
           
           maj_EV_draw = ifelse(major == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE") & next_st_state != 0, 
                                ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           
           # Majors - Powerplay
           maj_PP_take_h = ifelse(major == 1 & (game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3")),
                                  ifelse(time_diff < 120, 
                                         ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                           ((penrate_GF[1,5] - penrate_GA[1,5]) * (300 - time_diff)) / 3600, 
                                         ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600), 0), 
           
           maj_PP_take_a = ifelse(major == 1 & (game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE")),
                                  ifelse(time_diff < 120, 
                                         ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                           ((penrate_GF[1,5] - penrate_GA[1,5]) * (300 - time_diff)) / 3600, 
                                         ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600), 0), 
           
           home_PP1_away_PP2 = ifelse(maj_PP_take_h > 0, 1, ifelse(maj_PP_take_a > 0, 2, 0)), 
           
           maj_PP_draw_h = ifelse(maj_PP_take_h > 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           maj_PP_draw_a = ifelse(maj_PP_take_a > 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           
           # Combine
           EV_take = PS_EV_take + maj_EV_take, 
           EV_draw = PS_EV_draw + maj_EV_draw, 
           PP_SH_take = PS_PP_take + maj_PP_take_h + maj_PP_take_a, 
           PP_SH_draw = PS_PP_draw + maj_PP_draw_h + maj_PP_draw_a, 
           
           pen_value_take = EV_take + PP_SH_take, 
           pen_value_take = ifelse(pen_value_take == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, pen_value_take), 
           pen_value_draw = EV_draw + PP_SH_draw, 
           pen_value_draw = ifelse(pen_value_draw == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, pen_value_draw)
           ) %>% 
    data.frame()
  
  return(pen_calc_xtras)
}
pen_calc_xtras <- fun.pen_value_xtra(pen_enhanced)

# Sum and Combine: Player Game-by-Game Adj. Pen Differential
fun.pen_value_sum <- function(main_data, xtra_data) {
  
  # Sum Main
  p_main_take <- main_data %>% 
    filter(first_assign != 1, team_pen != 1) %>% 
    group_by(event_player_1, game_id) %>% 
    summarise(take = sum(pen_value), 
              adj_take = sum(ifelse(home_team == event_team, pen_value * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value * pen_score_adj[home_lead_state, 5], pen_value))), 
              take_count = sum(event_type == "PENL")
              ) %>% 
    filter(!is.na(event_player_1)) %>% 
    rename(player = event_player_1)
  
  p_main_draw <- main_data %>% 
    filter(first_assign != 1, team_pen != 1) %>% 
    group_by(event_player_2, game_id) %>% 
    summarise(draw = sum(pen_value), 
              adj_draw = sum(ifelse(home_team == event_team, pen_value * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value * pen_score_adj[home_lead_state, 5], pen_value))), 
              draw_count = sum(event_type == "PENL")
              ) %>%
    filter(!is.na(event_player_2)) %>% 
    rename(player = event_player_2)
  
  p_main_all <- p_main_take %>% 
    full_join(., p_main_draw, by = c("player", "game_id")) %>% 
    data.frame()
  
  p_main_all[is.na(p_main_all)] <- 0
  
  
  # Sum Extras
  p_xtra_take <- xtra_data %>% 
    group_by(event_player_1, game_id) %>% 
    summarise(take = sum(pen_value_take), 
              adj_take = sum(ifelse(home_team == event_team, pen_value_take * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value_take * pen_score_adj[home_lead_state, 5], pen_value_take))), 
              take_count = sum(event_type == "PENL")
              ) %>%
    filter(!is.na(event_player_1)) %>% 
    rename(player = event_player_1)
  
  p_xtra_draw <- xtra_data %>% 
    group_by(event_player_2, game_id) %>% 
    summarise(draw = sum(pen_value_draw), 
              adj_draw = sum(ifelse(home_team == event_team, pen_value_draw * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value_draw * pen_score_adj[home_lead_state, 5], pen_value_draw))), 
              draw_count = sum(event_type == "PENL")
              ) %>%
    filter(!is.na(event_player_2)) %>% 
    rename(player = event_player_2)
  
  p_xtra_all <- p_xtra_take %>% 
    full_join(., p_xtra_draw, by = c("player", "game_id")) %>% 
    data.frame()
  
  p_xtra_all[is.na(p_xtra_all)] <- 0
  
  
  # Sum Assign
  p_assign_take <- main_data %>% 
    filter(first_assign == 1, assign_verify == 1, team_pen != 1) %>% 
    group_by(take_assign, game_id) %>% 
    summarise(take = sum(pen_value), 
              ## Unable to determine player's team at this point
              adj_take = sum(pen_value), 
              take_count = sum(event_type == "PENL")
              ) %>% 
    filter(!is.na(take_assign)) %>% 
    rename(player = take_assign)
  
  p_assign_draw <- main_data %>% 
    filter(first_assign == 1, assign_verify == 1, team_pen != 1) %>% 
    group_by(draw_assign, game_id) %>% 
    summarise(draw = sum(pen_value), 
              ## Unable to determine player's team at this point
              adj_draw = sum(pen_value), 
              draw_count = sum(event_type == "PENL")
              ) %>%
    filter(!is.na(draw_assign)) %>% 
    rename(player = draw_assign)
  
  p_assign_all <- p_assign_take %>% 
    full_join(., p_assign_draw, by = c("player", "game_id")) %>% 
    data.frame()
  
  p_assign_all[is.na(p_assign_all)] <- 0
  
  
  # Combine All Penalty Tables
  p_combined <- p_main_all %>% 
    rbind(., p_xtra_all, p_assign_all) %>% 
    group_by(player, game_id) %>% 
    summarise_all(funs(sum)) %>% 
    mutate_at(vars(take:draw_count), funs(round(.,3))) %>% 
    mutate(take = -(take), 
           adj_take = -(adj_take), 
           adj_pen_diff = adj_take + adj_draw
           ) %>% 
    select(player, game_id, take_count, draw_count, take, 
           draw, adj_take, adj_draw, adj_pen_diff
           ) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(p_combined)
}
adj_pen_diff <- fun.pen_value_sum(pen_calc_main, pen_calc_xtras)

# Sum Team Penalties: Game by Game
fun.pen_team_take <- function(data) {
  
  p_team_take <- data %>% 
    filter(team_pen == 1) %>% 
    group_by(event_team, game_id) %>% 
    summarise(take_count = sum(event_type == "PENL"), 
              take = sum(pen_value), 
              adj_take = sum(ifelse(home_team == event_team, pen_value*pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value*pen_score_adj[home_lead_state, 5], pen_value)))
              ) %>% 
    filter(!is.na(event_team)) %>% 
    mutate_at(vars(take:adj_take), funs(round(., 3))) %>% 
    rename(Team = event_team) %>% 
    data.frame()
  
  return(p_team_take)
}
pen_team_take <- fun.pen_team_take(pen_calc_main)



## -------------------- ##
##     Extras / Sum     ##
## -------------------- ##

# Simple Sum
player_pen <- adj_pen_diff %>% 
  select(-c(game_id, adj_pen_diff)) %>% 
  group_by(player) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(count_diff = draw_count - take_count, 
         adj_pen_diff = adj_take + adj_draw)




## - Requires additional data - ##

# Sum per Season/Team
seasons <- games_full_EV %>% 
  filter(season == "20162017") %>% 
  select(player, season, game_id, Team, TOI) %>% 
  left_join(., player_position, by = c("player"))

adj_pen_diff$game_id <- as.character(adj_pen_diff$game_id)

sum_test <- adj_pen_diff %>% 
  left_join(., seasons, by = c("player", "game_id")) %>% 
  group_by(player, position, season, Team) %>% 
  summarise_at(vars(take_count:adj_draw), funs(sum)
               ) %>% 
  mutate(adj_pen_diff = adj_take + adj_draw, 
         per_take = ifelse(take_count > 0, adj_take/take_count, 0), 
         per_draw = ifelse(draw_count > 0, adj_draw/draw_count, 0)
         ) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  filter(!is.na(season))

rm(seasons)



# Sum Career
sum_test_career <- adj_pen_diff %>% 
  left_join(., player_position, by = "player") %>% 
  group_by(player, position) %>% 
  summarise(draw = sum(draw_total), 
            take = sum(take_total), 
            adj_pen_diff = take + draw) %>% 
  mutate(position = ifelse(is.na(position), 3, position)) %>% 
  mutate_if(is.numeric, funs(round(., 2)))

TOI_all_sit_sum <- TOI_all_sit %>% 
  group_by(player) %>% 
  summarise(TOI_total = sum(TOI_total))

sum_career_pen <- sum_test_career %>% 
  left_join(., TOI_all_sit_sum, by = "player") %>% 
  data.frame()

pen_averages <- sum_career_pen_AA %>% 
  mutate(position = ifelse(position == 1 & is.na(TOI_total), 3, position)) %>% 
  filter(position != 3) %>% 
  group_by(position) %>% 
  summarise_at(vars(TOI_total, draw, take), funs(sum)) %>% 
  mutate(draw_avg = draw/TOI_total, take_avg = take/TOI_total) %>% 
  data.frame()

sum_career_pen_AA <- sum_career_pen %>% 
  filter(position != 3) %>% 
  mutate(draw_AA = ((draw/TOI_total) - pen_averages[position, 5])*TOI_total, 
         take_AA = ((take/TOI_total) - pen_averages[position, 6])*TOI_total, 
         diff = take_AA + draw_AA) %>% 
  mutate_at(vars(draw_AA, take_AA, diff), funs(round(., 2)))
