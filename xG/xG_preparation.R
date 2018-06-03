#### --------------------------------------------------- ####
## -       xG Preparation Functions     |   06.02.18     - ##
#### --------------------------------------------------- ####


# Required packages
library(xgboost); library(dplyr)


options(scipen = 999)
set.seed(250)


## Objects
st.fenwick_events <- c("SHOT", "GOAL", "MISS") 
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor() 
st.uneven_strength <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
st.pp_strength <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor()
st.empty_net <- c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()




## ----------------------- ##
##  Model prep functions   ##
## ----------------------- ##

# All functions are set up to work with a dataframe that contains the NHL RTSS/play-by-play data. This data can be either downloaded 
# from Emmanuel Perry's Corsica site (https://twitter.com/CorsicaHockey/status/980931037533532160) or scraped using his dryscrape 
# functions (https://github.com/mannyelk/corsica/tree/master/modules). The "data" argument should be this play-by-play dataframe. 
# If xG values are needed an xG model is done, it is recommended to use the pbp_full_add function, which uses all of the following. 


# Main preparation functions 
fun.pbp_expand <- function(data) {
  
  # Prepares play by play data for the xG model. These variables are not xG specific and can be used for other tasks. 
  # Variables: event_circle, event_rinkside, event_zone, home_zone, pbp_distance, event_distance, event_angle, 
  # game_strength_state, home_skaters, away_skaters. Additionally, a faceoff index is added for later joins.
  
  data$event_description <- as.character(data$event_description)
  data$coords_x <- as.numeric(as.character(data$coords_x))
  data$coords_y <- as.numeric(as.character(data$coords_y))
  
  print("expand", quote = F) 
  
  hold <- data %>% 
    # Manny's enhanced pbp functions (from the dryscrape functions)
    mutate(event_circle = 
             1 * (coords_x <= -25 & coords_y > 0) + 
             2 * (coords_x <= -25 & coords_y < 0) + 
             3 * (coords_x < 0 & coords_x > 25 & coords_y > 0) + 
             4 * (coords_x < 0 & coords_x > 25 & coords_y < 0) +
             5 * (abs(coords_x) < 5 & abs(coords_y) < 5) + 
             6 * (coords_x > 0 & coords_x < 25 & coords_y > 0) +
             7 * (coords_x > 0 & coords_x < 25 & coords_y < 0) + 
             8 * (coords_x >= 25 & coords_y > 0) +
             9 * (coords_x >= 25 & coords_y < 0), 
           
           event_rinkside = ifelse(coords_x <= -25, "L",
                                   ifelse(coords_x > -25 & coords_x < 25, "N", 
                                          ifelse(coords_x >= 25, "R", NA))),
           
           event_zone = ifelse((grepl("off. zone", tolower(event_description)) == TRUE), "Off", 
                               ifelse((grepl("neu. zone", tolower(event_description)) == TRUE), "Neu", 
                                      ifelse((grepl("def. zone", tolower(event_description)) == TRUE), "Def", NA))),  
           
           event_zone = ifelse(event_zone == "Def" & event_type == "BLOCK", "Off", event_zone), 
           
           home_zone = ifelse(event_team == away_team & event_zone == "Off", "Def", 
                              ifelse(event_team == away_team & event_zone == "Def", "Off", event_zone)), 
           
           #  Initial distance / angle calculation
           pbp_distance = suppressWarnings(as.numeric(sub(".*Zone, *(.*?) * ft.*", "\\1", event_description))), 
           pbp_distance = ifelse(event_type %in% st.fenwick_events & is.na(pbp_distance), 0, pbp_distance), 
           
           event_distance = sqrt((89 - abs(coords_x))^2 + coords_y^2), 
           
           event_angle = abs(atan(coords_y / (89 - abs(coords_x))) * (180 / pi)), 
           
           # Update distance calc for long shots (and various mistakes)
           event_distance = ifelse(event_type %in% st.fenwick_events & 
                                     pbp_distance > 89 & 
                                     coords_x < 0 & 
                                     event_detail != "Tip-In" & 
                                     event_detail != "Wrap-around" & 
                                     event_detail != "Deflected" & 
                                     !(pbp_distance > 89 & event_zone == "Off"), 
                                   sqrt((abs(coords_x) + 89)^2 + coords_y^2), 
                                   
                                   ifelse(event_type %in% st.fenwick_events & 
                                            pbp_distance > 89 & 
                                            coords_x > 0 & 
                                            event_detail != "Tip-In" & 
                                            event_detail != "Wrap-around" & 
                                            event_detail != "Deflected" & 
                                            !(pbp_distance > 89 & event_zone == "Off"), 
                                          sqrt((coords_x + 89)^2 + coords_y^2), 
                                          event_distance)),  
           
           event_angle = ifelse(event_type %in% st.fenwick_events & pbp_distance > 89 & coords_x < 0 & 
                                  event_detail != "Tip-In" & 
                                  event_detail != "Wrap-around" & 
                                  event_detail != "Deflected" & 
                                  !(pbp_distance > 89 & event_zone == "Off"), 
                                abs( atan(coords_y / (abs(coords_x) + 89)) * (180 / pi)), 
                                
                                ifelse(event_type %in% st.fenwick_events & 
                                         pbp_distance > 89 & 
                                         coords_x > 0 & 
                                         event_detail != "Tip-In" & 
                                         event_detail != "Wrap-around" & 
                                         event_detail != "Deflected" & 
                                         !(pbp_distance > 89 & event_zone == "Off"), 
                                       abs(atan(coords_y / (coords_x + 89)) * (180 / pi)), 
                                       event_angle)), 
           
           event_zone = ifelse(event_type %in% st.fenwick_events & 
                                 event_zone == "Def" & 
                                 pbp_distance <= 64, "Off", event_zone), 
           
           # Update penalty shot strength states
           game_strength_state = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                          event_team == home_team, "Ev1", 
                                        ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                                 event_team == away_team, "1vE", game_strength_state)), 
           
           home_skaters = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                   event_team == home_team, 1, 
                                 ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                          event_team == away_team, 0, home_skaters)), 
           
           away_skaters = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                   event_team == home_team, 0, 
                                 ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & 
                                          event_team == away_team, 1, away_skaters))
           )
  
  print("face_ID", quote = F)  
  
  # Add home_zonestart for corsi events
  face_index <- hold %>% 
    filter(event_type %in% c(st.corsi_events, "FAC"),
           game_period < 5
           ) %>%
    arrange(game_id, event_index) %>%
    mutate(face_index = cumsum(event_type == "FAC")) %>%
    group_by(game_id, face_index) %>%
    arrange(event_index) %>% 
    mutate(test = first(home_zone),  
           home_zonestart = ifelse(first(home_zone) == "Def", 1, 
                                   ifelse(first(home_zone) == "Neu", 2, 
                                          ifelse(first(home_zone) == "Off", 3, NA)))
           ) %>%
    ungroup() %>% 
    select(game_id, event_index, home_zonestart) %>% 
    data.frame()
  
  # Join
  print("join", quote = F) 
  print("---", quote = F)
  
  hold <- left_join(hold, face_index, by = c("game_id", "event_index"))
  }
fun.pbp_index <- function(data) { 
  
  # Add shift/penlaty indexes, adjust the faceoff_index for xG training, & create unique shift IDs
  
  print("index", quote = F)
  
  pbp_hold <- data %>% 
    arrange(game_id, event_index) %>% 
    mutate(face_index =  cumsum(event_type == "FAC"), 
           shift_index = cumsum(event_type == "ON"), 
           pen_index =   cumsum(event_type == "PENL"))
  
  
  print("shift_ID", quote = F)
  
  hold <- pbp_hold %>% 
    filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
           game_period < 5
           ) %>% 
    group_by(game_id, game_period, season,
             home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
             home_goalie, away_goalie, 
             face_index, shift_index, pen_index
             ) %>% 
    mutate(shift_ID = round(first(event_index) * as.numeric(game_id))) %>% 
    summarise(shift_ID = first(shift_ID), 
              shift_length = last(game_seconds) - first(game_seconds)) %>% 
    ungroup() %>% 
    select(game_id, game_period, season, shift_ID, face_index, 
           shift_index, pen_index, shift_length, home_on_1:away_goalie
           ) %>% 
    data.frame()
  
  
  print("join", quote = F)
  print("---", quote = F)
  
  join <- left_join(pbp_hold, hold, by = c("game_id", "game_period", "season",
                                           "home_on_1", "home_on_2", "home_on_3", 
                                           "home_on_4", "home_on_5", "home_on_6", 
                                           "away_on_1", "away_on_2", "away_on_3", 
                                           "away_on_4","away_on_5", "away_on_6", 
                                           "home_goalie", "away_goalie", 
                                           "face_index", "shift_index", "pen_index"))
}
fun.pbp_prep <- function(data, prep_type) {
  
  # Prepare pbp data for xG model training. 
  # prep_type can be either "EV", "UE", "SH", or "EN" depending on preferred strength state.  
  
  if(prep_type == "EV") { 
    
    # Prep for EV xG model
    pbp_prep_EV <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last = game_seconds - lag(game_seconds),
             event_type_last = lag(event_type),
             event_team_last = lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last = lag(coords_x),
             coords_y_last = lag(coords_y)
             ) %>%
      ungroup() %>%
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             game_strength_state %in% st.even_strength, 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      # Create priors
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2)
             ) %>%
      rename(shot_distance = event_distance, 
             shot_angle = event_angle
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, event_type_last, 
             seconds_since_last, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length
             ) %>% 
      data.frame()
    
  }
  else if(prep_type == "UE") { 
    
    # Prep for UE xG model
    pbp_prep_UE <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last = game_seconds - lag(game_seconds),
             event_type_last = lag(event_type),
             event_team_last = lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last = lag(coords_x),
             coords_y_last = lag(coords_y),
             true_strength_state = paste0(home_skaters, "v", away_skaters)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      mutate(pen_seconds_since = (game_strength_state %in% st.pp_strength) * (game_seconds - first(game_seconds)), 
             pen_seconds_since = ifelse(pen_seconds_since > 0 & pen_seconds_since >= 300, 120, pen_seconds_since)
             ) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             event_team == home_team & true_strength_state %in% c("6v5", "6v4", "5v4", "5v3", "4v3") | 
               event_team == away_team & true_strength_state %in% c("5v6", "4v6", "4v5", "3v5", "3v4"), 
             !is.na(coords_x_last), 
             !is.na(coords_y_last) 
             ) %>% 
      # Create priors
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             prior_event_EV =     1 * (event_strength_last %in% st.even_strength)
             ) %>%
      rename(shot_distance = event_distance, 
             shot_angle = event_angle
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, true_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, prior_event_EV, event_type_last, 
             seconds_since_last, pen_seconds_since, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length
             ) %>% 
      data.frame()
    
  }
  else if(prep_type == "SH") { 
    
    # Prep for SH xG model
    pbp_prep_SH <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last = game_seconds - lag(game_seconds),
             event_type_last = lag(event_type),
             event_team_last = lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last = lag(coords_x),
             coords_y_last = lag(coords_y),
             shift_ID = as.numeric(shift_ID)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      mutate(pen_seconds_since = (game_strength_state %in% st.pp_strength) * (game_seconds - first(game_seconds)), 
             pen_seconds_since = ifelse(pen_seconds_since > 0 & pen_seconds_since >= 300, 120, pen_seconds_since)
             ) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             event_team == away_team & game_strength_state %in% c("5v4", "5v3", "4v3") | 
               event_team == home_team & game_strength_state %in% c("4v5", "3v5", "3v4"), 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             prior_event_EV =     1 * (event_strength_last %in% st.even_strength)
             ) %>%
      rename(shot_distance = event_distance, 
             shot_angle = event_angle
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, prior_event_EV, event_type_last, 
             seconds_since_last, pen_seconds_since, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length
             ) %>% 
      data.frame()
    
  }
  else if(prep_type == "EN") { 
    
    # Prep for EN xG model
    pbp_prep_EN <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last = game_seconds - lag(game_seconds),
             event_type_last = lag(event_type),
             event_team_last = lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last = lag(coords_x),
             coords_y_last = lag(coords_y),
             shift_ID = as.numeric(shift_ID)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             event_team == away_team & game_strength_state %in% c("Ev5", "Ev4", "Ev3") | 
               event_team == home_team & game_strength_state %in% c("5vE", "4vE", "3vE"), 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2)
             ) %>%
      rename(shot_distance = event_distance, 
             shot_angle = event_angle
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, event_type_last, 
             seconds_since_last, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length
             ) %>% 
      filter(!is.na(shot_distance), 
             !is.na(shot_angle)
             ) %>% 
      data.frame()
    
  }
}
fun.model_prep <- function(data, prep_type) { 
  
  # Prep model data frames 
  # prep_type can be "EV", "UE", "SH", or "EN". 
  
  if(prep_type == "EV") { 
    
    # Create EV dummy variables, returns a matrix.
    model_prep <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_5v5 = 1 * (game_strength_state == "5v5"), 
             state_4v4 = 1 * (game_strength_state == "4v4"), 
             state_3v3 = 1 * (game_strength_state == "3v3"), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             
             prior_give_opp =  1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_give_same = 1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_opp =  1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_take_same = 1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_opp =   1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_hit_same =  1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_face =      1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal,
             shot_distance, shot_angle, is_home, 
             state_5v5:state_3v3, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, 
             prior_shot_same:prior_face
             ) %>% 
      data.matrix()
    
  }
  else if(prep_type == "UE") { 
    
    # Create UE dummy variables, returns a matrix.
    model_prep <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_5v4 = 1 * ((true_strength_state == "5v4" & event_team == home_team) | (true_strength_state == "4v5" & event_team == away_team)), 
             state_5v3 = 1 * ((true_strength_state == "5v3" & event_team == home_team) | (true_strength_state == "3v5" & event_team == away_team)), 
             state_4v3 = 1 * ((true_strength_state == "4v3" & event_team == home_team) | (true_strength_state == "3v4" & event_team == away_team)), 
             state_6v5 = 1 * ((true_strength_state == "6v5" & event_team == home_team) | (true_strength_state == "5v6" & event_team == away_team)), 
             state_6v4 = 1 * ((true_strength_state == "6v4" & event_team == home_team) | (true_strength_state == "4v6" & event_team == away_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_5v4:state_6v4, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, prior_event_EV, 
             pen_seconds_since, prior_shot_same:prior_face
             ) %>% 
      data.matrix()
    
  }
  else if(prep_type == "SH") { 
    
    # Create SH dummy variables, returns a matrix.
    model_prep_SH <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_4v5 = 1 * ((game_strength_state == "5v4" & event_team == away_team) | (game_strength_state == "4v5" & event_team == home_team)), 
             state_3v5 = 1 * ((game_strength_state == "5v3" & event_team == away_team) | (game_strength_state == "3v5" & event_team == home_team)), 
             state_3v4 = 1 * ((game_strength_state == "4v3" & event_team == away_team) | (game_strength_state == "3v4" & event_team == home_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_4v5:state_3v4, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, prior_event_EV, 
             pen_seconds_since, prior_shot_same:prior_face) %>% 
      data.matrix()
    
  }
  else if(prep_type == "EN") { 
    
    # Create EN dummy variables, returns a matrix.
    model_prep_EN <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_Ev5 = 1 * ((game_strength_state == "Ev5" & event_team == away_team) | (game_strength_state == "5vE" & event_team == home_team)), 
             state_Ev4 = 1 * ((game_strength_state == "Ev4" & event_team == away_team) | (game_strength_state == "4vE" & event_team == home_team)), 
             state_Ev3 = 1 * ((game_strength_state == "Ev3" & event_team == away_team) | (game_strength_state == "3vE" & event_team == home_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_Ev5:state_Ev3, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, 
             prior_shot_same:prior_face) %>% 
      data.matrix()
    
  }
}

# Full function for xG values
fun.pbp_full_add <- function(data, model_EV, model_UE, model_SH, model_EN) { 
  
  ### Function should be run using unprocessed scraped pbp data. Returns a list of 3 data frames: 
  ## [[1]] - full pbp data with extras and xG probabilities for both EV and UE situations
  ## [[2]] - EV prep data frame for xG model evaluation
  ## [[3]] - UE prep data frame for xG model evaluation
  ## [[4]] - SH prep data frame for xG model evaluation
  ## [[5]] - EN prep data frame for xG model evaluation
  
  # Initial prep of scraped pbp data.
  pbp_part <- fun.pbp_expand(data)
  pbp_part <- fun.pbp_index(pbp_part)
  
  
  # Convert for use with XGBoost and predict goal probability - EV
  print("predict_EV", quote = F)
  pbp_prep_EV <- fun.pbp_prep(pbp_part, "EV")
  model_prep_EV <- fun.model_prep(pbp_prep_EV, "EV")
  
  model_prep_EV <- Matrix(model_prep_EV, sparse = TRUE)
  pred_matrix_EV <- model_prep_EV[, 2:ncol(model_prep_EV)]
  xgb_matrix_EV <- xgb.DMatrix(data = pred_matrix_EV)
  
  pbp_prep_EV$pred_XGB_7 <- predict(object = model_EV, xgb_matrix_EV)
  pred_goal_EV <- select(pbp_prep_EV, game_id, event_index, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - UE
  print("predict_UE", quote = F)
  pbp_prep_UE <- fun.pbp_prep(pbp_part, "UE")
  model_prep_UE <- fun.model_prep(pbp_prep_UE, "UE")
  
  model_prep_UE <- Matrix(model_prep_UE, sparse = TRUE)
  pred_matrix_UE <- model_prep_UE[, 2:ncol(model_prep_UE)]
  xgb_matrix_UE <- xgb.DMatrix(data = pred_matrix_UE)
  
  pbp_prep_UE$pred_XGB_7 <- predict(object = model_UE, xgb_matrix_UE)
  pred_goal_UE <- select(pbp_prep_UE, game_id, event_index, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - SH
  print("predict_SH", quote = F)
  pbp_prep_SH <- fun.pbp_prep(pbp_part, "SH")
  model_prep_SH <- fun.model_prep(pbp_prep_SH, "SH")
  
  model_prep_SH <- Matrix(model_prep_SH, sparse = TRUE)
  pred_matrix_SH <- model_prep_SH[, 2:ncol(model_prep_SH)]
  xgb_matrix_SH <- xgb.DMatrix(data = pred_matrix_SH)
  
  pbp_prep_SH$pred_XGB_7 <- predict(object = model_SH, xgb_matrix_SH)
  pred_goal_SH <- select(pbp_prep_SH, game_id, event_index, pred_XGB_7)
  
  
  
  # Convert for use with XGBoost and predict goal probability - EN
  print("predict_EN", quote = F)
  pbp_prep_EN <- fun.pbp_prep(pbp_part, "EN")
  model_prep_EN <- fun.model_prep(pbp_prep_EN, "EN")
  
  model_prep_EN <- Matrix(model_prep_EN, sparse = TRUE)
  pred_matrix_EN <- model_prep_EN[, 2:ncol(model_prep_EN)]
  xgb_matrix_EN <- xgb.DMatrix(data = pred_matrix_EN)
  
  pbp_prep_EN$pred_XGB_7 <- predict(object = model_EN, xgb_matrix_EN)
  pred_goal_EN <- select(pbp_prep_EN, game_id, event_index, pred_XGB_7)
  
  
  
  # Join / Make List
  print("final_join", quote = F)
  
  hold <- rbind(pred_goal_EV, 
                pred_goal_UE, 
                pred_goal_SH, 
                pred_goal_EN
                )
  
  pbp_return <- pbp_part %>% 
    left_join(., hold, by = c("game_id", "event_index")) #%>% 
  #select(-c(face_index:pen_index))
  
  pbp_return$shift_ID <- as.character(pbp_return$shift_ID)
  pbp_prep_EV$shift_ID <- as.character(pbp_prep_EV$shift_ID)
  pbp_prep_UE$shift_ID <- as.character(pbp_prep_UE$shift_ID)
  pbp_prep_SH$shift_ID <- as.character(pbp_prep_SH$shift_ID)
  pbp_prep_EN$shift_ID <- as.character(pbp_prep_EN$shift_ID)
  
  list_data <- list(pbp_full = pbp_return, 
                    prep_EV = pbp_prep_EV, 
                    prep_UE = pbp_prep_UE, 
                    prep_SH = pbp_prep_SH, 
                    prep_EN = pbp_prep_EN
                    )
  
}

# Run
pbp_full_list <- fun.pbp_full_add(data = pbp_raw, 
                                  model_EV = xG_model_XGB_7_EV, 
                                  model_UE = xG_model_XGB_7_UE, 
                                  model_SH = xG_model_XGB_10_SH, 
                                  model_EN = xG_model_XGB_10_EN
                                  )


# Return data from function
pbp_df <-   pbp_full_list$pbp_full
model_EV <- pbp_full_list$prep_EV
model_UE <- pbp_full_list$prep_UE
model_SH <- pbp_full_list$prep_SH
model_EN <- pbp_full_list$prep_EN




##### ----------------------                END                ---------------------- #####               

