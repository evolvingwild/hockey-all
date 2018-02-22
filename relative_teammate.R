#################################################################################
#####        Relative to Teammate       ||              12/26/17            #####
#################################################################################

# NHL pbp data scraped using Emmanuel Perry's dryscrape functions. Code available here: https://github.com/mannyelk/corsica/tree/master/modules

library(dplyr)

options(scipen = 999)

# Create Score Adjust Data Frame / xG adjustment
scoreadj_corsi <- data.frame(matrix(nrow = 7, ncol = 3))

scoreadj_corsi[, 1] <- c(1, 2, 3, 4, 5, 6, 7)
scoreadj_corsi[, 2] <- c(0.840, 0.865, 0.898, 0.970, 1.052, 1.104, 1.138)
scoreadj_corsi[, 3] <- c(1.236, 1.186, 1.128, 1.032, 0.953, 0.914, 0.892)

colnames(scoreadj_corsi) <- c("home_lead", "home_corsi_adj", "away_corsi_adj")

# xG Venue Adjustment Values
xG_adj_h <- 0.9468472
xG_adj_a <- 1.059477

# Skater Positions
player_position <- readRDS("skater_position.rds") # available here: https://github.com/evolvingwild/hockey-all/blob/master/skater_position.rds

## Objects
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
st.even_strength <- as.factor(c("5v5", "4v4", "3v3"))

off_f_cut <- 90
off_d_cut <- 115
def_f_cut <- 125
def_d_cut <- 100

# Functions
fun.goalie_find <- function(data) {
  
  # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
  goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$home_goalie, data$away_goalie))))), 
                              is_goalie = 1)
  
  goalie_return$player <- as.character(goalie_return$player)
  
  return(goalie_return)
}
rel_adj <- function(t_metric_wo, t_toi_wo, t_metric, cut, perc) {
  
  # Low TOI adjustment for various metrics
  x <- ((t_metric_wo * (t_toi_wo^2 / cut^2)) + t_metric * (1 - (t_toi_wo^2 / cut^2))) * perc
  
  return(as.numeric(x))
  }


## ------------------------------------------------- ##
##       EV On-Ice Shot Metrics - Game by Game       ##
## ------------------------------------------------- ##

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.onice_H <- function(data, venue) {
  
  on_ice <- data %>% 
    summarise(Team = first(home_team), 
              TOI = sum(event_length) / 60,
              GF =  sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)), 
              GA =  sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)),
              CF =  sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1 * scoreadj_corsi[home_lead, 2]), 0)), 
              CA =  sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1 * scoreadj_corsi[home_lead, 3]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal * xG_adj_h, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal * xG_adj_a, 0)))
              )
  
  return(on_ice)
}
fun.onice_A <- function(data, venue) {
  
  on_ice <- data %>% 
    summarise(Team = first(away_team), 
              TOI = sum(event_length) / 60,
              GF =  sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              GA =  sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)),
              CF =  sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1 * scoreadj_corsi[home_lead, 3]), 0)), 
              CA =  sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1 * scoreadj_corsi[home_lead, 2]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal * xG_adj_a, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal * xG_adj_h, 0)))
              )
  
  return(on_ice)
}
fun.onice_combine <- function(data, year) {
  
  # Filter pbp data
  hold <- data %>% 
    filter(game_strength_state %in% st.even_strength, game_period < 5, event_length < 900) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)),
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           )
  
  print("home_skaters", quote = F)
  h1 <- hold %>% 
    group_by(game_id, home_on_1, home_team) %>% 
    fun.onice_H(., "home_on_1") %>% 
    rename(player = home_on_1)
  
  h2 <- hold %>% 
    group_by(game_id, home_on_2, home_team) %>% 
    fun.onice_H(., "home_on_2") %>% 
    rename(player = home_on_2)
  
  h3 <- hold %>% 
    group_by(game_id, home_on_3, home_team) %>%
    fun.onice_H(., "home_on_3") %>% 
    rename(player = home_on_3)
  
  h4 <- hold %>% 
    group_by(game_id, home_on_4, home_team) %>% 
    fun.onice_H(., "home_on_4") %>% 
    rename(player = home_on_4)
  
  h5 <- hold %>% 
    group_by(game_id, home_on_5, home_team) %>% 
    fun.onice_H(., "home_on_5") %>% 
    rename(player = home_on_5)
  
  h6 <- hold %>% 
    group_by(game_id, home_on_6, home_team) %>% 
    fun.onice_H(., "home_on_6") %>% 
    rename(player = home_on_6)
  
  
  print("away_skaters", quote = F)
  a1 <-  hold %>% 
    group_by(game_id, away_on_1, away_team) %>% 
    fun.onice_A(., "away_on_1") %>% 
    rename(player = away_on_1)
  
  a2 <-  hold %>% 
    group_by(game_id, away_on_2, away_team) %>% 
    fun.onice_A(., "away_on_2") %>% 
    rename(player = away_on_2)
  
  a3 <-  hold %>% 
    group_by(game_id, away_on_3, away_team) %>%
    fun.onice_A(., "away_on_3") %>% 
    rename(player = away_on_3)
  
  a4 <- hold %>% 
    group_by(game_id, away_on_4, away_team) %>% 
    fun.onice_A(., "away_on_4") %>% 
    rename(player = away_on_4)
  
  a5 <- hold %>% 
    group_by(game_id, away_on_5, away_team) %>% 
    fun.onice_A(., "away_on_5") %>% 
    rename(player = away_on_5)
  
  a6 <- hold %>% 
    group_by(game_id, away_on_6, away_team) %>% 
    fun.onice_A(., "away_on_6") %>% 
    rename(player = away_on_6)
  
  
  # Combine
  join_df <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  
  # Remove Goalies
  goalie_remove <- fun.goalie_find(data)
  join_remove <- join_df[!(join_df$player %in% goalie_remove$player),]
  
  
  # Format
  join_return <- join_remove %>% 
    group_by(player, game_id) %>% 
    mutate_at(vars(TOI, CF, CA), funs(round(., 2))) %>% 
    mutate_at(vars(xGF, xGA), funs(round(., 3))) %>% 
    summarise(Team = first(Team), 
              TOI  = sum(TOI), 
              GF   = sum(GF), 
              GA   = sum(GA), 
              CF   = sum(CF), 
              CA   = sum(CA), 
              xGF  = sum(xGF), 
              xGA  = sum(xGA)
              ) %>% 
    filter(!is.na(player)) %>% 
    mutate(season = year) %>% 
    select(player, game_id, season, Team, TOI, GF, GA, CF, CA, xGF, xGA) %>% 
    data.frame()
  
  return(join_return)
}

on_ice_EV <- fun.onice_combine(data = pbp_df, 
                               year = "20172018")


## ------------------------------------------------- ##
##       EV Rel_TM Shot Metrics - Game by Game       ##
## ------------------------------------------------- ##

# Combos: On Ice Corsi For / Against, TOI, Team - per game
fun.QoT_H <- function(data) {
  
  hold_player <- data %>% 
    summarise(Team = first(home_team), 
              TOI = sum(event_length) / 60,
              GF =  sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)), 
              GA =  sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              CF =  sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1 * scoreadj_corsi[home_lead, 2]), 0)), 
              CA =  sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1 * scoreadj_corsi[home_lead, 3]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal * xG_adj_h, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal * xG_adj_a, 0)))
              )
  
  return(hold_player)
}
fun.QoT_A <- function(data) {
  
  hold_player <- data %>% 
    summarise(Team = first(away_team), 
              TOI = sum(event_length) / 60,
              GF =  sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              GA =  sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)),
              CF =  sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1 * scoreadj_corsi[home_lead, 3]), 0)), 
              CA =  sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1 * scoreadj_corsi[home_lead, 2]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal * xG_adj_a, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal * xG_adj_h, 0)))
              )
  
  return(hold_player)
}
fun.event_playerH <- function(data, player) {
  
  if(player == "home_on_1") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_1, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_2)
    h2 <- data %>% 
      group_by(game_id, home_on_1, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_1, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_1, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_1, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_6)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
  else if(player == "home_on_2") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_2, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_2, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_2, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_2, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_2, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_6)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
  else if(player == "home_on_3") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_3, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_3, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_3, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_3, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_3, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_6)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
  else if(player == "home_on_4") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_4, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_4, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_4, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_4, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_4, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_6)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
  else if(player == "home_on_5") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_5, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_5, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_5, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_5, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_5, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_6)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
  else if(player == "home_on_6") {
    
    h1 <- data %>% 
      group_by(game_id, home_on_6, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_6, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_6, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_6, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_6, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_5)
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  }
}
fun.event_playerA <- function(data, player) {
  
  if(player == "away_on_1") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_1, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_2)
    a2 <- data %>% 
      group_by(game_id, away_on_1, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_1, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_1, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_1, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_6)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
  else if(player == "away_on_2") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_2, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_2, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_2, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_2, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_2, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_6)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
  else if(player == "away_on_3") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_3, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_3, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_3, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_3, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_3, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_6)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
  else if(player == "away_on_4") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_4, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_4, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_4, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_4, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_4, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_6)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
  else if(player == "away_on_5") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_5, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_5, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_5, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_5, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_5, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_6)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
  else if(player == "away_on_6") {
    
    a1 <- data %>% 
      group_by(game_id, away_on_6, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_6, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_6, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_6, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_6, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_5)
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  }
}

# Combine 
fun.teammate <- function(data, year) {
  
  # Filter pbp data
  hold <- data %>% 
    filter(game_strength_state %in% st.even_strength, 
           game_period < 5, 
           event_length < 900
           ) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)),
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           )
  
  
  # Home functions
  print("home_on_1", quote = F)
  home1 <- fun.event_playerH(hold, "home_on_1")
  
  print("home_on_2", quote = F)
  home2 <- fun.event_playerH(hold, "home_on_2")
  
  print("home_on_3", quote = F)
  home3 <- fun.event_playerH(hold, "home_on_3")
  
  print("home_on_4", quote = F)
  home4 <- fun.event_playerH(hold, "home_on_4")
  
  print("home_on_5", quote = F)
  home5 <- fun.event_playerH(hold, "home_on_5")
  
  print("home_on_6", quote = F)
  home6 <- fun.event_playerH(hold, "home_on_6")
  
  # Away functions
  print("away_on_1", quote = F)
  away1 <- fun.event_playerA(hold, "away_on_1")
  
  print("away_on_2", quote = F)
  away2 <- fun.event_playerA(hold, "away_on_2")
  
  print("away_on_3", quote = F)
  away3 <- fun.event_playerA(hold, "away_on_3")
  
  print("away_on_4", quote = F)
  away4 <- fun.event_playerA(hold, "away_on_4")
  
  print("away_on_5", quote = F)
  away5 <- fun.event_playerA(hold, "away_on_5")
  
  print("away_on_6", quote = F)
  away6 <- fun.event_playerA(hold, "away_on_6")
  
  
  # Bind 
  print("bind", quote = F)
  
  home_all <- home1 %>% 
    rbind(., home2, home3, home4, home5, home6) %>% 
    group_by(player, teammate, game_id, home_team) %>% 
    summarise_at(vars(TOI:xGA), funs(sum)) %>% 
    filter(!is.na(player), 
           !is.na(teammate)
           ) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  away_all <- away1 %>% 
    rbind(., away2, away3, away4, away5, away6) %>% 
    group_by(player, teammate, game_id, away_team) %>% 
    summarise_at(vars(TOI:xGA), funs(sum)) %>% 
    filter(!is.na(player), 
           !is.na(teammate)
           ) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  
  # Remove Goalies
  print("remove_goalies", quote = F)
  
  goalie_remove <- fun.goalie_find(data)
  
  testH <- home_all[!(home_all$player %in% goalie_remove$player),]
  testH <- testH[!(testH$teammate %in% goalie_remove$player),]
  
  testA <- away_all[!(away_all$player %in% goalie_remove$player),]
  testA <- testA[!(testA$teammate %in% goalie_remove$player),]
  
  
  # Combine
  print("combine", quote = F)
  
  ALL <- testH %>% 
    rbind(., testA) %>% 
    mutate(season = year) %>% 
    group_by(player, teammate, game_id, Team) %>% 
    mutate_at(vars(TOI, CF, CA), funs(round(., 2))) %>% 
    mutate_at(vars(xGF, xGA), funs(round(., 3))) %>% 
    select(player, teammate, game_id, season, Team, TOI, GF, GA, CF, CA, xGF, xGA) %>% 
    arrange(player, teammate, game_id) %>% 
    data.frame()

  return(ALL)
}
rel_TM_combos_EV <- fun.teammate(data = pbp_df, 
                                 year = "20172018")



## --------------------- ##
##       Calculate       ##
## --------------------- ##

# Teams
teams <- on_ice_EV %>% 
  group_by(player, Team, season) %>% 
  summarise(TOI = sum(TOI)) %>% 
  group_by(player, season) %>% 
  mutate(Team = paste0(Team, collapse = "/")) %>% 
  summarise(Team = first(Team))


# WOWY
fun.rel_source <- function(TM_data, games_data, position_data, year) {
  
  # Prep
  prep_TM <- TM_data %>% 
    filter(season == year, !is.na(CF)) %>% 
    group_by(player, teammate, season) %>% 
    mutate(G_T = n()) %>% 
    summarise(TOI_tog = round(sum(TOI), 2),
              CF_tog  = sum(CF), 
              CA_tog  = sum(CA),
              xGF_tog = sum(xGF),
              xGA_tog = sum(xGA)
              ) %>%
    data.frame()
  
  # Create player and teammate data.frames
  rel_player <- games_data %>% 
    filter(season == year, !is.na(CF)) %>% 
    group_by(player, season) %>% 
    summarise_at(vars(TOI, CF, CA, xGF, xGA), funs(sum)) %>% 
    mutate(CF60  = round(((CF / TOI) * 60), 2), 
           CA60  = round(((CA / TOI) * 60), 2),
           xGF60 = round(((xGF / TOI) * 60), 3),
           xGA60 = round(((xGA / TOI) * 60), 3)
           ) %>% 
    data.frame()
  
  rel_teammate <- rename(rel_player, teammate = player)
  
  
  # Join
  relmerge_P <- left_join(prep_TM, rel_player, by = c("player", "season"))
  relmerge_T <- left_join(prep_TM, rel_teammate, by = c("teammate", "season"))
  rel_All <- left_join(relmerge_P, relmerge_T, by = c("player", "teammate", "season", "TOI_tog", 
                                                      "CF_tog", "CA_tog", "xGF_tog", "xGA_tog"))
  
  
  # Add in positions
  rel_All_position <- position_data %>% 
    rename(TM_position = position, 
           teammate = player) %>% 
    left_join(rel_All, ., by = c("teammate")) %>% 
    mutate(TM_position = ifelse(is.na(TM_position), 1, TM_position)) %>% 
    data.frame()
  
  colnames(rel_All_position) <- gsub(".x", "_p", colnames(rel_All_position))
  colnames(rel_All_position) <- gsub(".y", "_t", colnames(rel_All_position))
  rel_All_position <- rename(rel_All_position, player = pl_ter)

  
  # Calculate With/Without Numbers for Each Player & Teammate
  rel_TM_metrics <- rel_All_position %>% 
    mutate(TM_TOI_w.o = TOI_t - TOI_tog,
           TM_CF_w.o  = CF_t - CF_tog,
           TM_CA_w.o  = CA_t - CA_tog,
           TM_xGF_w.o = xGF_t - xGF_tog,
           TM_xGA_w.o = xGA_t - xGA_tog
           ) %>% 
    filter(TM_TOI_w.o > 0) %>% 
    mutate(TM_CF_60_w.o  = round(((TM_CF_w.o / TM_TOI_w.o) * 60), 2), 
           TM_CA_60_w.o  = round(((TM_CA_w.o / TM_TOI_w.o) * 60), 2), 
           TM_xGF_60_w.o = round(((TM_xGF_w.o / TM_TOI_w.o) * 60), 3), 
           TM_xGA_60_w.o = round(((TM_xGA_w.o / TM_TOI_w.o) * 60), 3), 
           
           player_TOI_perc_w = round(TOI_tog / TOI_p, 4),
           
           weighted_TM_CF60  = TM_CF_60_w.o * player_TOI_perc_w, 
           weighted_TM_CA60  = TM_CA_60_w.o * player_TOI_perc_w, 
           weighted_TM_xGF60 = TM_xGF_60_w.o * player_TOI_perc_w, 
           weighted_TM_xGA60 = TM_xGA_60_w.o * player_TOI_perc_w, 
           
          # Adjust teammate for low TOI without player
           adj_weighted_TM_CF60  = ifelse(TM_TOI_w.o <= off_f_cut & TM_position == 1, 
                                         rel_adj(TM_CF_60_w.o, TM_TOI_w.o, CF60_t, off_f_cut, player_TOI_perc_w), 
                                         
                                         ifelse(TM_TOI_w.o <= off_d_cut & TM_position == 2, 
                                                rel_adj(TM_CF_60_w.o, TM_TOI_w.o, CF60_t, off_d_cut, player_TOI_perc_w), 
                                                weighted_TM_CF60)), 
           
           adj_weighted_TM_CA60  = ifelse(TM_TOI_w.o <= def_f_cut & TM_position == 1, 
                                         rel_adj(TM_CA_60_w.o, TM_TOI_w.o, CA60_t, def_f_cut, player_TOI_perc_w), 
                                         
                                         ifelse(TM_TOI_w.o <= def_d_cut & TM_position == 2, 
                                                rel_adj(TM_CA_60_w.o, TM_TOI_w.o, CA60_t, def_d_cut, player_TOI_perc_w), 
                                                weighted_TM_CA60)), 
           
           adj_weighted_TM_xGF60 = ifelse(TM_TOI_w.o <= off_f_cut & TM_position == 1, 
                                          rel_adj(TM_xGF_60_w.o, TM_TOI_w.o, xGF60_t, off_f_cut, player_TOI_perc_w), 
                                          
                                          ifelse(TM_TOI_w.o <= off_d_cut & TM_position == 2, 
                                                 rel_adj(TM_xGF_60_w.o, TM_TOI_w.o, xGF60_t, off_d_cut, player_TOI_perc_w), 
                                                 weighted_TM_xGF60)), 
           
           adj_weighted_TM_xGA60 = ifelse(TM_TOI_w.o <= def_f_cut & TM_position == 1, 
                                          rel_adj(TM_xGA_60_w.o, TM_TOI_w.o, xGA60_t, def_f_cut, player_TOI_perc_w), 
                                          
                                          ifelse(TM_TOI_w.o <= def_d_cut & TM_position == 2, 
                                                 rel_adj(TM_xGA_60_w.o, TM_TOI_w.o, xGA60_t, def_d_cut, player_TOI_perc_w), 
                                                 weighted_TM_xGA60))
          ) %>% 
    data.frame()
  
  return(rel_TM_metrics)
}
rel_source_EV <- fun.rel_source(TM_data = rel_TM_combos_EV, 
                                games_data = on_ice_EV, 
                                position_data = player_position, 
                                year = "20172018")


# Relative to Teammate - Initial & TOI Adjust
fun.rel_teammate <- function(data, position_data, teams_data) {
  rel_TM_impact <- data %>% 
    group_by(player, season) %>%
    summarise(TOI   = first(TOI_p),
              CF60  = first(CF60_p),
              CA60  = first(CA60_p),
              xGF60 = first(xGF60_p),
              xGA60 = first(xGA60_p),
              
              perc_total = sum(player_TOI_perc_w),
              
              weighted_TM_CF_raw  = sum(weighted_TM_CF60),
              weighted_TM_CA_raw  = sum(weighted_TM_CA60),
              weighted_TM_xGF_raw = sum(weighted_TM_xGF60),
              weighted_TM_xGA_raw = sum(weighted_TM_xGA60), 
              
              adj_weighted_TM_CF_raw = sum(adj_weighted_TM_CF60),
              adj_weighted_TM_CA_raw = sum(adj_weighted_TM_CA60),
              adj_weighted_TM_xGF_raw = sum(adj_weighted_TM_xGF60),
              adj_weighted_TM_xGA_raw = sum(adj_weighted_TM_xGA60)
              ) %>% 
    mutate(w_TM_CF60  = weighted_TM_CF_raw / perc_total,
           w_TM_CA60  = weighted_TM_CA_raw / perc_total, 
           w_TM_xGF60 = weighted_TM_xGF_raw / perc_total, 
           w_TM_xGA60 = weighted_TM_xGA_raw / perc_total, 
           
           adj_w_TM_CF60  = adj_weighted_TM_CF_raw / perc_total, 
           adj_w_TM_CA60  = adj_weighted_TM_CA_raw / perc_total, 
           adj_w_TM_xGF60 = adj_weighted_TM_xGF_raw / perc_total, 
           adj_w_TM_xGA60 = adj_weighted_TM_xGA_raw / perc_total, 
           
           rel_CF60_TM  = CF60 - w_TM_CF60, 
           rel_CA60_TM  = CA60 - w_TM_CA60, 
           rel_xGF60_TM = xGF60 - w_TM_xGF60, 
           rel_xGA60_TM = xGA60 - w_TM_xGA60, 
           
           adj_rel_CF60_TM  = CF60 - adj_w_TM_CF60,
           adj_rel_CA60_TM  = CA60 - adj_w_TM_CA60, 
           adj_rel_xGF60_TM = xGF60 - adj_w_TM_xGF60, 
           adj_rel_xGA60_TM = xGA60 - adj_w_TM_xGA60, 
           
           CF_impact  = rel_CF60_TM * (TOI / 60), 
           CA_impact  = rel_CA60_TM * (TOI / 60), 
           xGF_impact = rel_xGF60_TM * (TOI / 60), 
           xGA_impact = rel_xGA60_TM * (TOI / 60), 
           
           adj_CF_impact  = adj_rel_CF60_TM * (TOI / 60), 
           adj_CA_impact  = adj_rel_CA60_TM * (TOI / 60), 
           adj_xGF_impact = adj_rel_xGF60_TM * (TOI / 60), 
           adj_xGA_impact = adj_rel_xGA60_TM * (TOI / 60), 
           
           rel_Cdiff60_TM  = rel_CF60_TM - rel_CA60_TM,
           rel_xGdiff60_TM = rel_xGF60_TM - rel_xGA60_TM, 
           
           adj_rel_Cdiff60_TM  = adj_rel_CF60_TM - adj_rel_CA60_TM,
           adj_rel_xGdiff60_TM = adj_rel_xGF60_TM - adj_rel_xGA60_TM, 
           
           rel_CF_TM_perc  = 100 * round(CF60 / (CF60 + CA60) - w_TM_CF60 / (w_TM_CF60 + w_TM_CA60) + .5, 4), 
           rel_xGF_TM_perc = 100 * round(xGF60 / (xGF60 + xGA60) - w_TM_xGF60 / (w_TM_xGF60 + w_TM_xGA60) + .5, 4), 
           
           adj_rel_CF_TM_perc  = 100 * round(CF60 / (CF60 + CA60) - adj_w_TM_CF60 / (adj_w_TM_CF60 + adj_w_TM_CA60) + .5, 4), 
           adj_rel_xGF_TM_perc = 100 * round(xGF60 / (xGF60 + xGA60) - adj_w_TM_xGF60 / (adj_w_TM_xGF60 + adj_w_TM_xGA60) + .5, 4), 
           
           Corsi_total_impact = CF_impact - CA_impact,
           xG_total_impact = xGF_impact - xGA_impact, 
           
           adj_Corsi_total_impact = adj_CF_impact - adj_CA_impact,
           adj_xG_total_impact = adj_xGF_impact - adj_xGA_impact
           ) %>% 
    left_join(., position_data, by = c("player")) %>% 
    left_join(., teams_data, by = c("player", "season")) %>% 
    
    # Select and Round
    select(player, position, season, Team, TOI, CF60, CA60, xGF60, xGA60, w_TM_CF60, w_TM_CA60, w_TM_xGF60, w_TM_xGA60, 
           adj_w_TM_CF60, adj_w_TM_CA60, adj_w_TM_xGF60, adj_w_TM_xGA60, rel_CF60_TM, rel_CA60_TM, rel_xGF60_TM, rel_xGA60_TM, 
           adj_rel_CF60_TM, adj_rel_CA60_TM, adj_rel_xGF60_TM, adj_rel_xGA60_TM, rel_Cdiff60_TM, rel_xGdiff60_TM,
           adj_rel_Cdiff60_TM, adj_rel_xGdiff60_TM, rel_CF_TM_perc, rel_xGF_TM_perc,adj_rel_CF_TM_perc, adj_rel_xGF_TM_perc,
           CF_impact, CA_impact, xGF_impact, xGA_impact, Corsi_total_impact, xG_total_impact, 
           adj_CF_impact, adj_CA_impact, adj_xGF_impact, adj_xGA_impact, adj_Corsi_total_impact, adj_xG_total_impact
           ) %>% 
    mutate_at(vars(w_TM_CF60, w_TM_CA60, adj_w_TM_CF60, adj_w_TM_CA60, rel_CF60_TM, rel_CA60_TM, adj_rel_CF60_TM, 
                   adj_rel_CA60_TM, rel_Cdiff60_TM, adj_rel_Cdiff60_TM, CF_impact:adj_xG_total_impact), 
              funs(round(., 2))
              ) %>% 
    mutate_at(vars(w_TM_xGF60, w_TM_xGA60, adj_w_TM_xGF60, adj_w_TM_xGA60, rel_xGF60_TM, rel_xGA60_TM, adj_rel_xGF60_TM, 
                   adj_rel_xGA60_TM, rel_xGdiff60_TM, adj_rel_xGdiff60_TM), 
              funs(round(., 3))
              ) %>% 
    data.frame()
  }
rel_TM_player <- fun.rel_teammate(data = rel_source_EV, 
                                  position_data = player_position, 
                                  teams_data = teams)


# Relative to Teammate - Team Adjust
fun.rel_teammate_adj <- function(data) {
  
  rel_TM_team_adj <- data %>% 
    mutate(tm_CF_center  = adj_w_TM_CF60 - mean(adj_w_TM_CF60), 
           tm_CA_center  = adj_w_TM_CA60 - mean(adj_w_TM_CA60), 
           tm_xGF_center = adj_w_TM_xGF60 - mean(adj_w_TM_xGF60), 
           tm_xGA_center = adj_w_TM_xGA60 - mean(adj_w_TM_xGA60), 
           
           n_rel_CF60  = round(CF60 - (tm_CF_center*.8 + mean(adj_w_TM_CF60)), 2), 
           n_rel_CA60  = round(CA60 - (tm_CA_center*.88 + mean(adj_w_TM_CA60)), 2), 
           n_rel_xGF60 = round(xGF60 - (tm_xGF_center*.81 + mean(adj_w_TM_xGF60)), 3), 
           n_rel_xGA60 = round(xGA60 - (tm_xGA_center*.85 + mean(adj_w_TM_xGA60)), 3), 
           
           t_adj_CF60  = n_rel_CF60 - adj_rel_CF60_TM, 
           t_adj_CA60  = n_rel_CA60 - adj_rel_CA60_TM, 
           t_adj_xGF60 = n_rel_xGF60 - adj_rel_xGF60_TM, 
           t_adj_xGA60 = n_rel_xGA60 - adj_rel_xGA60_TM, 
           
           n_rel_CF_TM_perc  = 100 * round(CF60/(CF60 + CA60) - 
                                             (tm_CF_center*.8 + mean(adj_w_TM_CF60)) / ((tm_CF_center*.8 + mean(adj_w_TM_CF60)) + (tm_CA_center*.88 + mean(adj_w_TM_CA60))) + .5, 4), 
           
           n_rel_xGF_TM_perc = 100 * round(xGF60/(xGF60 + xGA60) - 
                                             (tm_xGF_center*.81 + mean(adj_w_TM_xGF60)) / ((tm_xGF_center*.81 + mean(adj_w_TM_xGF60)) + (tm_xGA_center*.85 + mean(adj_w_TM_xGA60))) + .5, 4), 
           
           n_CF_impact  = round(n_rel_CF60*(TOI/60), 2), 
           n_CA_impact  = round(n_rel_CA60*(TOI/60), 2), 
           n_xGF_impact = round(n_rel_xGF60*(TOI/60), 3), 
           n_xGA_impact = round(n_rel_xGA60*(TOI/60), 3), 
           
           n_Corsi_total_impact = n_CF_impact - n_CA_impact, 
           n_xG_total_impact = n_xGF_impact - n_xGA_impact
           ) %>% 
    select(player, position, season, Team, TOI, 
           CF60, CA60, xGF60, xGA60, 
           t_adj_CF60:t_adj_xGA60,
           n_rel_CF60:n_xG_total_impact)
  }
rel_TM_player_adj <- fun.rel_teammate_adj(data = rel_TM_player)


# Quick qualifying
rel_TM_qual <- rel_TM_player_adj %>% 
  group_by(position) %>% 
  arrange(desc(TOI)) %>% 
  mutate(n = row_number(), 
         qual = 1 * ((position == 1 & n <= 390) | (position == 2 & n <= 210))
         ) %>% 
  filter(qual == 1) %>% 
  select(-c(qual, n))



