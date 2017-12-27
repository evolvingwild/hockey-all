#################################################################################
#####        Relative to Teammate     ||              12/26/17              #####
#################################################################################

library(dplyr)

options(scipen = 999)

# Create Score Adjust Data Frame / xG adjustment
scoreadj_corsi <- data.frame(matrix(nrow = 7, ncol = 3))
scoreadj_corsi[, 1] <- c(1, 2, 3, 4, 5, 6, 7)
scoreadj_corsi[, 2] <- c(0.840, 0.865, 0.898, 0.970, 1.052, 1.104, 1.138)
scoreadj_corsi[, 3] <- c(1.236, 1.186, 1.128, 1.032, 0.953, 0.914, 0.892)
colnames(scoreadj_corsi) <- c("home_lead", "home_corsi_adj", "away_corsi_adj")

xG_adj_h <- .9468472
xG_adj_a <- 1.059477

## Objects
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
st.even_strength <- as.factor(c("5v5", "4v4", "3v3"))



## ---------------------------------- ##
##       EV Rel_TM Shot Metrics       ##
## ---------------------------------- ##

fun.QoT_H <- function(data) {
  
  hold_player <- data %>% 
    summarise(Team = first(away_team), 
              TOI = sum(event_length)/60,
              GF = sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)), 
              GA = sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              CF = sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1*scoreadj_corsi[home_lead, 2]), 0)), 
              CA = sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1*scoreadj_corsi[home_lead, 3]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal*xG_adj_h, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal*xG_adj_a, 0)))
              )
  
  return(hold_player)
}
fun.QoT_A <- function(data) {
  
  hold_player <- data %>% 
    summarise(Team = first(away_team), 
              TOI = sum(event_length)/60,
              GF = sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              GA = sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)),
              CF = sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1*scoreadj_corsi[home_lead, 3]), 0)), 
              CA = sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1*scoreadj_corsi[home_lead, 2]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, prob_goal*xG_adj_a, 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, prob_goal*xG_adj_h, 0)))
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
fun.goalie_find <- function(data) {
  
  # Identifies goalies within a given pbp df 
  goalie_h <- data.frame(unique(data$home_goalie))
  names(goalie_h) <- c("goalie")
  
  goalie_a <- data.frame(unique(data$away_goalie))
  names(goalie_a) <- c("goalie")
  
  goalie_all <- rbind(goalie_h, goalie_a)
  goalie_all <- data.frame(unique(goalie_all$goalie))
  names(goalie_all) <- c("player")
  
  goalie_return <- goalie_all %>% 
    filter(!is.na(player)) %>% 
    mutate(is_goalie = 1)
  
  goalie_return$player <- as.character(goalie_return$player)
  
  return(goalie_return)
}

# Combine 
fun.teammate <- function(data, year) {
  
  # Filter pbp data
  hold <- data %>% 
    filter(game_strength_state %in% st.even_strength, game_period < 5, event_length < 900) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)),
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           )
  
  # Run Functions
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
    filter(!is.na(player), !is.na(teammate)) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  away_all <- away1 %>% 
    rbind(., away2, away3, away4, away5, away6) %>% 
    group_by(player, teammate, game_id, away_team) %>% 
    summarise_at(vars(TOI:xGA), funs(sum)) %>% 
    filter(!is.na(player), !is.na(teammate)) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  
  # Remove Goalies
  print("remove_goalies", quote = F)
  
  goalie_remove <- fun.goalie_find(data)
  
  testH <- home_all[!(home_all$player %in% goalie_remove$player),]
  testH <- testH[!(testH$teammate %in% goalie_remove$player),]
  
  testA <- away_all[!(away_all$player %in% goalie_remove$player),]
  testA <- testA[!(testA$teammate %in% goalie_remove$player),]
  
  testH$is_home <- 1
  testA$is_home <- 0
  
  
  # Combine
  print("combine", quote = F)
  
  ALL <- testH %>% 
    rbind(., testA) %>% 
    mutate(season = year) %>% 
    group_by(player, teammate, game_id, Team) %>% 
    mutate_at(vars(TOI, xGF, xGA), funs(round(., 3))) %>% 
    select(player, teammate, game_id, season, Team, is_home, TOI, GF, GA, CF, CA, xGF, xGA) %>% 
    arrange(player, teammate, game_id) %>% 
    data.frame()

  return(ALL)
}

rel_TM_1718_EV <- fun.teammate(pbp_data, "20172018")











