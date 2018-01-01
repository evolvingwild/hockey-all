#################################################################################
#####         Adjusted Plus/Minus - Goals         ||         01/01/18       #####
#################################################################################

library(glmnet); library(doMC); library(dplyr)

options(scipen = 999)

set.seed(123)


## Load Extra Data
#######################################
fun.position <- function() {
  
  player_position <- read.csv("player_position.csv", stringsAsFactors = FALSE)
  pos_upd <- data.frame(matrix(nrow = 2, ncol = 2))
  names(pos_upd) <- c("player", "position")
  pos_upd[1, 1] <- c("FRÉDÉRIC.ST-DENIS") 
  pos_upd[1, 2] <- 1
  pos_upd[2, 1] <- c("MICHAËL.BOURNIVAL")
  pos_upd[2, 2] <- 1
  player_position <- player_position %>% rbind(., pos_upd) %>% arrange(player)
  
  return(player_position)
}
player_position <- fun.position()
scoreadj <- read.csv("ScoreAdj.csv")
no_xg <- readRDS("no_xg.rds")
schedule_full <- readRDS("team_results_with1617_pl.rds")
schedule_full <- schedule_full %>% 
  filter(game_id <= 2016021230)

btb <- schedule_full %>% 
  select(game_id, home_btb, away_btb) %>% 
  mutate(home_btb = ifelse(home_btb > 1, 1, home_btb), 
         away_btb = ifelse(away_btb > 1, 1, away_btb))

## Objects - UPDATED event_types to match HTM names throughout (including functions below)
c("SHOT",  "GOAL") -> st.shot_events
c("SHOT", "GOAL", "MISS") -> st.fenwick_events
c("SHOT", "GOAL", "MISS", "BLOCK" ) -> st.corsi_events
c("3v3", "5v5", "4v4", "5v4", "4v5", 
  "5v3", "3v5", "4v3", "3v4", "5vE", 
  "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor() -> st.strength_states
c("5v5", "4v4", "3v3") %>% as.factor() -> st.even_strength
c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor() -> st.pp_strength
c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor() -> st.empty_net
#######################################



## ------------------------ ##
##          Setup           ##
## ------------------------ ##

# Cutoff
# F = 1127 min // D = 1372 min
# G = 2865 min, which is (1127 / 354) * 900 (which is 20 * 45min)
# 10-year Cutoff: ~1184 skaters // ~100 goalies (680 forwards / 386 defensemen / 116 goalies)
# 3-year Cutoff: F = 338.1 // D = 411.6


# Goalie Qualifying
fun.goalie_qual <- function(data, cutoff) {
  
  hold <- data %>% 
    filter(game_strength_state %in% st.even_strength)
  
  pbp_goalie_H <- hold %>% 
    group_by(home_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & home_goalie != 50) %>% 
    rename(player = home_goalie)
  
  pbp_goalie_A <- hold %>% 
    group_by(away_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & away_goalie != 50) %>% 
    rename(player = away_goalie)
  
  pbp_goalie <- pbp_goalie_A %>% 
    rbind(., pbp_goalie_H) %>% 
    group_by(player) %>% 
    summarise(TOI = sum(TOI/60)) %>% 
    mutate(qual = ifelse(TOI > cutoff, 1, 0)) %>% 
    select(player, qual)
}
goalie_qual <- fun.goalie_qual(pbp_full, 900)


# Find qualified players from games data - USE FOR GOALS REGRESSION
fun.qualified_GF <- function(data, goalie_data, f_cut, d_cut) {
  
  qualified <- data %>% 
    group_by(player) %>% 
    summarise(TOI = round(sum(TOI), 2)) %>% 
    left_join(., player_position) %>% 
    mutate(qual = ifelse(TOI > f_cut & position == 1, 1, 
                         ifelse(TOI > d_cut & position == 2, 1, 0))
           ) %>% 
    select(player, qual)
  
  qualified_return <- qualified %>% 
    rbind(., goalie_data) %>% 
    data.frame()
  
  return(qualified_return)
}
Qualified_GF <- fun.qualified_GF(games_full_EV, 
                                 goalie_qual, 
                                 #338.1, 411.6) # 3 year qualifying
                                 1127, 1372) # 10 year qualifying
rm(goalie_qual)


# Remove no_xG from pbp_full
v <- unique(pbp_full$game_id)
include <- v[!v %in% no_xg]
pbp_full <- pbp_full %>% filter(game_id %in% include)


# Prepare pbp (initial) - filter to EV, column select + join in btb
fun.pbp_prepare <- function(data) {
  
  pbp_part <- data %>% 
    filter(game_strength_state %in% st.even_strength, 
           event_type != "OFF", event_type != "PEND", event_type != "GEND", 
           event_type != "CHL", event_type != "GOFF", event_type != "EIEND", 
           event_type != "EISTR", event_type != "PENL", 
           game_period < 5
    ) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)), 
           event_length = ifelse(is.na(event_length), 0, event_length), 
           event_team = ifelse(is.na(event_team), 0, event_team), 
           home_zonestart = ifelse(is.na(home_zonestart), 0, home_zonestart)
    )
  
  pbp_part <- pbp_part %>% 
    select(game_id, 
           home_on_1:away_on_6, 
           home_goalie, away_goalie, 
           home_team, away_team, 
           event_length, 
           event_team, 
           event_type, 
           home_lead, 
           home_zonestart, 
           prob_goal
    )
  
  pbp_part <- left_join(pbp_part, btb, by = c("game_id"))
  
  pbp_part[is.na(pbp_part)] <- 0
  
  return(pbp_part)
}
pbp_part <- fun.pbp_prepare(pbp_full)

rm(pbp_full)
gc()


# Create data frame with all players, goalies, teams, and event_types and their respective IDs
fun.names_match <- function(sub_data2, qual_data) {
  
  # Skaters
  player_position <- player_position %>% 
    mutate(ID = row_number() + 10000)
  
  # Goalies
  fun.goalie_find <- function(data) {
    
    goalie_h <- data.frame(unique(data$home_goalie))
    names(goalie_h) <- c("goalie")
    
    goalie_a <- data.frame(unique(data$away_goalie))
    names(goalie_a) <- c("goalie")
    
    goalie_all <- rbind(goalie_h, goalie_a)
    
    goalie_all <- data.frame(unique(goalie_all$goalie))
    names(goalie_all) <- c("player")
    
    goalie_all <- goalie_all %>% 
      arrange(player)
    
    return(goalie_all)
  }
  goalies <- fun.goalie_find(sub_data2)
  
  goalies <- goalies %>% 
    mutate(position = NA, 
           ID = NA)
  
  goalies$position <- 3
  
  goalies <- goalies %>% 
    arrange(player) %>% 
    mutate(ID = row_number() + 20000)
  
  # Teams
  teams_str <- unique(na.omit(sub_data2$event_team))
  teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
  names(teams) <- c("player", "position", "ID")
  teams$player <- teams_str
  teams$position <- 4
  
  teams <- teams %>% 
    arrange(player) %>% 
    mutate(ID = row_number())
  
  # Event Type
  event_str <- unique(na.omit(sub_data2$event_type))
  event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
  names(event) <- c("player", "position", "ID")
  event$player <- event_str
  event$position <- 5
  
  event <- event %>% 
    arrange(player) %>%
    mutate(ID = row_number() + 100)
  
  # Combine
  all <- teams %>% 
    rbind(., player_position, goalies, event) %>% 
    filter(player != 0, 
           player != "TIMOTHY JR..THOMAS") %>% 
    left_join(., qual_data) %>% 
    mutate(qual = ifelse(is.na(qual), 0, qual))
  
  return(all)
}
names_match <- fun.names_match(pbp_part, Qualified_GF)


# Determine non-qualified players
exclude <- names_match %>% 
  filter(ID > 10000, qual == 0) %>% 
  select(ID)

exclude <- as.vector(exclude[, 1])


# Identify and save specific event type IDs for dummy function/creation below
st.corsi_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
st.fenwick_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
st.goal_ID <- names_match[which(names_match[, 1] %in% c("GOAL")), 3] 
st.fac_ID <- names_match[which(names_match[, 1] %in% c("FAC")), 3]


# Convert prepared pbp data frame to all numeric values
fun.IDs <- function(data) {
  
  # Home Players
  data$home_on_1 <- names_match$ID[match(data$home_on_1, names_match$player)]
  data$home_on_2 <- names_match$ID[match(data$home_on_2, names_match$player)]
  data$home_on_3 <- names_match$ID[match(data$home_on_3, names_match$player)]
  data$home_on_4 <- names_match$ID[match(data$home_on_4, names_match$player)]
  data$home_on_5 <- names_match$ID[match(data$home_on_5, names_match$player)]
  data$home_on_6 <- names_match$ID[match(data$home_on_6, names_match$player)]
  
  # Away Players
  data$away_on_1 <- names_match$ID[match(data$away_on_1, names_match$player)]
  data$away_on_2 <- names_match$ID[match(data$away_on_2, names_match$player)]
  data$away_on_3 <- names_match$ID[match(data$away_on_3, names_match$player)]
  data$away_on_4 <- names_match$ID[match(data$away_on_4, names_match$player)]
  data$away_on_5 <- names_match$ID[match(data$away_on_5, names_match$player)]
  data$away_on_6 <- names_match$ID[match(data$away_on_6, names_match$player)]
  
  # Goalies
  data$home_goalie <- names_match$ID[match(data$home_goalie, names_match$player)]
  data$away_goalie <- names_match$ID[match(data$away_goalie, names_match$player)]
  
  # Teams
  data$event_team <- names_match$ID[match(data$event_team, names_match$player)]
  data$home_team <- names_match$ID[match(data$home_team, names_match$player)]
  data$away_team <- names_match$ID[match(data$away_team, names_match$player)]
  
  # Event Type
  data$event_type <- names_match$ID[match(data$event_type, names_match$player)]
  
  # Make Empty Slots 0s
  data[is.na(data)] <- 0
  
  return(data)
}
pbp_part <- fun.IDs(pbp_part)

rm(games_full_EV)
gc()



## ------------------------ ##
##    Create APM Tables     ##
## ------------------------ ##

# GF60 - Vectorized!
fun.APMsparse_GF <- function(data) {
  
  ### Create Home Matrix
  print("home_matrix")
  
  test.H <- data %>% 
    mutate(home_zonestart = ifelse(event_type == st.fac_ID & home_zonestart == 3, 1, 0)
           ) %>% 
    group_by(game_id, 
             home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
             home_lead
             ) %>% 
    summarise(home_goalie = first(home_goalie), 
              away_goalie = first(away_goalie), 
              team = first(home_team), 
              length = sum(event_length),
              GF60 = round((sum(ifelse(event_type == st.goal_ID & event_team == home_team, 1, 0)) / length) * 3600, 2), 
              off_zonestart = sum(home_zonestart), 
              btb = first(home_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           is_home = 1) %>% 
    select(-c(game_id)) %>% 
    data.matrix()
  
  # Column Names
  print("get_names")
  
  # Retrieve player names
  groups_d <- unique(as.vector(test.H[, grep("_on_", colnames(test.H))]))
  # Remove Missing Slots & Remove Non-Qualified Players
  groups_d <- groups_d[!groups_d %in% 0]
  groups_d <- groups_d[!groups_d %in% exclude]
  # Remove Goalies for Offense groups
  groups_o <- groups_d[!groups_d %in% c(20000:30000)]
  # Order Smallest to Largest
  groups_d <- sort(groups_d, decreasing = FALSE)
  groups_o <- sort(groups_o, decreasing = FALSE)
  
  
  ### Home Offense
  print("home_offense")
  
  # Determine Columns
  tmp <- lapply(groups_o, function(x, test.H)  which(test.H[, "home_on_1"] == x | 
                                                       test.H[, "home_on_2"] == x |
                                                       test.H[, "home_on_3"] == x |
                                                       test.H[, "home_on_4"] == x |
                                                       test.H[, "home_on_5"] == x |
                                                       test.H[, "home_on_6"] == x), test.H = test.H)
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_o)))
  # Rename
  colnames(dummies_o) <- groups_o
  colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
  
  
  ### Home Defense
  print("home_defense")
  
  # Determine Columns
  tmp <- lapply(groups_d, function(x, test.H)  which((test.H[, "away_on_1"] == x | 
                                                        test.H[, "away_on_2"] == x |
                                                        test.H[, "away_on_3"] == x |
                                                        test.H[, "away_on_4"] == x |
                                                        test.H[, "away_on_5"] == x |
                                                        test.H[, "away_on_6"] == x) & test.H[, "home_goalie"] != x), test.H = test.H)
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_d)))
  # Rename
  colnames(dummies_d) <- groups_d
  colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
  
  # Combine
  test_sparse.H <- cbind(test.H, dummies_o, dummies_d)
  
  rm(test.H)
  gc()
  
  ##----------------------##
  
  ### Create Away Matrix
  print("away_matrix")
  
  test.A <- data %>% 
    mutate(home_zonestart = ifelse(event_type == st.fac_ID & home_zonestart == 1, 1, 0), 
           home_lead = -1*home_lead
           ) %>% 
    group_by(game_id, 
             home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
             home_lead
             ) %>% 
    summarise(home_goalie = first(home_goalie), 
              away_goalie = first(away_goalie), 
              team = first(away_team), 
              length = sum(event_length),
              GF60 = round((sum(ifelse(event_type == st.goal_ID & event_team == away_team, 1, 0)) / length) * 3600, 2), 
              off_zonestart = sum(home_zonestart), 
              btb = first(away_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           is_home = 0) %>% 
    select(-c(game_id)) %>% 
    data.matrix()
  
  
  ### Away Offense
  print("away_offense")
  
  # Determine Columns
  tmp <- lapply(groups_o, function(x, test.A)  which(test.A[, "away_on_1"] == x | 
                                                       test.A[, "away_on_2"] == x |
                                                       test.A[, "away_on_3"] == x |
                                                       test.A[, "away_on_4"] == x |
                                                       test.A[, "away_on_5"] == x |
                                                       test.A[, "away_on_6"] == x), test.A = test.A)
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_o)))
  # Rename
  colnames(dummies_o) <- groups_o
  colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
  
  ### Away Defense
  print("away_defense")
  # Determine Columns
  tmp <- lapply(groups_d, function(x, test.A)  which((test.A[, "home_on_1"] == x | 
                                                        test.A[, "home_on_2"] == x |
                                                        test.A[, "home_on_3"] == x |
                                                        test.A[, "home_on_4"] == x |
                                                        test.A[, "home_on_5"] == x |
                                                        test.A[, "home_on_6"] == x) & test.A[, "away_goalie"] != x), test.A = test.A)
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_d)))
  # Rename
  colnames(dummies_d) <- groups_d
  colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
  
  # Combine
  test_sparse.A <- cbind(test.A, dummies_o, dummies_d)
  
  rm(test.A)
  gc()
  
  
  #####  Big Join  #####
  print("combine")
  
  test_all <- rbind(test_sparse.H, test_sparse.A)
  
  rm(test_sparse.H, test_sparse.A)
  gc()
  
  return(test_all)
}
APM <- fun.APMsparse_GF(pbp_part)



## ------------------------ ##
##          Models          ##
## ------------------------ ##

# Cleanup
GF60_l <- list(APM[, 19])
length_l <- list(APM[, 18])

length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace"))
GF60 <- unlist(rapply(GF60_l, f = function(x) ifelse(is.nan(x), 1, x), how = "replace"))

APM_g <- APM[, -c(1:13, 15:19)]

rm(APM, GF60_l, length_l)
gc()


# Cross Validation / Ridge Regression
CV_results <- cv.glmnet(APM_g, 
                        GF60, 
                        weights = length, 
                        alpha = 0, 
                        nfolds = 10, 
                        parallel = TRUE)
gc()

lambda_min <- CV_results$lambda.min

ridge <- glmnet(APM_g, 
                GF60, 
                family = c("gaussian"), 
                weights = length, 
                alpha = 0, 
                lambda = lambda_min)



## ------------------------ ##
##      Retrieve Names      ##
## ------------------------ ##

fun.APM_bind <- function(model_data, names_data) {
  
  # Retrieve Coefficients
  APM <- data.frame(as.matrix(coef(model_data, s = lambda_min)))
  APM_names <- dimnames(coef(ridge))[[1]]
  APM_test <- cbind(APM_names, APM)
  
  # Remove .d / .o suffixes
  APM_test_d <- APM_test %>% 
    filter(grepl(".d", APM_names), APM_names != "is_home") %>% 
    mutate(APM_names = gsub(".d", "", APM_names)) %>% 
    rename(Def = X1)
  
  APM_test_o <- APM_test %>% 
    filter(grepl(".o", APM_names), APM_names != "is_home") %>% 
    mutate(APM_names = gsub(".o", "", APM_names)) %>% 
    rename(Off = X1)
  
  # Join
  APM_all <- APM_test_d %>% 
    left_join(., APM_test_o) %>% 
    mutate(APM = Off - Def) %>% 
    select(APM_names, Off, Def, APM)
  
  APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
  
  return(APM_all)
}
APM_initial <- fun.APM_bind(ridge, names_match)

APM <- APM_initial %>% 
  rename(player = APM_names) %>% 
  left_join(., names_match, by = c("player"))

TOI_full <- games_full_EV %>% 
  group_by(player) %>% 
  mutate(n = n()) %>% 
  summarise(TOI = sum(TOI), 
            Games = first(n)) %>% 
  mutate(TOI.GP = round(TOI/Games, 2))

APM_join <- left_join(APM, TOI_full)

# GF APM Chart
APM_GF <- APM_join %>% 
  filter(!is.na(player)) %>% 
  mutate(O_impact = Off * (TOI / 60), 
         D_impact = Def * (TOI / 60), 
         GF_impact = O_impact - D_impact, 
         Off = ifelse(Off != 0 & position == 3, 0, Off), 
         APM = ifelse(player == "MARC-ANDRE.FLEURY", Off - Def, APM), 
         TOI = round(TOI, 2)
         ) %>% 
  select(player, position, TOI, TOI.GP, Off:APM, O_impact:GF_impact) %>% 
  mutate_at(vars(Off:APM), funs(round(., 3))) %>% 
  mutate_at(vars(O_impact:GF_impact), funs(round(., 2))) %>% 
  rename(GPM = APM)

APM_GF[is.na(APM_GF)] <- 0







