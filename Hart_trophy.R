#################################################################################
#####       Hart trophy model              ||           03/31/17            #####
#################################################################################


# Required packages
library(rvest); library(car); library(doMC); library(dplyr)


## ----------------------------- ##
##    Scrape hockey-reference    ##
## ----------------------------- ##

# Compile Hart award voting results - '05 season omitted
awards_table <- foreach(i = c(1:45, 47:58), .combine = rbind) %do% { 
  
  Sys.sleep(1)
  
  year <- 1959 + i
  print(year, quote = F)
  
  url <- read_html(paste0("https://www.hockey-reference.com/awards/voting-", year, ".html"))
  
  awards <- url %>% 
    html_table(header = T) %>% 
    data.frame()
  
  names(awards) <- awards[1, ]
  awards <- awards[-1, ]
  
  clean_awards <- awards %>% 
    mutate(season = year) %>% 
    rename(SV_perc = `SV%`, 
           plus_minus = `+/-`, 
           ties_OT_SO = `T/O`, 
           Vote_perc = `Vote%`
           ) %>% 
    mutate_at(vars(Votes, Vote_perc, G:PS), funs(as.numeric(.)))
  
  }


# Compile team standings - '05 season omitted
standings_table <- foreach(i = c(1:45, 47:58), .combine = rbind) %do% { 
  
  Sys.sleep(1)
  
  year <- 1959 + i
  print(year, quote = F)
  
  url <- read_html(paste0("https://www.hockey-reference.com/leagues/NHL_", year, ".html"))
  
  standings <- html_table(url, fill = T)
  
  # Deal with League changes
  if(year >= 1975) { 
    
    results <- rbind(standings[[1]], standings[[2]])
    colnames(results)[1] <- "Team"
    
  } else { 
    
    results <- standings[[1]]
    colnames(results)[1] <- "Team"
    
    }
  
  # Clean data
  clean_results <- results %>% 
    filter(Team != GP) %>% 
    select(Team, GP, GF, GA, PTS, `PTS%`) %>% 
    rename(PTS_perc = `PTS%`) %>% 
    mutate(playoffs = 1 * (grepl("[*]", Team) == T), 
           season = year,
           Team = gsub("[*]", "", Team) 
           ) %>% 
    mutate_at(vars(GP:PTS_perc), funs(as.numeric(.))) %>% 
    arrange(desc(PTS_perc))
  
  }



## ----------- ##
##    Model    ##
## ----------- ##  

# Deal with team names for binding
Team_names <- unique(sort(standings_table$Team))
Team_codes <- c("ANA", "ARI", "ATF", "ATL", "BOS", "BUF", "CGY", "CGS", "CAR", "CBH", "CHI", "CLB", "COL", "CLR", "CBJ", 
                "DAL", "DET", "EDM", "FLA", "HAR", "KCS", "LAK", "MDA", "MNS", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", 
                "OKS", "OTT", "PHI", "PHX", "PIT", "QUE", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG", "WIN", "VEG")

Team_df <- data.frame(Team = c(Team_names, "Winnipeg Jets", "Vegas Golden Knights"), 
                      Tm = Team_codes)


# Join Hart awards voting and team results
Awards_All <- awards_table %>% 
  left_join(., Team_df, by = "Tm") %>% 
  left_join(., standings_table, by = c("Team", "season")) %>% 
  rename(Points = PTS.x, 
         Team_points = PTS.y) %>% 
  mutate(Pos = ifelse(Pos == "D", "D", 
                      ifelse(Pos == "G", "G", "F"))
         ) %>% 
  mutate_at(vars(Age, Pos, Place, playoffs), funs(as.factor(.)))


# Model - current version is what was used
awards_model <- lm(Vote_perc ~ Age 
                   + Pos 
                   #+ Points 
                   + plus_minus 
                   + PS 
                   + PTS_perc, 
                    data = filter(Awards_All, 
                                  Pos != "G", 
                                  Vote_perc > 5))

summary(awards_model)
vif(awards_model)



## --------------------- ##
##   Predict for '17-18  ##
## --------------------- ##

### Scrape '17-18 skater stats
url <- read_html("https://www.hockey-reference.com/leagues/NHL_2018_skaters.html")

skaters_1718 <- url %>% 
  html_table(header = T) %>% 
  data.frame()

# Clean data
names(skaters_1718) <- skaters_1718[1, ]
skaters_1718 <- skaters_1718[-1, ]
skaters_1718 <- skaters_1718[, -c(13:15, 17:19)]

pred_1718 <- skaters_1718 %>%  
  filter(Rk != "Rk") %>% 
  mutate(Pos = ifelse(Pos == "D", "D", "F"), 
         season = 2018) %>%
  rename(plus_minus = `+/-`) %>% 
  select(Player:plus_minus, PS, season) %>% 
  mutate_at(vars(GP:plus_minus, PS), funs(as.numeric(.)))



### Scrape '17-18 team standings 
url <- read_html("https://www.hockey-reference.com/leagues/NHL_2018.html")

standings_1718 <- html_table(url, fill = T)

# Clean data (same as in the foreach loop above)
results_1718 <- rbind(standings_1718[[1]], standings_1718[[2]])
colnames(results_1718)[1] <- "Team"

clean_results_1718 <- results_1718 %>% 
  filter(Team != GP) %>% 
  select(Team, GP, GF, GA, PTS, `PTS%`) %>% 
  rename(PTS_perc = `PTS%`) %>% 
  mutate(playoffs = 1 * (grepl("[*]", Team) == T), 
         season = 2018,
         Team = gsub("[*]", "", Team) 
         ) %>% 
  mutate_at(vars(GP:PTS_perc), funs(as.numeric(.))) %>% 
  arrange(desc(PTS_perc))


### Join skater stats and team standings for '17-18
Awards_1718 <- pred_1718 %>% 
  left_join(., Team_df, by = "Tm") %>% 
  left_join(., clean_results_1718, by = c("Team", "season")) %>% 
  rename(Points = PTS.x, 
         Team_points = PTS.y) %>% 
  filter(Tm != "TOT") %>% 
  mutate(Age = ifelse(Age == 18, 19, 
                      ifelse(Age >= 39, 39, Age))) %>% 
  mutate_at(vars(Age, Pos, playoffs), funs(as.factor(.)))


# PREDICT
Awards_1718$pred_vote_perc <- predict.lm(object = awards_model, 
                                         newdata = Awards_1718)

# Final table
display_1718 <- Awards_1718 %>% 
  select(Player, Tm, Age, Pos, plus_minus, PS, PTS_perc, pred_vote_perc) %>% 
  mutate(pred_vote_perc = round(pred_vote_perc, 1)) %>% 
  arrange(desc(pred_vote_perc))



### --- END --- ###

