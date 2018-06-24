#################################################################################
#####         PHWA Power Rankings          ||             06/23/18          #####
#################################################################################

library(RCurl); library(dplyr)

options(scipen = 999)

set.seed(123)

## Objects
standardize <- function(metric) {
  
  x <- (metric - mean(metric)) / sd(metric)
  
}


## --------------------- ##
##   Load / Clean Data   ##
## --------------------- ##

###########################

# Get data from github:
votes_hart <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/Hart%20Trophy.csv"), 
                       stringsAsFactors = F)

votes_norris <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/Norris%20Trophy.csv"), 
                         stringsAsFactors = F)

votes_selke <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/Selke%20Trophy.csv"), 
                        stringsAsFactors = F)

votes_calder <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/Calder%20Trophy.csv"), 
                         stringsAsFactors = F)

votes_byng <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/Lady%20Byng%20Trophy.csv"), 
                       stringsAsFactors = F)

calder_players <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/calder_players.csv"), 
                           stringsAsFactors = F)



## Clean Vote Tables
names(votes_hart) <- c("order", "last_name", "first_name", "org", "vote_1", "vote_2", "vote_3", "vote_4", "vote_5")

votes_hart <- votes_hart %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(",.*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub("\\..*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(toupper(gsub(" ", ".", .))))


names(votes_norris) <- c("order", "last_name", "first_name", "org", "vote_1", "vote_2", "vote_3", "vote_4", "vote_5")

votes_norris <- votes_norris %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(",.*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub("\\..*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(toupper(gsub(" ", ".", .)))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(ifelse(. == "PK.SUBBAN", "P.K..SUBBAN", .)))


names(votes_selke) <- c("order", "last_name", "first_name", "org", "vote_1", "vote_2", "vote_3", "vote_4", "vote_5")

votes_selke <- votes_selke %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(",.*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub("\\..*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(toupper(gsub(" ", ".", .)))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(ifelse(. == "EVGENII.DADONOV", "EVGENIY.DADONOV", .)))


names(votes_byng) <- c("order", "last_name", "first_name", "org", "vote_1", "vote_2", "vote_3", "vote_4", "vote_5")

votes_byng <- votes_byng %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(",.*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(" Columbus Blue Jackets", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub("\\..*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(toupper(gsub(" ", ".", .)))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(ifelse(. == "EVGENII.DADONOV", "EVGENIY.DADONOV", .)))


names(votes_calder) <- c("order", "last_name", "first_name", "org", "vote_1", "vote_2", "vote_3", "vote_4", "vote_5")

votes_calder <- votes_calder %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub(",.*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(gsub("\\..*", "", .))) %>% 
  mutate_at(vars(vote_1:vote_5), funs(toupper(gsub(" ", ".", .))))


calder_players <- (calder_players %>% filter(player != "SEBASTIAN.AHO") %>% mutate(player = ifelse(player == "JOEL.ERIKSSON.EK", "JOEL.ERIKSSON EK", player)))$player




## Get data from GitHub
corsica_GAR_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/war_ratings_2018-04-06.csv"), 
                             stringsAsFactors = F)

corsica_all_sit_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/corsica_all_sit_1718.csv"), 
                                 stringsAsFactors = F)

corsica_rel_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/corsica_rel_1718.csv"), 
                             stringsAsFactors = F)

star_ratings_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/star_ratings_1718.csv"), 
                              stringsAsFactors = F)

corsica_goalies_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/corsica_goalies_1718.csv"), 
                                 stringsAsFactors = F)

NST_rel_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/NST_rel_1718.csv"), 
                         stringsAsFactors = F)

href_1718 <- read.csv(text = getURL("https://raw.githubusercontent.com/evolvingwild/hockey-all/master/NHL_Awards/hockey_ref_1718.csv"), 
                      stringsAsFactors = F)


# Clean / modify data
corsica_GAR_1718 <- corsica_GAR_1718 %>% 
  filter(Season == "2017-2018") %>% 
  rename(player = Player, 
         position = Position, 
         C_GAR = GAR, 
         C_GAR_D = DGAR
         ) %>% 
  mutate(Season = gsub("-", "", Season), 
         position = ifelse(position == "F", 1, 2))


corsica_all_sit_1718 <- corsica_all_sit_1718 %>% 
  mutate(Season = gsub("-", "", Season), 
         iPENT_60 = -1 * (iPENT / TOI) * 60) %>% 
  rename(player = Player)


corsica_rel_1718 <- corsica_rel_1718 %>% 
  filter(Player != "Player") %>% 
  mutate_at(vars(GP:Rel.ZSR), funs(suppressWarnings(as.numeric(.)))) %>% 
  mutate_at(vars(GP:Rel.ZSR), funs(ifelse(is.na(.), 0, .))) %>% 
  rename(player = Player) %>% 
  mutate(player = toupper(gsub("\\s", ".", player)), 
         rel_TM_CF_impact = (RelT.CF.60 / 60) * TOI, 
         rel_TM_CA_impact = (RelT.CA.60 / 60) * TOI, 
         rel_TM_C_impact =  rel_TM_CF_impact - rel_TM_CA_impact,
         
         rel_TM_xGF_impact = (RelT.xGF.60 / 60) * TOI, 
         rel_TM_xGA_impact = (RelT.xGA.60 / 60) * TOI, 
         rel_TM_xG_impact =  rel_TM_xGF_impact - rel_TM_xGA_impact, 
         
         # New Corsica name updates 
         player = ifelse(player == "CALVIN.DE.HAAN", "CALVIN.DE HAAN", player), 
         player = ifelse(player == "JACOB.DE.LA.ROSE", "JACOB.DE LA ROSE", player), 
         player = ifelse(player == "JAMES.VAN.RIEMSDYK", "JAMES.VAN RIEMSDYK", player), 
         player = ifelse(player == "JOEL.ERIKSSON.EK", "JOEL.ERIKSSON EK", player), 
         player = ifelse(player == "MICHAEL.DEL.ZOTTO", "MICHAEL.DEL ZOTTO", player), 
         player = ifelse(player == "PHILLIP.DI.GIUSEPPE", "PHILLIP.DI GIUSEPPE", player), 
         player = ifelse(player == "TREVOR.VAN.RIEMSDYK", "TREVOR.VAN RIEMSDYK", player)
         ) %>% 
  mutate_if(is.numeric, funs(round(., 2)))


star_ratings_1718 <- star_ratings_1718 %>% 
  select(player, position, SR) %>% 
  mutate_if(is.numeric, funs(round(., 2)))


corsica_goalies_1718 <- corsica_goalies_1718 %>% 
  rename(player = Player) %>% 
  mutate(player = toupper(gsub("\\s", ".", player))) %>% 
  left_join(., select(star_ratings_1718, -(position)), by = "player")


NST_rel_1718 <- NST_rel_1718 %>% 
  select(-c(X)) %>% 
  rename(player = Player) %>% 
  mutate(player = toupper(gsub("\\s", ".", player)), 
         rel_HDCF_impact = (HDCF.60.Rel / 60) * TOI, 
         rel_HDCA_impact = (HDCA.60.Rel / 60) * TOI, 
         rel_HD_impact = rel_HDCF_impact - rel_HDCA_impact, 
         
         player = ifelse(grepl("ALEXANDER", player), gsub("ALEXANDER", "ALEX", player), player), 
         player = ifelse(grepl("ALEXANDRE", player), gsub("ALEXANDRE", "ALEX", player), player), 
         player = ifelse(player == "BEN.ONDRUS", "BENJAMIN.ONDRUS", player),
         player = ifelse(player == "BRYCE.VAN.BRABANT", "BRYCE.VAN BRABANT", player),
         player = ifelse(player == "CALVIN.DE.HAAN", "CALVIN.DE HAAN", player), 
         player = ifelse(player == "CHASE.DE.LEO", "CHASE.DE LEO", player),
         player = ifelse(player == "CHRISTOPHER.DIDOMENICO", "CHRIS.DIDOMENICO", player), 
         player = ifelse(player == "CHRISTOPHER.TANEV", "CHRIS.TANEV", player), 
         player = ifelse(player == "DANIEL.CARCILLO", "DAN.CARCILLO", player),
         player = ifelse(player == "DANNY.O'REGAN", "DANIEL.O'REGAN", player), 
         player = ifelse(player == "DAVID.VAN.DER.GULIK", "DAVID.VAN DER GULIK", player),
         player = ifelse(player == "EVGENII.DADONOV", "EVGENY.DADONOV", player), 
         player = ifelse(player == "FREDDY.MODIN", "FREDRIK.MODIN", player),
         player = ifelse(player == "GREG.DE.VRIES", "GREG.DE VRIES", player),
         player = ifelse(player == "ILYA.ZUBOV", "ILJA.ZUBOV", player),
         player = ifelse(player == "JACOB.DE.LA.ROSE", "JACOB.DE LA ROSE", player), 
         player = ifelse(player == "JAMES.VAN.RIEMSDYK", "JAMES.VAN RIEMSDYK", player), 
         player = ifelse(player == "JEAN-FRANCOIS.JACQUES", "J-F.JACQUES", player),
         player = ifelse(player == "JAKOB.FORSBACKA.KARLSSON", "JAKOB.FORSBACKA KARLSSON", player),
         player = ifelse(player == "JIM.DOWD", "JAMES.DOWD", player),
         player = ifelse(player == "JEFF.HAMILTON", "JEFFREY.HAMILTON", player),
         player = ifelse(player == "JEFF.PENNER", "JEFFREY.PENNER", player),
         player = ifelse(player == "JOEL.ERIKSSON.EK", "JOEL.ERIKSSON EK", player), 
         player = ifelse(player == "MARK.VAN.GUILDER", "MARK.VAN GUILDER", player),
         player = ifelse(player == "MARTIN.ST..LOUIS", "MARTIN.ST. LOUIS", player),
         player = ifelse(player == "MARTIN.ST.PIERRE", "MARTIN.ST PIERRE", player),
         player = ifelse(player == "MICHAEL.CAMMALLERI", "MIKE.CAMMALLERI", player), 
         player = ifelse(player == "MICHAEL.DAL.COLLE", "MICHAEL.DAL COLLE", player), 
         player = ifelse(player == "MICHAEL.DEL.ZOTTO", "MICHAEL.DEL ZOTTO", player), 
         player = ifelse(player == "MIKE.VERNACE", "MICHAEL.VERNACE", player),
         player = ifelse(player == "MIKE.YORK", "MICHAEL.YORK", player),
         player = ifelse(player == "MIKE.VAN.RYN", "MIKE.VAN RYN", player),
         player = ifelse(player == "MITCHELL.MARNER", "MITCH.MARNER", player),
         player = ifelse(player == "PA.PARENTEAU", "P.A..PARENTEAU", player),
         player = ifelse(player == "PHILLIP.DI.GIUSEPPE", "PHILLIP.DI GIUSEPPE", player), 
         player = ifelse(player == "STEFAN.DELLA.ROVERE", "STEFAN.DELLA ROVERE", player),
         player = ifelse(player == "STEPHANE.DA.COSTA", "STEPHANE.DA COSTA", player),
         player = ifelse(player == "TJ.GALIARDI", "T.J..GALIARDI", player),
         player = ifelse(player == "TOBY.ENSTROM", "TOBIAS.ENSTROM", player), 
         player = ifelse(player == "TREVOR.VAN.RIEMSDYK", "TREVOR.VAN RIEMSDYK", player), 
         player = ifelse(player == "ZACK.FITZGERALD", "ZACH.FITZGERALD", player), 
         
         # NST specific
         player = ifelse(player == "MATTHEW.BENNING", "MATT.BENNING", player), 
         player = ifelse(player == "SEBASTIAN.AHO" & TOI < 400, "5EBASTIAN.AHO", player)
         ) %>% 
  select(player, TOI, HDCF.60.Rel, HDCA.60.Rel, rel_HDCF_impact:rel_HD_impact) %>% 
  mutate_if(is.numeric, funs(round(., 2)))


href_1718 <- href_1718 %>% 
  rename(player = Player) %>% 
  mutate(player = gsub(",.*", "", player), 
         player = toupper(gsub("\\s", ".", player))
         ) %>% 
  group_by(player, Pos) %>% 
  summarise_at(vars(GP:PS), funs(sum)) %>% 
  ungroup() %>% 
  mutate(player = ifelse(grepl("ALEXANDER", player), gsub("ALEXANDER", "ALEX", player), player), 
         player = ifelse(grepl("ALEXANDRE", player), gsub("ALEXANDRE", "ALEX", player), player), 
         player = ifelse(player == "BEN.ONDRUS", "BENJAMIN.ONDRUS", player),
         player = ifelse(player == "BRYCE.VAN.BRABANT", "BRYCE.VAN BRABANT", player),
         player = ifelse(player == "CALVIN.DE.HAAN", "CALVIN.DE HAAN", player), 
         player = ifelse(player == "CHASE.DE.LEO", "CHASE.DE LEO", player),
         player = ifelse(player == "CHRISTOPHER.DIDOMENICO", "CHRIS.DIDOMENICO", player), 
         player = ifelse(player == "CHRISTOPHER.TANEV", "CHRIS.TANEV", player), 
         player = ifelse(player == "DANIEL.CARCILLO", "DAN.CARCILLO", player),
         player = ifelse(player == "DANNY.O'REGAN", "DANIEL.O'REGAN", player), 
         player = ifelse(player == "DAVID.VAN.DER.GULIK", "DAVID.VAN DER GULIK", player),
         player = ifelse(player == "EVGENI.DADONOV", "EVGENY.DADONOV", player), 
         player = ifelse(player == "FREDDY.MODIN", "FREDRIK.MODIN", player),
         player = ifelse(player == "GREG.DE.VRIES", "GREG.DE VRIES", player),
         player = ifelse(player == "ILYA.ZUBOV", "ILJA.ZUBOV", player),
         player = ifelse(player == "JACOB.DE.LA.ROSE", "JACOB.DE LA ROSE", player), 
         player = ifelse(player == "JAMES.VAN.RIEMSDYK", "JAMES.VAN RIEMSDYK", player), 
         player = ifelse(player == "JEAN-FRANCOIS.JACQUES", "J-F.JACQUES", player),
         player = ifelse(player == "JAKOB.FORSBACKA.KARLSSON", "JAKOB.FORSBACKA KARLSSON", player),
         player = ifelse(player == "JIM.DOWD", "JAMES.DOWD", player),
         player = ifelse(player == "JEFF.HAMILTON", "JEFFREY.HAMILTON", player),
         player = ifelse(player == "JEFF.PENNER", "JEFFREY.PENNER", player),
         player = ifelse(player == "JOEL.ERIKSSON.EK", "JOEL.ERIKSSON EK", player), 
         player = ifelse(player == "MARK.VAN.GUILDER", "MARK.VAN GUILDER", player),
         player = ifelse(player == "MARTIN.ST..LOUIS", "MARTIN.ST. LOUIS", player),
         player = ifelse(player == "MARTIN.ST.PIERRE", "MARTIN.ST PIERRE", player),
         player = ifelse(player == "MICHAEL.CAMMALLERI", "MIKE.CAMMALLERI", player), 
         player = ifelse(player == "MICHAEL.DAL.COLLE", "MICHAEL.DAL COLLE", player), 
         player = ifelse(player == "MICHAEL.DEL.ZOTTO", "MICHAEL.DEL ZOTTO", player), 
         player = ifelse(player == "MIKE.VERNACE", "MICHAEL.VERNACE", player),
         player = ifelse(player == "MIKE.YORK", "MICHAEL.YORK", player),
         player = ifelse(player == "MIKE.VAN.RYN", "MIKE.VAN RYN", player),
         player = ifelse(player == "MITCHELL.MARNER", "MITCH.MARNER", player),
         player = ifelse(player == "PA.PARENTEAU", "P.A..PARENTEAU", player),
         player = ifelse(player == "PHILLIP.DI.GIUSEPPE", "PHILLIP.DI GIUSEPPE", player), 
         player = ifelse(player == "STEFAN.DELLA.ROVERE", "STEFAN.DELLA ROVERE", player),
         player = ifelse(player == "STEPHANE.DA.COSTA", "STEPHANE.DA COSTA", player),
         player = ifelse(player == "TJ.GALIARDI", "T.J..GALIARDI", player),
         player = ifelse(player == "TOBY.ENSTROM", "TOBIAS.ENSTROM", player), 
         player = ifelse(player == "TREVOR.VAN.RIEMSDYK", "TREVOR.VAN RIEMSDYK", player), 
         player = ifelse(player == "ZACK.FITZGERALD", "ZACH.FITZGERALD", player), 
         
         # hockey reference specific
         player = ifelse(player == "SEBASTIAN.AHO" & Pos == "D", "5EBASTIAN.AHO", player),  
         player = ifelse(player == "JOSEPH.MORROW", "JOE.MORROW", player), 
         player = ifelse(player == "JONATHON.MERRILL", "JON.MERRILL", player), 
         player = ifelse(player == "JON.MARCHESSAULT", "JONATHAN.MARCHESSAULT", player), 
         player = ifelse(player == "MATHEW.DUMBA", "MATT.DUMBA", player), 
         player = ifelse(player == "MATTHEW.NIETO", "MATT.NIETO", player), 
         player = ifelse(player == "NIKOLAI.KULEMIN", "NIKOLAY.KULEMIN", player), 
         player = ifelse(player == "SAMUEL.BLAIS", "SAMMY.BLAIS", player), 
         player = ifelse(player == "T.J..BRODIE", "TJ.BRODIE", player), 
         player = ifelse(player == "ANTHONY.DEANGELO", "TONY.DEANGELO", player)
         ) %>% 
  data.frame()

  
###########################


## --------------------------- ##
##   Prepare stats for joing   ##
## --------------------------- ##

#################################

## Hart Combine
metrics_hart <- corsica_GAR_1718 %>% 
  select(player, position, Team, TOI, C_GAR) %>% 
  left_join(., corsica_all_sit_1718 %>% select(player, GS), by = "player") %>% 
  left_join(., star_ratings_1718 %>% select(player, SR), by = "player") %>% 
  left_join(., href_1718 %>% select(player, PS), by = "player") %>% 
  left_join(., corsica_rel_1718 %>% select(player, rel_TM_C_impact, rel_TM_xG_impact), by = "player") %>% 
  left_join(., NST_rel_1718 %>% select(player, rel_HD_impact), by = "player") %>% 
  mutate_at(vars(C_GAR, GS, PS, SR, rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact), funs(standardize(.))) %>% 
  mutate(avg_rel =  (rel_TM_C_impact + rel_TM_xG_impact + rel_HD_impact) / 3, 
         avg_score = (C_GAR + GS + PS + SR + avg_rel) / 5) %>% 
  select(-c(rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact)) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  arrange(desc(avg_score)) %>% 
  data.frame()


# Norris Combine
metrics_norris <- corsica_GAR_1718 %>% 
  select(player, position, C_GAR) %>% 
  left_join(., corsica_all_sit_1718 %>% select(player, GS), by = "player") %>% 
  left_join(., star_ratings_1718 %>% select(player, SR), by = "player") %>% 
  left_join(., href_1718 %>% select(player, PS), by = "player") %>% 
  left_join(., corsica_rel_1718 %>% select(player, rel_TM_C_impact, rel_TM_xG_impact), by = "player") %>% 
  left_join(., NST_rel_1718 %>% select(player, rel_HD_impact), by = "player") %>% 
  group_by(position) %>% 
  mutate_at(vars(C_GAR, GS, PS, SR, rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact), funs(standardize(.))) %>% 
  mutate(avg_rel =  (rel_TM_C_impact + rel_TM_xG_impact + rel_HD_impact) / 3, 
         avg_score = (C_GAR + GS + PS + SR + avg_rel) / 5) %>% 
  select(-c(rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact)) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  filter(position == 2) %>% 
  arrange(desc(avg_score)) %>% 
  data.frame()


# Calder Combine
metrics_calder <- corsica_GAR_1718 %>% 
  select(player, position, C_GAR) %>% 
  left_join(., corsica_all_sit_1718 %>% select(player, GS), by = "player") %>% 
  left_join(., star_ratings_1718 %>% select(player, SR), by = "player") %>% 
  left_join(., href_1718 %>% select(player, PS), by = "player") %>% 
  left_join(., corsica_rel_1718 %>% select(player, rel_TM_C_impact, rel_TM_xG_impact), by = "player") %>% 
  left_join(., NST_rel_1718 %>% select(player, rel_HD_impact), by = "player") %>% 
  mutate_at(vars(C_GAR, GS, PS, SR, rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact), funs(standardize(.))) %>% 
  mutate(avg_rel =  (rel_TM_C_impact + rel_TM_xG_impact + rel_HD_impact) / 3, 
         avg_score = (C_GAR + GS + PS + SR + avg_rel) / 5) %>% 
  select(-c(rel_TM_C_impact, rel_TM_xG_impact, rel_HD_impact)) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  arrange(desc(avg_score)) %>% 
  data.frame()


# Selke Combine
metrics_selke <- corsica_GAR_1718 %>% 
  select(player, position, C_GAR_D) %>% 
  left_join(., href_1718 %>% select(player, DPS), by = "player") %>% 
  left_join(., corsica_rel_1718 %>% select(player, rel_TM_CA_impact, rel_TM_xGA_impact), by = "player") %>% 
  left_join(., NST_rel_1718 %>% select(player, rel_HDCA_impact), by = "player") %>% 
  mutate_at(vars(rel_TM_CA_impact, rel_TM_xGA_impact, rel_HDCA_impact), funs(-1 * .)) %>% 
  filter(position == 1) %>% 
  mutate_at(vars(C_GAR_D, DPS, rel_TM_CA_impact, rel_TM_xGA_impact, rel_HDCA_impact), funs(standardize(.))) %>% 
  mutate(avg_score = (C_GAR_D + DPS + rel_TM_CA_impact + rel_TM_xGA_impact + rel_HDCA_impact) / 5) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  arrange(desc(avg_score)) %>% 
  data.frame()


#################################




## --------------------- ##
##   Rank PHWA Writers   ##
## --------------------- ##

###########################

# HART: Add in player z-scores for each vote
combine_hart <- votes_hart %>% 
  left_join(., metrics_hart %>% select(player, avg_score) %>% rename(vote_1 = player, avg_score_1 = avg_score), by = "vote_1") %>% 
  left_join(., metrics_hart %>% select(player, avg_score) %>% rename(vote_2 = player, avg_score_2 = avg_score), by = "vote_2") %>% 
  left_join(., metrics_hart %>% select(player, avg_score) %>% rename(vote_3 = player, avg_score_3 = avg_score), by = "vote_3") %>% 
  left_join(., metrics_hart %>% select(player, avg_score) %>% rename(vote_4 = player, avg_score_4 = avg_score), by = "vote_4") %>% 
  left_join(., metrics_hart %>% select(player, avg_score) %>% rename(vote_5 = player, avg_score_5 = avg_score), by = "vote_5")

# Impute average vote value for goalie votes
avg_vote <- mean(c(na.omit(combine_hart[, 10:14])[, 1], na.omit(combine_hart[, 10:14])[, 2], 
                   na.omit(combine_hart[, 10:14])[, 3], na.omit(combine_hart[, 10:14])[, 4]))

combine_hart <- combine_hart %>% 
  mutate_at(vars(avg_score_1:avg_score_5), funs(ifelse(is.na(.), round(avg_vote, 2), .))) %>% 
  mutate(score_hart = (avg_score_1 * 10 + avg_score_2 * 7 + avg_score_3 * 5 + avg_score_4 * 3 + avg_score_5 * 1) / 26)

scores_hart <- combine_hart %>% 
  mutate(writer = paste0(first_name, " ", last_name)) %>% 
  select(writer, org, score_hart)


# NORRIS: Add in player z-scores for each vote
combine_norris <- votes_norris %>% 
  left_join(., metrics_norris %>% select(player, avg_score) %>% rename(vote_1 = player, avg_score_1 = avg_score), by = "vote_1") %>% 
  left_join(., metrics_norris %>% select(player, avg_score) %>% rename(vote_2 = player, avg_score_2 = avg_score), by = "vote_2") %>% 
  left_join(., metrics_norris %>% select(player, avg_score) %>% rename(vote_3 = player, avg_score_3 = avg_score), by = "vote_3") %>% 
  left_join(., metrics_norris %>% select(player, avg_score) %>% rename(vote_4 = player, avg_score_4 = avg_score), by = "vote_4") %>% 
  left_join(., metrics_norris %>% select(player, avg_score) %>% rename(vote_5 = player, avg_score_5 = avg_score), by = "vote_5")

combine_norris <- combine_norris %>% 
  mutate(score_norris = (avg_score_1 * 10 + avg_score_2 * 7 + avg_score_3 * 5 + avg_score_4 * 3 + avg_score_5 * 1) / 26)

scores_norris <- combine_norris %>% 
  mutate(writer = paste0(first_name, " ", last_name)) %>% 
  select(writer, org, score_norris)


# SELKE: Add in player z-scores for each vote
combine_selke <- votes_selke %>% 
  left_join(., metrics_selke %>% select(player, avg_score) %>% rename(vote_1 = player, avg_score_1 = avg_score), by = "vote_1") %>% 
  left_join(., metrics_selke %>% select(player, avg_score) %>% rename(vote_2 = player, avg_score_2 = avg_score), by = "vote_2") %>% 
  left_join(., metrics_selke %>% select(player, avg_score) %>% rename(vote_3 = player, avg_score_3 = avg_score), by = "vote_3") %>% 
  left_join(., metrics_selke %>% select(player, avg_score) %>% rename(vote_4 = player, avg_score_4 = avg_score), by = "vote_4") %>% 
  left_join(., metrics_selke %>% select(player, avg_score) %>% rename(vote_5 = player, avg_score_5 = avg_score), by = "vote_5")

combine_selke <- combine_selke %>% 
  mutate(score_selke = (avg_score_1 * 10 + avg_score_2 * 7 + avg_score_3 * 5 + avg_score_4 * 3 + avg_score_5 * 1) / 26)

scores_selke <- combine_selke %>% 
  mutate(writer = paste0(first_name, " ", last_name)) %>% 
  select(writer, org, score_selke)


# CALDER: Add in player z-scores for each vote
combine_calder <- votes_calder %>% 
  left_join(., metrics_calder %>% select(player, avg_score) %>% rename(vote_1 = player, avg_score_1 = avg_score), by = "vote_1") %>% 
  left_join(., metrics_calder %>% select(player, avg_score) %>% rename(vote_2 = player, avg_score_2 = avg_score), by = "vote_2") %>% 
  left_join(., metrics_calder %>% select(player, avg_score) %>% rename(vote_3 = player, avg_score_3 = avg_score), by = "vote_3") %>% 
  left_join(., metrics_calder %>% select(player, avg_score) %>% rename(vote_4 = player, avg_score_4 = avg_score), by = "vote_4") %>% 
  left_join(., metrics_calder %>% select(player, avg_score) %>% rename(vote_5 = player, avg_score_5 = avg_score), by = "vote_5")

combine_calder <- combine_calder %>% 
  mutate(score_calder = (avg_score_1 * 10 + avg_score_2 * 7 + avg_score_3 * 5 + avg_score_4 * 3 + avg_score_5 * 1) / 26)

scores_calder <- combine_calder %>% 
  mutate(writer = paste0(first_name, " ", last_name)) %>% 
  select(writer, org, score_calder)


# ALL COMBINE
total_scores <- scores_hart %>% 
  left_join(., scores_norris, by = c("writer", "org")) %>% 
  left_join(., scores_selke, by = c("writer", "org")) %>% 
  left_join(., scores_calder, by = c("writer", "org")) %>% 
  mutate(total_score = (score_hart + score_norris + score_selke + score_calder) / 4) %>% 
  arrange(desc(total_score)) %>% 
  mutate_at(vars(score_hart:total_score), funs(round(., 2)))
  


###########################






### -- Depricated -- ###

# Max Scores
max_scores <- data.frame(max_hart = (join_hart[1, 2] * 10 + join_hart[2, 2] * 7 + join_hart[3, 2] * 5 + join_hart[4, 2] * 3 + join_hart[5, 2] * 1) / 26, 
                         max_norris = (join_norris[1, 2] * 10 + join_norris[2, 2] * 7 + join_norris[3, 2] * 5 + join_norris[4, 2] * 3 + join_norris[5, 2] * 1) / 26, 
                         max_selke = (join_selke[1, 2] * 10 + join_selke[2, 2] * 7 + join_selke[3, 2] * 5 + join_selke[4, 2] * 3 + join_selke[5, 2] * 1) / 26, 
                         max_calder = (join_calder[1, 2] * 10 + join_calder[2, 2] * 7 + join_calder[3, 2] * 5 + join_calder[4, 2] * 3 + join_calder[5, 2] * 1) / 26 
                         )

max_scores <- max_scores %>% 
  mutate(max_total = (max_hart + max_norris + max_selke + max_calder) / 4) %>% 
  mutate_if(is.numeric, funs(round(., 2)))






