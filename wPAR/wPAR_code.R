#################################################################################
#####            wPAR Source            ||            08/02/17              #####
#################################################################################


require(dplyr); options(scipen=999)

# The below code will generate both the "wPAR.Final" table (season-by-season data for all players)
# and the "wPAR.career" table (career totals). Various other tables are created as well. 

# The "rawdata" .csv file can be found here:
# https://docs.google.com/spreadsheets/d/1jxCXQf054NCfnOfiwlImfbZDyCRopMxpOlCBWy-VZ-c/edit?usp=sharing
 


# LOAD DATA
rawdata <- read.csv("rawdata.csv")
rawdataXtra <- rawdata %>% select(Player.NHL, Season, Mid.Age, Total.GS)


# Create Points Rates
PAAratesF <- rawdata %>% group_by(Season, Position.N) %>% filter(Position.N == 1) %>% 
  summarise(F.Pts.Rate = sum(Points)/sum(Total.TOI))
PAAratesD <- rawdata %>% group_by(Season, Position.N) %>% filter(Position.N == 2) %>% 
  summarise(D.Pts.Rate = sum(Points)/sum(Total.TOI))
PAArates <- data.frame(cbind(PAAratesF, PAAratesD)) %>% select(Season, F.Pts.Rate, D.Pts.Rate)


# Create Empty Weight table
wPAAweights <- data.frame(matrix(nrow = 10, ncol =  13))
names(wPAAweights) <- c("Season",	"G",	"A1",	"A2",	
                        "Tango", "ixG",	"Tango.ixG", "G.ixG", "G.Tango",	
                        "Pen.Taken", "Pen.Drawn", "FO.Diff", "Rel.Corsi.Diff")
wPAAweights$Season <- c(20072008, 20082009,	20092010,	20102011,	20112012,	
                        20122013,	20132014,	20142015,	20152016,	20162017)

 
# Regression Weights
baseweightsF <- wPAAweights %>% 
  mutate(G = 0.7474,
         A1 = 0.6194,
         A2  = 0.7565,
         Tango = 0.0324,
         ixG =  1.3534,
         Tango.ixG = -0.0241,
         G.ixG = -0.4890,
         G.Tango = 0.0098,
         Pen.Taken = 0.17,
         Pen.Drawn = 0.17,
         FO.Diff = 0.013, #Shuckers' Number
         Rel.Corsi.Diff = 0.0375) 

baseweightsD <- wPAAweights %>% 
  mutate(G = 0.4867,
         A1 = 0.4900,
         A2  = 0.4726,
         Tango = 0.0255,
         ixG = 1.9206,
         Tango.ixG = -0.0101,
         G.ixG = -0.7636,
         G.Tango = -0.0167,
         Pen.Taken = 0.17,
         Pen.Drawn = 0.17,
         FO.Diff = 0.013, #(removed later)
         Rel.Corsi.Diff = 0.0237) 


# Components Raw
fun.wPraw <- function(data, weight) {
  hold <- data %>% mutate(countsraw = 
                            (G*weight[Season.N,2]) + 
                            (A1*weight[Season.N,3]) + 
                            (A2*weight[Season.N,4]) + 
                            (Tango*weight[Season.N,5]) + 
                            (ixG*weight[Season.N,6]) + 
                            ((Tango + ixG)*weight[Season.N,7]) + 
                            ((G + ixG)*weight[Season.N,8]) + 
                            ((G + Tango)*weight[Season.N,9]),
                          diff = ((RelCDiff*(EV.TOI/60))*weight[Season.N,13]),
                          penT = (iPENT*weight[Season.N, 10]),
                          penD = (iPEND*weight[Season.N, 11]),
                          FACE = (FO.Diff*weight[Season.N, 12])) %>% 
    select(Player.NHL, Season, Season.N, Position.N, Team, GP, Total.TOI, EV.TOI, countsraw, diff, penT, penD, FACE)
  return(hold)
}
wPrawF <- fun.wPraw(rawdata, baseweightsF)
wPrawD <- fun.wPraw(rawdata, baseweightsD)

fun.comprate <- function(data, position) {
  data %>% group_by(Season) %>% filter(Position.N == position) %>%
    summarise(sumTTOI = sum(Total.TOI), 
              sumEVTOI= sum(EV.TOI), 
              sumCRAW = sum(countsraw), 
              sumDIFF = sum(diff), 
              sumPENT = sum(penT), 
              sumPEND = sum(penD), 
              sumFACE = sum(FACE)) %>% 
    mutate(countrate = sumCRAW/sumTTOI,
           diffrate = sumDIFF/sumEVTOI, 
           penTrate = sumPENT/sumTTOI,
           penDrate = sumPEND/sumTTOI, 
           FACErate = sumFACE/sumTTOI) %>% 
    select(Season, countrate, diffrate, penTrate, penDrate, FACErate) %>% 
    data.frame() -> rate
  return(rate)
}
Frate <- fun.comprate(wPrawF, 1)
Drate <- fun.comprate(wPrawD, 2)


# Components Raw -> Above Average
fun.wPAA <- function(data, rate, position) {
  hold <- data %>% filter(Position.N == position)  %>% 
    mutate(Pcountrate = countsraw / Total.TOI, 
           Pdiffrate = diff/EV.TOI, 
           PENT = penT/Total.TOI, 
           PEND = penD/Total.TOI, 
           FO = FACE/Total.TOI, 
           countAA = (Pcountrate - rate[Season.N, 2])*Total.TOI, 
           diffAA = (Pdiffrate - rate[Season.N, 3])*EV.TOI,
           penTAA = -1*((PENT - rate[Season.N, 4])*Total.TOI),
           penDAA = (PEND - rate[Season.N, 5])*Total.TOI, 
           faceAA = (FO - rate[Season.N, 6])*Total.TOI, 
           wPAA = countAA + diffAA + penTAA + penDAA + faceAA) %>% 
    select(Player.NHL, Season, Position.N, Team, GP, Total.TOI, EV.TOI, countAA, diffAA, penTAA, penDAA, faceAA, wPAA)
  return(hold)
}
FAA <- fun.wPAA(wPrawF, Frate, 1)           
DAA <- fun.wPAA(wPrawD, Drate, 2); DAA$faceAA <- 0
allwPAA <- rbind(FAA, DAA)

# Adding in replacement level (placeholder)
wPAR <- allwPAA %>% 
  mutate(reppoints = ifelse(Position.N == 1, Total.TOI*(.681/60), Total.TOI*(.275/60)),
         wPAR = wPAA + reppoints)


# Scale
fun.PAA <- function(data, position, colnum) {
  data %>% filter(Position.N == position)  %>% 
    mutate(ptrate = Points / Total.TOI) %>% 
    mutate(PAA = (ptrate - PAArates[Season.N,colnum])*Total.TOI) %>% 
    select(Player.NHL, Season, Position.N, Team, GP, Total.TOI, EV.TOI, Points, PAA)
}
FPAA <- fun.PAA(rawdata, 1, 2)
DPAA <- fun.PAA(rawdata, 2, 3)
allPAA <- rbind(FPAA, DPAA)
allPAA <- allPAA %>% arrange(Player.NHL)


# Creating the Scale
fun.createscale <- function() {
  posPAA.F <- allPAA %>% group_by(Season) %>% filter(Position.N == 1, PAA > 0) %>% summarise(posPAA.F = sum(PAA))
  posPAA.D <- allPAA %>% group_by(Season) %>% filter(Position.N == 2, PAA > 0) %>% summarise(posPAA.D = sum(PAA)) %>% select(posPAA.D)
  poswPAA.F <- wPAR %>% group_by(Season) %>% filter(Position.N == 1, wPAA > 0) %>% summarise(poswPAA.F = sum(wPAA)) %>% select(poswPAA.F)
  poswPAA.D <- wPAR %>% group_by(Season) %>% filter(Position.N == 2, wPAA > 0) %>% summarise(poswPAA.D = sum(wPAA)) %>% select(poswPAA.D)
  
  wPscale.ind <- cbind(posPAA.F, poswPAA.F, posPAA.D, poswPAA.D)
  wPscale.ind <- wPscale.ind %>% mutate(wp.F = round(posPAA.F / poswPAA.F,3), wp.D = round(posPAA.D / poswPAA.D,3))
  return(wPscale.ind)
}
wPscale.ind <- fun.createscale()




##############   SCALE   ############## 

# Actual wPAA weights - SCALE
fun.scaleweights <- function(position) { 
  if(position == "F") {
    Act4F <- round(baseweightsF[, c(5,7,9,12,13)]   * wPscale.ind[,6],4)
    Act3F <- round(baseweightsF[, c(2:4,6,8,10:11)] * wPscale.ind[,6],3)
    ActualwPAAweightsF <- cbind(Act4F, Act3F)
    ActualwPAAweightsF <- ActualwPAAweightsF %>% 
      mutate(Season = baseweightsF[,1]) %>% 
      select(Season, G, A1, A2, Tango, ixG, Tango.ixG, G.ixG, G.Tango, 
             Pen.Taken, Pen.Drawn, FO.Diff, Rel.Corsi.Diff)
    return(ActualwPAAweightsF)
  }
  else {
    Act4D <- round(baseweightsD[, c(5,7,9,12,13)]   * wPscale.ind[,7],4)
    Act3D <- round(baseweightsD[, c(2:4,6,8,10:11)] * wPscale.ind[,7],3)
    ActualwPAAweightsD <- cbind(Act4D, Act3D)
    ActualwPAAweightsD <- ActualwPAAweightsD %>% 
      mutate(Season = baseweightsD[,1]) %>% 
      select(Season, G, A1, A2, Tango, ixG, Tango.ixG, G.ixG, G.Tango, 
             Pen.Taken, Pen.Drawn, FO.Diff, Rel.Corsi.Diff)
    return(ActualwPAAweightsD)
  }
}
ActualwPAAweightsF <- fun.scaleweights("F")
ActualwPAAweightsD <- fun.scaleweights("D")


# Components Raw - SCALE
wPrawscaleF <- fun.wPraw(rawdata, ActualwPAAweightsF)
wPrawscaleD <- fun.wPraw(rawdata, ActualwPAAweightsD)
Fratescale <- fun.comprate(wPrawscaleF, 1)
Dratescale <- fun.comprate(wPrawscaleD, 2)


# Components Raw -> Above Average - SCALE
FAAs <- fun.wPAA(wPrawscaleF, Fratescale, 1)           
DAAs <- fun.wPAA(wPrawscaleD, Dratescale, 2)
DAAs$faceAA <- 0
allwPAAs <- rbind(FAAs, DAAs)
allwPAAs <- allwPAAs %>% arrange(Player.NHL) 


# Creating Final Table
wPAR.Final <- allwPAAs %>% 
  mutate(Qualified = ifelse(Season != 20122013 & Position.N == "1" & Total.TOI/82 > 4.6, 1, 
                            ifelse(Season != 20122013 & Position.N == "2" & Total.TOI/82 > 6, 1,
                                   ifelse(Season == 20122013 & Position.N == "1" & Total.TOI/48 > 4.6, 1, 
                                          ifelse(Season == 20122013 & Position.N == "2" & Total.TOI/48 > 6, 1, 0)))),
         #Add Rep Points
         reppoints = ifelse(Position.N == "1", Total.TOI*(.681/60), Total.TOI*(.275/60)),
         wPAR = wPAA + reppoints,
         wPAR.60 = round((wPAR/Total.TOI)*60,3),
         PAA = allPAA$PAA) %>% 
  left_join(., rawdataXtra, by = c("Player.NHL", "Season"))
wPAR.Final$Mid.Age <- as.numeric(as.character(wPAR.Final$Mid.Age))


# Compiling Career Totals
fun.career <- function(data) {
  # Sorry
  hold <- data %>% group_by(Player.NHL) %>%  
    mutate(posnum = ifelse(Position.N == "1", 1, 2), scount = n())
  wPAR.career <- hold %>%  group_by(Player.NHL) %>%
    summarise(pos = ifelse(mean(posnum) == 1, "1", "2"), 
              Seasons = mean(scount), 
              sumGP = sum(GP), 
              Total.TOI = sum(Total.TOI), 
              EV.TOI = sum(EV.TOI), 
              countAA = round(sum(countAA),1), 
              diffAA = round(sum(diffAA),1), 
              penTAA = round(sum(penTAA),1), 
              penDAA = round(sum(penDAA),1), 
              faceAA = round(sum(faceAA),1), 
              wPAA = round(sum(wPAA),1), 
              wPAR = round(sum(wPAR),1), 
              #Per 60 / Misc.
              countAA60 = round(sum(countAA)/sum(EV.TOI)*60,3), 
              diffAA60 = round(sum(diffAA)/sum(EV.TOI)*60,3), 
              penTAA60 = round(sum(penTAA)/sum(Total.TOI)*60,3), 
              penDAA60 = round(sum(penDAA)/sum(Total.TOI)*60,3),
              faceAA60 = round(sum(faceAA)/sum(Total.TOI)*60,3), 
              wPAR60 = round(sum(wPAR)/sum(Total.TOI)*60,3), 
              Total.GS = sum(Total.GS), 
              PAA = round(sum(PAA),1), 
              PAA60 = round(sum(PAA)/sum(Total.TOI)*60,3)) %>% 
    arrange(desc(wPAR))
  return(wPAR.career)
}
wPAR.career <- fun.career(wPAR.Final)




###############          ----------- E N D -------------          ###############


