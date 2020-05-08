library(jsonlite)
library(tidyverse)
library(readxl)
library(Rfast)

# Load and Clean Mapping/ADP-----
nameID <- read.csv("data/nameid.csv")
nameID$Name <- as.character(nameID$Name)
nameID$playerid <- as.character(nameID$playerid)
adp <- read.csv("data/adp.csv")
adp$Player <- as.character(adp$Player)

# Load Projections ----
Proj_H <- read_xlsx("data/DraftTrackerTestDC.xlsx", sheet = "Combined")
Proj_P <- read_xlsx("data/DraftTrackerTestDC.xlsx", sheet = "Combined-P")

# Load Clean and Prep Draft Board From NFBC ----
url <- "https://draft.shgn.com/api/public/players/dp/1040"
draft <- fromJSON(url)
picks <- draft$pick
draft <- draft[,c("f",
                  "l",
                  "playerId",
                  "e"
)]
picks <- picks[,c("p",
                  "tu",
                  "po")]
draft <- cbind(draft,picks)
draft$Name <- paste(draft$f,draft$l,sep = " ")
draft <- draft[,c("Name",
                  "playerId",
                  "e",
                  "p",
                  "tu",
                  "po")]
draft <- left_join(draft,nameID,by = "Name")
draft <- draft %>% 
  filter(!grepl('Place Holder', Name))

# Merge With Projections ----
draft <- left_join(draft,Proj_H[,c(2,4:9)],by = "playerid")
draft <- left_join(draft,Proj_P[,c(2,4:10)], by = "playerid")

draft <- draft %>% 
  filter(AB > 30 | IP > 10)

draft[, 8:20][is.na(draft[, 8:20])] <- 0

# Cleanup and Breakout Available Players ----
available <- draft %>% filter(is.na(tu))
available$count <- str_count(available$e, "/")
two <- available %>% filter(count > 0)
three <- available %>% filter(count > 1)
four <- available %>% filter(count > 2)
available$e <- gsub("/.*","",available$e)
two$e <- gsub(".*/","",two$e)
three$e <- gsub(".*/ (.+) /.*", "\\1", three$e)
if (nrow(four) > 0)
{
  four$e <- "OF"
}
available <- rbind(available,two,three,four)
available <- available[,c("Name",
                          "playerid",
                          "e",
                          "AB",
                          "H.x",     
                          "HR",
                          "R",
                          "RBI",
                          "SB",
                          "W",
                          "SV",
                          "IP",
                          "H.y",
                          "ER",      
                          "SO",
                          "BB")]

available$e <- ifelse(available$SV < 2, available$e, "RP")
available[,4:16] <- round(available[,4:16],2)

# Prep Drafted Player For Team Analysis ----
drafted <- draft %>% filter(!is.na(tu))
drafted <- drafted[,c("Name",
                      "playerid",
                      "po",
                      "p",
                      "tu",
                      "AB",
                      "H.x",     
                      "HR",
                      "R",
                      "RBI",
                      "SB",
                      "W",
                      "SV",
                      "IP",
                      "H.y",
                      "ER",      
                      "SO",
                      "BB")]

# Seperate Current Roster and Develop team Level Stats ----
my_team <- drafted %>%
  filter(p == 15)

my_team <- my_team[order(factor(my_team$po, levels = c("C", "1B","2B", "SS", "3B", "OF", "CI", "MI","UT","P", "RES"))),]

Hitting_Stats <- my_team %>%
  filter(po != "P") %>%
  filter(po != "RES") %>%
  summarise(
    Count_H = n(),
    AB = sum(AB),
    H = sum(H.x),
    AVG = round(sum(H.x)/sum(AB),3),
    R = round(mean(R),2),
    RBI = round(mean(RBI),2),
    HR = round(mean(HR),2),
    SB = round(mean(SB),2),
  )

Pitching_Stats <- my_team %>%
  filter(po == "P") %>%
  summarise(
    Count_P = n(),
    IP = sum(IP),
    ER = sum(ER),
    HBB = sum(H.y)+sum(BB),
    ERA = round((sum(ER)*9)/sum(IP),3),
    WHIP = round((sum(H.y)+sum(BB))/sum(IP),3),
    W = round(mean(W),2),
    SO = round(mean(SO),2),
    SV = round(sum(SV)/sum(my_team$SV > 5),2),
  )

team_stats <- cbind(Hitting_Stats,Pitching_Stats)
team_stats$Type <- "Current Roster"
team_stats <- team_stats[,c("Type",
                            "Count_H",
                            "AB",
                            "H",
                            "AVG",
                            "R",
                            "RBI",
                            "HR",
                            "SB",
                            "Count_P",
                            "IP",     
                            "ER",
                            "HBB",
                            "ERA",
                            "WHIP",
                            "W",
                            "SO",
                            "SV")]


# Record Target Information ----
targets <- data.frame(
  Type = "Targets",
  AVG = 0.2692,
  R = 79.8,
  RBI = 79.8,
  HR = 25.9,
  SB = 9.2,
  ERA = 3.808,
  WHIP = 1.195,
  SO = 165.8,
  W = 10.7,
  SV = 25)

# Calculate Remaining Needs and Adjustment Weights ----
remains <- data.frame(
  Type = "Remains",
  AVG = round(ifelse((7000-team_stats[1,"AB"] > 0) & (14-team_stats[1,"Count_H"] > 0),
                     ((targets[1,"AVG"]*500*14)-team_stats[1,"H"])/(7000-team_stats[1,"AB"]),
                     targets[1,"AVG"]),3),
  R = round(ifelse(14-team_stats[1,"Count_H"] > 0,
                   ((targets[1,"R"]*14)-(team_stats[1,"R"]*team_stats[1,"Count_H"]))/(14-team_stats[1,"Count_H"]),
                   targets[1,"R"]),2),
  RBI = round(ifelse(14-team_stats[1,"Count_H"] > 0,
                     ((targets[1,"RBI"]*14)-(team_stats[1,"RBI"]*team_stats[1,"Count_H"]))/(14-team_stats[1,"Count_H"]),
                     targets[1,"RBI"]),2),
  HR = round(ifelse(14-team_stats[1,"Count_H"] > 0,
                    ((targets[1,"HR"]*14)-(team_stats[1,"HR"]*team_stats[1,"Count_H"]))/(14-team_stats[1,"Count_H"]),
                    targets[1,"HR"]),2),
  SB = round(ifelse(14-team_stats[1,"Count_H"] > 0,
                    ((targets[1,"SB"]*14)-(team_stats[1,"SB"]*team_stats[1,"Count_H"]))/(14-team_stats[1,"Count_H"]),
                    targets[1,"SB"]),2),
  ERA = round(ifelse((1350-team_stats[1,"IP"] > 0) & (9-team_stats[1,"Count_P"] > 0),
                     ((((targets[1,"ERA"]*1350)/9)-team_stats[1,"ER"])*9)/(1350-team_stats[1,"IP"]),
                     targets[1,"ERA"]),3),
  WHIP = round(ifelse((1350-team_stats[1,"IP"] > 0) & (9-team_stats[1,"Count_P"] > 0),
                      (((targets[1,"WHIP"]*1350)-team_stats[1,"HBB"]))/(1350-team_stats[1,"IP"]),
                      targets[1,"WHIP"]),3),
  SO = round(ifelse(9-team_stats[1,"Count_P"] > 0,
                    ((targets[1,"SO"]*9)-(team_stats[1,"SO"]*team_stats[1,"Count_P"]))/(9-team_stats[1,"Count_P"]),
                    targets[1,"SO"]),2),
  W = round(ifelse(9-team_stats[1,"Count_P"] > 0,
                   ((targets[1,"W"]*9)-(team_stats[1,"W"]*team_stats[1,"Count_P"]))/(9-team_stats[1,"Count_P"]),
                   targets[1,"W"]),2),
  SV = round(ifelse(3-sum(my_team$SV > 5) > 0,
                    ((targets[1,"SV"]*3)-(team_stats[1,"SV"]*sum(my_team$SV > 5)))/(3-sum(my_team$SV > 5)),
                    targets[1,"SV"]),2))

adjustments <- data.frame(
  AVG = ifelse(team_stats[1,"AVG"] != 0,
               round(team_stats[1,"AVG"]/remains[1,"AVG"],2)
               ,1),
  R = ifelse(team_stats[1,"R"] != 0,
             round(team_stats[1,"R"]/remains[1,"R"],2)
             ,1),
  RBI = ifelse(team_stats[1,"RBI"] != 0,
               round(team_stats[1,"RBI"]/remains[1,"RBI"],2)
               ,1),
  HR = ifelse(team_stats[1,"HR"] != 0,
              round(team_stats[1,"HR"]/remains[1,"HR"],2)
              ,1),
  SB = ifelse(team_stats[1,"SB"] != 0,
              round(team_stats[1,"SB"]/remains[1,"SB"],2)
              ,1),
  ERA = ifelse(team_stats[1,"ERA"] != 0,
               round(remains[1,"ERA"]/team_stats[1,"ERA"],2)
               ,1),
  WHIP = ifelse(team_stats[1,"WHIP"] != 0,
                round(remains[1,"WHIP"]/team_stats[1,"WHIP"],2)
                ,1),
  SO = ifelse(team_stats[1,"SO"] != 0,
              round(team_stats[1,"SO"]/remains[1,"SO"],2)
              ,1),
  W = ifelse(team_stats[1,"W"] != 0,
             round(team_stats[1,"W"]/remains[1,"W"],2)
             ,1),
  SV = ifelse(team_stats[1,"SV"] != 0,
              round(team_stats[1,"SV"]/remains[1,"SV"],2)
              ,1)
)

total <- sum(adjustments[,1:10])

weights <- data.frame(
  AVG = adjustments[1,"AVG"]/total,
  R = adjustments[1,"R"]/total,
  RBI = adjustments[1,"RBI"]/total,
  HR = adjustments[1,"HR"]/total,
  SB = adjustments[1,"SB"]/total,
  ERA = adjustments[1,"ERA"]/total,
  WHIP = adjustments[1,"WHIP"]/total,
  SO = adjustments[1,"SO"]/total,
  W = adjustments[1,"W"]/total,
  SV = adjustments[1,"SV"]/total
)


# Adjust Projections For Relative Value Due to Needs ----
available_adjusted <- available
available_adjusted$AVG <- round((((available_adjusted$H.x/available_adjusted$AB)/remains[1,"AVG"])*available_adjusted$AB/
                                   ((7000-team_stats[1,"AB"])/ifelse((14-team_stats[1,"Count_H"] > 0),14-team_stats[1,"Count_H"] > 0,1)))/weights[1,"AVG"],2)
available_adjusted$HR <- round((available_adjusted$HR/remains[1,"HR"])/weights[1,"HR"],2)
available_adjusted$R <- round((available_adjusted$R/remains[1,"R"])/weights[1,"R"],2)
available_adjusted$RBI <- round((available_adjusted$RBI/remains[1,"RBI"])/weights[1,"RBI"],2)
available_adjusted$SB <- round((available_adjusted$SB/remains[1,"SB"])/weights[1,"SB"],2)

available_adjusted$ERA <- round(((1+(remains[1,"ERA"]-((available_adjusted$ER/available_adjusted$IP)*9))/remains[1,"ERA"])*
                                   ((available_adjusted$IP)/((1350-team_stats[1,"IP"])/ifelse(9-team_stats[1,"Count_P"] > 0,9-team_stats[1,"Count_P"],1))))/weights[1,"ERA"],2)

available_adjusted$WHIP <- round(((1+(remains[1,"WHIP"]-(((available_adjusted$H.y+available_adjusted$BB)/available_adjusted$IP)))/remains[1,"WHIP"])*
                                    ((available_adjusted$IP)/((1350-team_stats[1,"IP"])/ifelse(9-team_stats[1,"Count_P"] > 0,9-team_stats[1,"Count_P"],1))))/weights[1,"WHIP"],2)

available_adjusted$W <- round((available_adjusted$W/remains[1,"W"])/weights[1,"W"],2)
available_adjusted$SO <- round((available_adjusted$SO/remains[1,"SO"])/weights[1,"SO"],2)
available_adjusted$SV <- round((available_adjusted$SV/remains[1,"SV"])/weights[1,"SV"],2)

available_adjusted <- available_adjusted[,c(
  "Name",
  "playerid",
  "e",
  "AVG",
  "R",
  "RBI",
  "HR",
  "SB",
  "ERA",
  "WHIP",
  "W",
  "SO",
  "SV")]

available_adjusted[, 4:13][is.na(available_adjusted[, 4:13])] <- 0
available_adjusted$e <- gsub('\\s+', '', available_adjusted$e)


# Develop Positional Averages ----
hitters_averages <- available_adjusted %>%
  filter(e != "P") %>%
  filter(e != "RP") %>%
  group_by(e) %>%
  summarise(
    AVG = round(mean(AVG),2),
    R = round(mean(R),2),
    RBI = round(mean(RBI),2),
    HR = round(mean(HR),2),
    SB = round(mean(SB),2)
  )

pitchers_averages <- available_adjusted %>%
  filter(e == "P" | e == "RP") %>%
  group_by(e) %>%
  summarise(
    ERA = round(mean(ERA),2),
    WHIP = round(mean(WHIP),2),
    W = round(mean(W),2),
    SO = round(mean(SO),2),
    SV = round(mean(SV),2)
  )


# Calculate Overall Player Value ----
available_adjusted$Value <- round(ifelse(available_adjusted$e == "P",
                                         ((available_adjusted$ERA/pitchers_averages[pitchers_averages$e == "P","ERA"][[1]])+
                                            (available_adjusted$WHIP/pitchers_averages[pitchers_averages$e == "P","WHIP"][[1]])+
                                            (available_adjusted$W/pitchers_averages[pitchers_averages$e == "P","W"][[1]])+
                                            (available_adjusted$SO/pitchers_averages[pitchers_averages$e == "P","SO"][[1]])+
                                            (available_adjusted$SV/pitchers_averages[pitchers_averages$e == "P","SV"][[1]])+
                                            (available_adjusted$ERA + available_adjusted$WHIP + available_adjusted$W + available_adjusted$SO + available_adjusted$SV)-
                                            (rowMaxs(as.matrix(available_adjusted[,9:13]),value = TRUE)-rowMins(as.matrix(available_adjusted[,9:13]),value = TRUE))/2)*1.1,
                                         ifelse(available_adjusted$e == "RP",
                                                ((available_adjusted$ERA/pitchers_averages[pitchers_averages$e == "RP","ERA"][[1]])+
                                                   (available_adjusted$WHIP/pitchers_averages[pitchers_averages$e == "RP","WHIP"][[1]])+
                                                   (available_adjusted$W/pitchers_averages[pitchers_averages$e == "RP","W"][[1]])+
                                                   (available_adjusted$SO/pitchers_averages[pitchers_averages$e == "RP","SO"][[1]])+
                                                   (available_adjusted$SV/pitchers_averages[pitchers_averages$e == "RP","SV"][[1]])+
                                                   (available_adjusted$ERA + available_adjusted$WHIP + available_adjusted$W + available_adjusted$SO + available_adjusted$SV)-
                                                   (rowMaxs(as.matrix(available_adjusted[,9:13]),value = TRUE)-rowMins(as.matrix(available_adjusted[,9:13]),value = TRUE))/2)*1.5,
                                                ifelse(available_adjusted$e == "C",
                                                       ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "C","AVG"][[1]])+
                                                          (available_adjusted$R/hitters_averages[hitters_averages$e == "C","R"][[1]])+
                                                          (available_adjusted$RBI/hitters_averages[hitters_averages$e == "C","RBI"][[1]])+
                                                          (available_adjusted$HR/hitters_averages[hitters_averages$e == "C","HR"][[1]])+
                                                          (available_adjusted$SB/hitters_averages[hitters_averages$e == "C","SB"][[1]])+
                                                          (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                          (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                       ifelse(available_adjusted$e == "1B",
                                                              ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "1B","AVG"][[1]])+
                                                                 (available_adjusted$R/hitters_averages[hitters_averages$e == "1B","R"][[1]])+
                                                                 (available_adjusted$RBI/hitters_averages[hitters_averages$e == "1B","RBI"][[1]])+
                                                                 (available_adjusted$HR/hitters_averages[hitters_averages$e == "1B","HR"][[1]])+
                                                                 (available_adjusted$SB/hitters_averages[hitters_averages$e == "1B","SB"][[1]])+
                                                                 (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                 (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                              ifelse(available_adjusted$e == "2B",
                                                                     ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "2B","AVG"][[1]])+
                                                                        (available_adjusted$R/hitters_averages[hitters_averages$e == "2B","R"][[1]])+
                                                                        (available_adjusted$RBI/hitters_averages[hitters_averages$e == "2B","RBI"][[1]])+
                                                                        (available_adjusted$HR/hitters_averages[hitters_averages$e == "2B","HR"][[1]])+
                                                                        (available_adjusted$SB/hitters_averages[hitters_averages$e == "2B","SB"][[1]])+
                                                                        (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                        (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                                     ifelse(available_adjusted$e == "3B",
                                                                            ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "3B","AVG"][[1]])+
                                                                               (available_adjusted$R/hitters_averages[hitters_averages$e == "3B","R"][[1]])+
                                                                               (available_adjusted$RBI/hitters_averages[hitters_averages$e == "3B","RBI"][[1]])+
                                                                               (available_adjusted$HR/hitters_averages[hitters_averages$e == "3B","HR"][[1]])+
                                                                               (available_adjusted$SB/hitters_averages[hitters_averages$e == "3B","SB"][[1]])+
                                                                               (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                               (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                                            ifelse(available_adjusted$e == "SS",
                                                                                   ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "SS","AVG"][[1]])+
                                                                                      (available_adjusted$R/hitters_averages[hitters_averages$e == "SS","R"][[1]])+
                                                                                      (available_adjusted$RBI/hitters_averages[hitters_averages$e == "SS","RBI"][[1]])+
                                                                                      (available_adjusted$HR/hitters_averages[hitters_averages$e == "SS","HR"][[1]])+
                                                                                      (available_adjusted$SB/hitters_averages[hitters_averages$e == "SS","SB"][[1]])+
                                                                                      (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                                      (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                                                   ifelse(available_adjusted$e == "OF",
                                                                                          ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "OF","AVG"][[1]])+
                                                                                             (available_adjusted$R/hitters_averages[hitters_averages$e == "OF","R"][[1]])+
                                                                                             (available_adjusted$RBI/hitters_averages[hitters_averages$e == "OF","RBI"][[1]])+
                                                                                             (available_adjusted$HR/hitters_averages[hitters_averages$e == "OF","HR"][[1]])+
                                                                                             (available_adjusted$SB/hitters_averages[hitters_averages$e == "OF","SB"][[1]])+
                                                                                             (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                                             (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),
                                                                                          ifelse(available_adjusted$e == "UT",
                                                                                                 ((available_adjusted$AVG/hitters_averages[hitters_averages$e == "UT","AVG"][[1]])+
                                                                                                    (available_adjusted$R/hitters_averages[hitters_averages$e == "UT","R"][[1]])+
                                                                                                    (available_adjusted$RBI/hitters_averages[hitters_averages$e == "UT","RBI"][[1]])+
                                                                                                    (available_adjusted$HR/hitters_averages[hitters_averages$e == "UT","HR"][[1]])+
                                                                                                    (available_adjusted$SB/hitters_averages[hitters_averages$e == "UT","SB"][[1]])+
                                                                                                    (available_adjusted$AVG + available_adjusted$R + available_adjusted$RBI + available_adjusted$HR + available_adjusted$SB)-
                                                                                                    (rowMaxs(as.matrix(available_adjusted[,4:8]),value = TRUE)-rowMins(as.matrix(available_adjusted[,4:8]),value = TRUE))/2),0
                                                                                          ))))))))),2)

# Build Big Board/ Target Chart ----
big_board <- available_adjusted[,c("Name",
                                   "playerid",
                                   "e",
                                   "Value")]

big_board <- big_board %>% filter(Value >= 5)

colnames(big_board) <- c("Name",
                         "playerid",
                         "POS",
                         "Value")

big_board <- big_board[order(-big_board$Value),]
row.names(big_board)  <- 1:nrow(big_board)

big_board <- left_join(big_board, adp, by = c("Name" = "Player"))
big_board$Diff <- round(big_board$ADP - max(drafted$tu),2)

target_chart <- rbind(targets,team_stats[,c("Type",
                                            "AVG",
                                            "R",
                                            "RBI",
                                            "HR",
                                            "SB",
                                            "ERA",
                                            "WHIP",
                                            "W",
                                            "SO",
                                            "SV")], remains)
