# ---------------------Datset collected from

# results.csv:
# https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017
# historical results for the 32 teams in the World Cup
# As of 1 June 2018, this dataset includes data on 38,949 international 
# football matches from 30 November 1872 to 28 May 2018.

# Fixture.csv
# Obtained from https://fixturedownload.com/results/fifa-world-cup-2018

#------------------Our model is based on this blog
# Based on this weightage criteria: https://www.fifa.com/fifa-world-ranking/procedure/men.html
# 2 Factors are base_weight and most recent match results should have more weightage
# 1st factor
# Friendlies and other matches: base_weight = 1
# Qualification matches for World Cup and continental championships: base_weight = 3
# Confederations Cup and continental championships: base_weight=5
# World Cup Matches: base_weight=8

# 2nd factor
# http://opisthokonta.net/?p=1685  
# 
# http://opisthokonta.net/?p=890
# 
# A link to add time relavance in the function for calculation differential scores and 
# match probability
# We shall use weight(w_i) 
# days_past = as.numeric(Sys.Date() - worldcup_results$date)
# w_i = base_weight_i * exp(-(days_past)/max(days_past))

# https://dashee87.github.io/data%20science/football/r/predicting-football-results-with-statistical-modelling/

# We shall use Linear regression 
# Y = b_0 + b_team * X_team + b_opponent * X_opponent + b_location * X_location + c
# X_team_i, X_opponent_i, and X_location_i are indicator vector's for the i'th 
# game's team, opponent and location
# Y_i is the differential score
# c: Constant and a representative of error

# This is the reason we write invert function:
# Key here is duplication: So that our prediction of team score becomes 
# function(team, opponent,location)
# i.e., a single match: 
# c(team=England, opponent=Spain, location=N|H|A, team_score=3, opponent_score=5)
# We shall also duplicate it to 
# c(team=Spain, opponent=England, location=N|A|H, team_score=5, opponent_score=3)

#-----------------------------------------------------------------------

rm(list=ls())

# Environment setup
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,         # Data munging functions
  data.table,    # Reading input files and Feature engineering
  ggplot2,       # Graphics visualization
  lubridate,     # Date format
  readr,         # Reading xlsx files
  stringr,       # String functions
  reshape2,      # Restructure and aggregate data
  corrplot,      # Correlation plots
  elo,           # Ratings
  gridExtra,
  XML,
  RCurl
)

#--------------------Load the datasets----------------------------------

worldcup_results <- fread("data/results.csv", stringsAsFactors = FALSE,data.table = FALSE)
fixtures <- read.csv("data/fixtures.csv", as.is = T)

set.seed(100)

#-------------Analyzing results.csv------------------

head(worldcup_results)
str(worldcup_results)
nrow(worldcup_results)

#--------------------Helper functions

# Inverts Home and Away teams
invert <- function(data, score = F) {
  data <- mutate(data, tmp = team, team = opponent, opponent = tmp)
  data$tmp[data$location == "H"] <- "A"
  data$tmp[data$location == "A"] <- "H"
  data$tmp[data$location == "N"] <- "N"
  data$location <- data$tmp
  if(score) {
    data$tmp <- data$team_score
    data$team_score <- data$opponent_score
    data$opponent_score <- data$tmp
  }
  return(select(data,-tmp))
}

# This function is taken from the blog
# View(dpois(0:2, 2) %o% dpois(0:2, 1))
# Obtain W: Win, L: Loss, T: Tie probabilities
# By default we have included max_goals =10: Use the history to find out 
# Future works:
# the max goals in between the team and opponent
match_probs <- function(x, y, max_goals = 10) {
  score_matrix <- dpois(0:max_goals, x) %o% dpois(0:max_goals, y)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(c(win_prob, tie_prob, loss_prob))
}

#-------------------Data cleaning-------------------------

# Change to date to Date format
worldcup_results$date <- as.Date(ymd(worldcup_results$date))

# days_past: Total days since tha match date
worldcup_results$days_past <- as.numeric(Sys.Date() - worldcup_results$date)

home <- select(worldcup_results, home_team, away_team, 
               tournament, neutral, home_score, days_past, date) %>%
        mutate(location = "H")
away <- select(worldcup_results, away_team, home_team, 
               tournament, neutral, away_score, days_past, date) %>%
        mutate(location = "A")
colnames(home) <- c("team", "opponent", "tournament", "neutral", "goals", 
                    "days_past", "date", "location")
colnames(away) <- c("team", "opponent", "tournament", "neutral", "goals", 
                    "days_past", "date", "location")
dataset <- rbind(home, away)
dataset$location[worldcup_results$neutral] <- "N"

## Classify Game Types (codes each tournament to certain game type)
# OM <- Other Match
# WC <- World Cup
# WCQ <- World Cup Qualifying
# CC <- Continental Cup
# CCQ <- Continental Cup Qualifying
# FR <- Friendly
# CFC <- Confederations Cup

dataset$game_type <- "OM"
dataset$game_type[dataset$tournament == "FIFA World Cup" ] <- "WC"
dataset$game_type[dataset$tournament == "FIFA World Cup qualification"] <- "WCQ"
dataset$game_type[dataset$tournament == "Friendly"] <- "FR"
dataset$game_type[dataset$tournament == "Confederations Cup"] <- "CFC"

dataset$game_type[dataset$tournament == "AFC Asian Cup"] <- "CC"
dataset$game_type[dataset$tournament == "AFC Challenge Cup"] <- "CC"
dataset$game_type[dataset$tournament == "African Cup of Nations"] <- "CC"
dataset$game_type[dataset$tournament == "CFU Caribbean Cup"] <- "CC"
dataset$game_type[dataset$tournament == "CONCACAF Championship"] <- "CC"
dataset$game_type[dataset$tournament == "Gold Cup"] <- "CC"
dataset$game_type[dataset$tournament == "Oceania Nations Cup"] <- "CC"
dataset$game_type[dataset$tournament == "UAFA Cup"] <- "CC"
dataset$game_type[dataset$tournament == "UEFA Euro"] <- "CC"

dataset$game_type[dataset$tournament == "AFC Asian Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "AFC Challenge Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "African Cup of Nations qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "CFU Caribbean Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "CONCACAF Championship qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "Gold Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "Oceania Nations Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "UAFA Cup qualification"] <- "CCQ"
dataset$game_type[dataset$tournament == "UEFA Euro qualification"] <- "CCQ"

# match_weight: base_weight Parameters (based on those used in the FIFA rankings formula)
dataset$match_weight <- 1
dataset$match_weight[dataset$game_type == "WC"] <- 8
dataset$match_weight[dataset$game_type == "WCQ" | dataset$game_type == "CCQ"] <- 3
dataset$match_weight[dataset$game_type == "CFC" | dataset$game_type == "CC"] <- 5

# Model building
# Parameters: 
# Team, Opponent, Match Type, Location, No. of days since previous world cup
# Filter the dataset since previous worldcup
since_prev_worldcup <- filter(dataset, date >= "2014/01/01")

# Row Bind Completed Fixtures with existing Data Set
fixtures_bind <- fixtures %>% mutate("tournament" = "World Cup", 
                                     "neutral" = location == "N", 
                                      "goals" = team_score,
                                      "date" = as.Date(date, "%m/%d/%y"),
                                      "days_past" = as.numeric(Sys.Date() - date), 
                                      "game_type" = "WC",
                                      "match_weight"= 8)
fixtures_bind <- fixtures_bind %>% 
                  select(team, opponent, tournament, neutral, goals,
                         days_past, date, location, game_type, match_weight) %>%
                  filter(days_past >= 0)
since_prev_worldcup <- rbind(since_prev_worldcup, fixtures_bind )

fixtures_invert_bind <- invert(fixtures, T) %>% mutate("tournament" = "World Cup", 
                                                 "neutral" = location == "N", 
                                                 "goals" = team_score,
                                                 "date" = as.Date(date, "%m/%d/%y"),
                                                 "days_past" = as.numeric(Sys.Date() - date), 
                                                 "game_type" = "WC",
                                                 "match_weight"= 8)

fixtures_invert_bind <- fixtures_invert_bind %>% 
                          select(team, opponent, tournament, neutral, goals,
                                 days_past, date, location, game_type, match_weight) %>%
                          filter(days_past >= 0)

since_prev_worldcup <- rbind(since_prev_worldcup, fixtures_invert_bind )

# Match weight calculations
since_prev_worldcup$match_weight <- 
  mutate(since_prev_worldcup, "match_weight" = match_weight * exp(-days_past/max(days_past))) %>% 
  pull(match_weight)

# Model description
# The model gives coefficients for each country 
# considering team and opponent factors.

glm_goals <- glm(goals ~ team + opponent + location, 
                 family = "poisson",
                 data = since_prev_worldcup, 
                 weights = match_weight)
#
glm_goals
total_teams <- (length(glm_goals$coefficients) - 1)/ 2
rankings <- data.frame("team" = sort(unique(since_prev_worldcup$team)),
                       "attack" = rep(NA, total_teams),
                       "defense" = rep(NA, total_teams))

# Each team has an attack and a defense parameter, and from a function of 
# these the expected number of goals for each team in a match is calculated

# attack_scale_factor: Considering only team coefficients for attack and taking thier mean
# Coefficients of Teams are from 2:k where k=total_teams
attack_scale_factor <- mean(glm_goals$coefficients[2:total_teams])

# defense_scale_factor: Considering only opponent coefficients for defense and taking thier mean
# Coefficients of opponent are k+1:2k-1 where k = total_teams
defense_scale_factor <- mean(glm_goals$coefficients[(total_teams + 1):(2*total_teams - 1)], na.rm = T)

rankings$attack <- c(0, glm_goals$coefficients[2:total_teams]) - attack_scale_factor
rankings$defense <- c(0, glm_goals$coefficients[(total_teams + 1):(2*total_teams - 1)]) - defense_scale_factor
rankings$net_rating <- rankings$attack - rankings$defense

rankings <- rankings[order(rankings$net_rating, decreasing = TRUE),]
rankings$rank <- 1:nrow(rankings)
write.csv(rankings, "rankings.csv", row.names = F)

############ Make Predictions ############
fixtures <- mutate(fixtures, "win" = NA, "tie" = NA, "loss" = NA)
index <- !is.na(fixtures$goal_diff)
fixtures[index & fixtures$goal_diff > 0, c("win", "tie", "loss")] <- c(1,0,0)
fixtures[index & fixtures$goal_diff == 0, c("win", "tie", "loss")] <- c(0,1,0)
fixtures[index & fixtures$goal_diff < 0, c("win", "tie", "loss")] <- c(0,0,1)
# fixtures[!is.na(fixtures$goal_diff) & fixtures$goal_diff > 0, 
#          c("win", "tie", "loss")] <- rep(c(1,0,0), rep(sum(!is.na(fixtures$goal_diff) & 
#                                                              fixtures$goal_diff > 0), 3))
# fixtures[!is.na(fixtures$goal_diff) & fixtures$goal_diff == 0, 
#          c("win", "tie", "loss")] <- rep(c(0,1,0), rep(sum(!is.na(fixtures$goal_diff) & 
#                                                              fixtures$goal_diff == 0), 3))
# fixtures[!is.na(fixtures$goal_diff) & fixtures$goal_diff < 0, 
#          c("win", "tie", "loss")] <- rep(c(0,0,1), rep(sum(!is.na(fixtures$goal_diff) & 
#                                                              fixtures$goal_diff < 0), 3))

# The below prediction is based on linear model, this will be replaced later
# since we shall build the model based on possion distribution. 
# As the historical data when plotted in histogram clearly indicates 
# that the model is poisson distribution

# Obtain Win, Loss, Tie probabilities

fixtures$team_score[is.na(fixtures$team_score)]<- 
  predict(glm_goals, newdata = fixtures[is.na(fixtures$team_score),], type = "response")
fixtures$opponent_score[is.na(fixtures$opponent_score)]<- 
  predict(glm_goals, newdata = invert(fixtures[is.na(fixtures$opponent_score),]), type = "response")
fixtures$goal_diff <- fixtures$team_score - fixtures$opponent_score

for(i in 1:nrow(fixtures)) {
  if(is.na(fixtures$win[i])) {
    fixtures[i, c("win", "tie", "loss")] <- match_probs(x = fixtures$team_score[i],
                                                        y = fixtures$opponent_score[i])
  }
}

# The goal distribution shows a clear poisson's distribution
ggplot(dataset,aes(x= goals,group=factor(goals), fill = factor(goals)))+
  geom_histogram(position="identity", alpha=0.8)+theme_bw() +
  scale_x_continuous(breaks=c(0:10),labels=c(0:10))

# Probability function should be
# P(X=k) = ( exp(-(lambda))*(lambda^k) ) / factorial(x)
# Where x= 0,1,2,.... etc

#--------- World Cup Simulation will start from here
# Create an empty data frame for world cup simulation
wc_sims <- data.frame("country" = unique(c(fixtures$team, fixtures$opponent)),
                      "group" = c(rep(c("A", "B", "C", "D", "E", "F", "G", "H"), rep(3, 8)),
                                  c("A", "B", "C", "D", "E", "F", "G", "H")),
                      "expected_pts" = rep(0, 32),
                      "first_in_group" = rep(0, 32),
                      "second_in_group" = rep(0, 32),
                      "r16" = rep(0, 32),
                      "qtrs" = rep(0, 32),
                      "semis" = rep(0, 32),
                      "finals" = rep(0, 32),
                      "champ" = rep(0, 32),
                      stringsAsFactors = F)

groups <- c("A", "B", "C", "D", "E", "F", "G", "H")

nsims <- 10000

for(k in 1:nsims) {
  if(k %% 50 == 0) {
    print(paste("Sim:", k))
  }
  
  # Actual results of the group stage or teams qualified to Round 16
  winners <- c("Uruguay", "Spain", "France", "Croatia", "Brazil", "Sweden", "Belgium", "Colombia")
  runners_up <- c("Russia", "Portugal", "Denmark", "Argentina", "Switzerland", "Mexico", "England", "Japan")
  
  ### Group Stage
  # for(i in 1:8) {
  #   sim_group_table <- sim_group(groups[i])
  #   index <- apply(as.data.frame(sim_group_table$country), 1, grep, wc_sims$country)
  #   wc_sims$expected_pts[index] <- wc_sims$expected_pts[index] + sim_group_table$pts/nsims
  #   index_1 <- wc_sims$country == sim_group_table$country[1]
  #   index_2 <- wc_sims$country == sim_group_table$country[2]
  #   wc_sims$first_in_group[index_1] <- wc_sims$first_in_group[index_1] + 1/nsims
  #   wc_sims$second_in_group[index_2] <- wc_sims$second_in_group[index_2] + 1/nsims
  #   wc_sims$r16[index_1 | index_2] <- wc_sims$r16[index_1 | index_2]+ 1/nsims
  #   winners[i] <- sim_group_table$country[1]
  #   runners_up[i] <- sim_group_table$country[2]
  # }
  
  ### Knock-Out Stage
  teams_left <- 16
  ko_round <- 1
  winners <- c(winners[c(1,3,5,7,2,4,6,8)], runners_up[c(2,4,6,8,1,3,5,7)])
  
  while(teams_left > 1) {
    # Empty knockout dataframe
    knockout <- data.frame("team" = winners[1:(teams_left/2)],
                           "opponent" = winners[(1 + teams_left/2):teams_left],
                           "team_goals" = rep(NA, teams_left/2),
                           "opp_goals" = rep(NA, teams_left/2),
                           "winner" = rep(NA, teams_left/2),
                           "location" = rep("N", teams_left/2),
                           stringsAsFactors = F)
    
    knockout$location[knockout$team == "Russia"] <- "H"
    knockout$location[knockout$opponent == "Russia"] <- "A"
    
    knockout$team_goals <- predict(glm_goals, newdata = knockout, type = "response")
    knockout$opp_goals <- predict(glm_goals, newdata = invert(knockout), type = "response")
    
    
    winners <- rep(NA, teams_left/2)
    
    for(i in 1:nrow(knockout)) {
      team_goals <- rpois(1, knockout$team_goals[i])
      opp_goals <- rpois(1, knockout$opp_goals[i])
      if(team_goals > opp_goals) {
        knockout$winner[i] <- knockout$team[i]
      }
      else if(team_goals < opp_goals) {
        knockout$winner[i] <- knockout$opponent[i]
      }
      else { 
        ## Penalty Shoot-out 50-50
        # Assumption: Similar to tossing the coin experiment
        # Ex: Run this   # sample(c("A", "B"), 1)
        knockout$winner[i] <- sample(c(knockout$team[i], knockout$opponent[i]), 1)
      }
    }
    
    # Anouncing the team winner
    if(teams_left > 2) {
      winners <- knockout$winner[c(seq(1, teams_left/2, 2), seq(2, teams_left/2, 2))]
    }else{
      winners <- knockout$winner
    }
    
    # Next stage iteration
    index <- wc_sims$country %in% knockout$winner
    teams_left <- teams_left/2
     
    if(teams_left >= 1) {
      wc_sims[index, 6 + ko_round] <- wc_sims[index, 6 + ko_round] + 1/nsims
    }
    ko_round <- ko_round + 1
  }
}

# Rounding the values
wc_sims$expected_pts <- round(wc_sims$expected_pts, 2)
wc_sims[, 4:10] <- round(wc_sims[, 4:10], 4)
wc_sims <- wc_sims[order(wc_sims$champ, decreasing = T),]
write.csv(wc_sims, "wc_sims.csv", row.names = F)

fifa_data<-read.csv("wc_sims.csv")
sum(is.na(fifa_data))

fifa<-fifa_data

fifa<-fifa[-c(3,4,5)]

library(party)
dtree_qtrs <- ctree(qtrs~.,data=fifa)
dtree_qtrs

plot(dtree_qtrs)

predata_qtrs <- predict(dtree_qtrs)

prob_cutoff <- max(predata_qtrs)
for(i in 1:nrow(predata_qtrs)) {
  if( predata_qtrs[i] < prob_cutoff)
    predata_qtrs[i] =0
  else predata_qtrs[i]=1
}

fifa$qtrs_team<-predata_qtrs

#decision tree to predict semis team

dtree_semis<-ctree(semis~.,data=fifa)
dtree_semis

plot(dtree_semis)

# using cutoff of 0.549
predata_semis <- predict(dtree_semis)
prob_cutoff <- max(predata_semis)
for(i in 1:nrow(predata_semis)){
  if( predata_semis[i] < prob_cutoff)
    predata_semis[i] =0
  else predata_semis[i]=1
}

fifa$semis_team<-predata_semis

#$semis_team<-as.vector(fifa$semis_team)

# decision tree to predict finals team

dtree_finals<-ctree(finals~.,data=fifa)
dtree_finals

plot(dtree_finals)

predata_finals<- predict(dtree_finals)
prob_cutoff <- max(predata_finals)
for(i in 1:nrow(predata_finals)){
  if( predata_finals[i] < prob_cutoff)
    predata_finals[i] =0
  else predata_finals[i]=1
}

fifa$finals_team<-predata_finals


length(which(fifa$qtrs_team==1))
length(which(fifa$semis_team==1))
length(which(fifa$winner==1))

group.colors <- c("Brazil" = "#4CAF50",
                  "Belgium" = "#17202A",
                  "Croatia" = "#EE2700",
                  "Colombia" = "#85C1E9",
                  "England" = "#3F51B5",
                  "Portugal" = "#A93226",
                  "Denmark" = "#F52300",
                  "France" = "#FFE082",
                  "Germany" = "#EF9A9A",
                  "Japan" = "#1E88E5",
                  "Russia" = "#EF9A9A",
                  "Senegal" = "#FF765B",
                  "Spain" = "#E96147",
                  "Sweden" = "#229954",
                  "Switzerland" = "#FC866E",
                  "Uruguay" = "#C0392B"
)
ggplot(filter(fifa_data,champ >= 0.03),
       aes(y = reorder(country,champ),x = champ,col = country)) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.1)) + coord_flip() +
  geom_label(aes(x = champ ,label = paste0(round(champ*100),"%"))) +
  scale_x_continuous(breaks = seq(0,1,0.01),
                     labels = seq(0,1,0.01)*100) +
  labs(title = "Most Likely Winner of the 2018 FIFA World Cup",
       x = "Probability (%)",
       y = "Team") +
  scale_fill_manual(values = group.colors)
