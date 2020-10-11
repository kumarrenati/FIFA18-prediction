# FIFA18- Predict

1. We predict that `Brazil` is going to win the worldcup.

# Data Collection

1. `data/results.csv` file is obtained from [kaggle](https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017)
  * This file contains historical results of 32 teams in the World Cup 
  * This dataset contains 38949 International football matches dating from 30th Nov 1872 to 28th May 2018
2. `fixtures` was hardcoded as shown below: Since our project focused in predicting winners from quarter finals, semi finals and finals
3. Data analyis is done only by considering the data from previous worldcup since 2014 World Cup and the 2016 Euro Cup are among the most heavily weighted games based on our weight formulae.

```r
  winners <- c("Uruguay", "Spain", "France", "Croatia", "Brazil", "Sweden", "Belgium", "Colombia")
  runners_up <- c("Russia", "Portugal", "Denmark", "Argentina", "Switzerland", "Mexico", "England", "Japan")
```

# Model

```r
glm_goals <- glm(goals ~ team + opponent + location, 
                 family = "poisson",
                 data = since_prev_worldcup, 
                 weights = match_weight)
```

1. The 2 factors in building the model were: (So that Most recent matches should have more weightage)
  * base_weight: Taken from [FIFA official rankings](https://www.fifa.com/fifa-world-ranking/procedure/men.html)
    * Friendlies and other matches: base_weight = 1
    * Qualification matches for World Cup and continental championships: base_weight = 3
    * Confederations Cup and continental championships: base_weight=5
    * World Cup Matches: base_weight=8
  * dynamic_weight(w_i) = base_weight_i * exp(-(days_past)/max(days_past))
    * `days_past = as.numeric(Sys.Date() - worldcup_results$date)`
2. The formulae we use in building the model is as follows
  * `Y = b_0 + b_team * X_team + b_opponent * X_opponent + b_location * X_location + c`
  * X_team_i, X_opponent_i, and X_location_i are indicator vector's for the i'th game's team, opponent and location
  * Y_i is the differential score
  * c: Constant and a representative of error
3. Reason behind writing invert function
  * Key here is duplication: So that our prediction of team score becomes `function(team, opponent,location)`
  * i.e., a single match: 

```r
# This is done so that we can reduce prediction parameters
x <- c(team=England, opponent=Spain, location=N|H|A, team_score=3, opponent_score=5)
y <- c(team=Spain, opponent=England, location=N|A|H, team_score=5, opponent_score=3)
data <- rbind(x,y)
```

4. The model gives us the coefficients for each country as levels of `team` and `opponent` factors
  * We can take them as `attack` and `defense` factors of the country
  * Higher `attack` coefficients would mean that the team is likely to score more 
  * Lower `defense` coefficients would mean that the team is likely to conceed less

```r
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
``` 

# Reason behind choosing Poissons distribution 

```r
# The goal distribution shows a clear poisson's distribution
ggplot(dataset,aes(x= goals,group=factor(goals), fill = factor(goals)))+
  geom_histogram(position="identity", alpha=0.8)+theme_bw() +
  scale_x_continuous(breaks=c(0:10),labels=c(0:10))
```

* Therefore the probability function is as follows
  * `P(X=k) = ( exp(-(lambda))*(lambda^k) ) / factorial(x)` Where x = 0,1,2,.... etc

# Simulating the worldcup

1. We run 10000 iterations of the following steps
  * Simulate knockout games by drawing the number of goals scored by each team from their respective poisson distributions.
  * If there is a tie, We assume that teams convert penalties at similar rates `sample(c("A", "B"), 1)` This is similar to flipping a coin and checking out the luck
  * Repeat these steps until there is a champion

# Assumptions

1. Rate of goals scored is independent of time
2. If there is a tie we flip the coin and declare the winner with `sample(c("A", "B"), 1)`