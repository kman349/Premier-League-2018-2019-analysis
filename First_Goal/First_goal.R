"-------------------------------------------------------------------------------------------------------------------------------------------
Author: Kar-Ming Man
Last updated: 12-05-2020
Purpose: The purpose of this code is to investigate the impact of scoring the first goal in winning the match
"-------------------------------------------------------------------------------------------------------------------------------------------

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 1:
Load the packages needed to manipulate the data
"-------------------------------------------------------------------------------------------------------------------------------------------

library(anchors)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(plotly)
library(janitor)

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 2: 
I have downloaded data from https://footystats.org/download-stats-csv and imported the CSV files into temporary tables
Below are the details of each table:

matches_2018_2019 - statistics on each match played in the premier league season 2018-2019
"-------------------------------------------------------------------------------------------------------------------------------------------
  
matches_2018_2019  <- read.csv('england-premier-league-matches-2018-to-2019-stats.csv')


"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 3: 
The colmn home_team_goal_timing and away_team_goal_timing contained the minutes goals were scored. I had to seperate out each minute into their
own respective column in order to run my analysis. 
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019 <- separate (data=matches_2018_2019,col=home_team_goal_timings,into= 
                                 c("home_goal_1","home_goal_2","home_goal_3",
                                   "home_goal_4","home_goal_5","home_goal_6",
                                   "home_goal_7","home_goal_8","home_goal_9"),sep = ',')

matches_2018_2019 <-separate (data=matches_2018_2019,col=away_team_goal_timings,into= 
                                c("away_goal_1","away_goal_2","away_goal_3",
                                  "away_goal_4","away_goal_5","away_goal_6",
                                  "away_goal_7","away_goal_8","away_goal_9"),sep = ',')

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 4: 
I had to manipulate the values in where we have '45'1','90'2' etc and convert them into '46' and '92' etc in order to run the analysis. This 
step seperates the values into different columns e.g. '45' and '1' would be in seperate columns.
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_1,into= c("home_goal_1","home_goal_1_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_2,into= c("home_goal_2","home_goal_2_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_3,into= c("home_goal_3","home_goal_3_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_4,into= c("home_goal_4","home_goal_4_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_5,into= c("home_goal_5","home_goal_5_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_6,into= c("home_goal_6","home_goal_6_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_7,into= c("home_goal_7","home_goal_7_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_8,into= c("home_goal_8","home_goal_8_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=home_goal_9,into= c("home_goal_9","home_goal_9_et"),sep = "'")

matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_1,into= c("away_goal_1","away_goal_1_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_2,into= c("away_goal_2","away_goal_2_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_3,into= c("away_goal_3","away_goal_3_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_4,into= c("away_goal_4","away_goal_4_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_5,into= c("away_goal_5","away_goal_5_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_6,into= c("away_goal_6","away_goal_6_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_7,into= c("away_goal_7","away_goal_7_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_8,into= c("away_goal_8","away_goal_8_et"),sep = "'")
matches_2018_2019 <- separate (data=matches_2018_2019,col=away_goal_9,into= c("away_goal_9","away_goal_9_et"),sep = "'")

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 5: 
I convert the 'NA's' into zeros
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019[is.na(matches_2018_2019)] <- 0

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 6: 
I convert the home_goal and away_goal columns into numeric values
"-------------------------------------------------------------------------------------------------------------------------------------------
  
matches_2018_2019$home_goal_1 <- as.numeric(as.character(matches_2018_2019$home_goal_1))
matches_2018_2019$home_goal_2 <- as.numeric(as.character(matches_2018_2019$home_goal_2))
matches_2018_2019$home_goal_3 <- as.numeric(as.character(matches_2018_2019$home_goal_3))
matches_2018_2019$home_goal_4 <- as.numeric(as.character(matches_2018_2019$home_goal_4))
matches_2018_2019$home_goal_5 <- as.numeric(as.character(matches_2018_2019$home_goal_5))
matches_2018_2019$home_goal_6 <- as.numeric(as.character(matches_2018_2019$home_goal_6))
matches_2018_2019$home_goal_7 <- as.numeric(as.character(matches_2018_2019$home_goal_7))
matches_2018_2019$home_goal_8 <- as.numeric(as.character(matches_2018_2019$home_goal_8))
matches_2018_2019$home_goal_9 <- as.numeric(as.character(matches_2018_2019$home_goal_9))

matches_2018_2019$home_goal_1_et <- as.numeric(as.character(matches_2018_2019$home_goal_1_et))
matches_2018_2019$home_goal_2_et <- as.numeric(as.character(matches_2018_2019$home_goal_2_et))
matches_2018_2019$home_goal_3_et <- as.numeric(as.character(matches_2018_2019$home_goal_3_et))
matches_2018_2019$home_goal_4_et <- as.numeric(as.character(matches_2018_2019$home_goal_4_et))
matches_2018_2019$home_goal_5_et <- as.numeric(as.character(matches_2018_2019$home_goal_5_et))
matches_2018_2019$home_goal_6_et <- as.numeric(as.character(matches_2018_2019$home_goal_6_et))
matches_2018_2019$home_goal_7_et <- as.numeric(as.character(matches_2018_2019$home_goal_7_et))
matches_2018_2019$home_goal_8_et <- as.numeric(as.character(matches_2018_2019$home_goal_8_et))
matches_2018_2019$home_goal_9_et <- as.numeric(as.character(matches_2018_2019$home_goal_9_et))

matches_2018_2019$away_goal_1 <- as.numeric(as.character(matches_2018_2019$away_goal_1))
matches_2018_2019$away_goal_2 <- as.numeric(as.character(matches_2018_2019$away_goal_2))
matches_2018_2019$away_goal_3 <- as.numeric(as.character(matches_2018_2019$away_goal_3))
matches_2018_2019$away_goal_4 <- as.numeric(as.character(matches_2018_2019$away_goal_4))
matches_2018_2019$away_goal_5 <- as.numeric(as.character(matches_2018_2019$away_goal_5))
matches_2018_2019$away_goal_6 <- as.numeric(as.character(matches_2018_2019$away_goal_6))
matches_2018_2019$away_goal_7 <- as.numeric(as.character(matches_2018_2019$away_goal_7))
matches_2018_2019$away_goal_8 <- as.numeric(as.character(matches_2018_2019$away_goal_8))
matches_2018_2019$away_goal_9 <- as.numeric(as.character(matches_2018_2019$away_goal_9))

matches_2018_2019$away_goal_1_et <- as.numeric(as.character(matches_2018_2019$away_goal_1_et))
matches_2018_2019$away_goal_2_et <- as.numeric(as.character(matches_2018_2019$away_goal_2_et))
matches_2018_2019$away_goal_3_et <- as.numeric(as.character(matches_2018_2019$away_goal_3_et))
matches_2018_2019$away_goal_4_et <- as.numeric(as.character(matches_2018_2019$away_goal_4_et))
matches_2018_2019$away_goal_5_et <- as.numeric(as.character(matches_2018_2019$away_goal_5_et))
matches_2018_2019$away_goal_6_et <- as.numeric(as.character(matches_2018_2019$away_goal_6_et))
matches_2018_2019$away_goal_7_et <- as.numeric(as.character(matches_2018_2019$away_goal_7_et))
matches_2018_2019$away_goal_8_et <- as.numeric(as.character(matches_2018_2019$away_goal_8_et))
matches_2018_2019$away_goal_9_et <- as.numeric(as.character(matches_2018_2019$away_goal_9_et))

matches_2018_2019[is.na(matches_2018_2019)] <- 0
"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 7: 
This adds up the columns to add up the values where we have injury time
"-------------------------------------------------------------------------------------------------------------------------------------------
  
matches_2018_2019$home_goal_1 <- matches_2018_2019$home_goal_1+matches_2018_2019$home_goal_1_et
matches_2018_2019$home_goal_2 <- matches_2018_2019$home_goal_2+matches_2018_2019$home_goal_2_et
matches_2018_2019$home_goal_3 <- matches_2018_2019$home_goal_3+matches_2018_2019$home_goal_3_et
matches_2018_2019$home_goal_4 <- matches_2018_2019$home_goal_4+matches_2018_2019$home_goal_4_et
matches_2018_2019$home_goal_5 <- matches_2018_2019$home_goal_5+matches_2018_2019$home_goal_5_et
matches_2018_2019$home_goal_6 <- matches_2018_2019$home_goal_6+matches_2018_2019$home_goal_6_et
matches_2018_2019$home_goal_7 <- matches_2018_2019$home_goal_7+matches_2018_2019$home_goal_7_et
matches_2018_2019$home_goal_8 <- matches_2018_2019$home_goal_8+matches_2018_2019$home_goal_8_et
matches_2018_2019$home_goal_9 <- matches_2018_2019$home_goal_9+matches_2018_2019$home_goal_9_et

matches_2018_2019$away_goal_1 <- matches_2018_2019$away_goal_1+matches_2018_2019$away_goal_1_et
matches_2018_2019$away_goal_2 <- matches_2018_2019$away_goal_2+matches_2018_2019$away_goal_2_et
matches_2018_2019$away_goal_3 <- matches_2018_2019$away_goal_3+matches_2018_2019$away_goal_3_et
matches_2018_2019$away_goal_4 <- matches_2018_2019$away_goal_4+matches_2018_2019$away_goal_4_et
matches_2018_2019$away_goal_5 <- matches_2018_2019$away_goal_5+matches_2018_2019$away_goal_5_et
matches_2018_2019$away_goal_6 <- matches_2018_2019$away_goal_6+matches_2018_2019$away_goal_6_et
matches_2018_2019$away_goal_7 <- matches_2018_2019$away_goal_7+matches_2018_2019$away_goal_7_et
matches_2018_2019$away_goal_8 <- matches_2018_2019$away_goal_8+matches_2018_2019$away_goal_8_et
matches_2018_2019$away_goal_9 <- matches_2018_2019$away_goal_9+matches_2018_2019$away_goal_9_et


"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 8: 
Create a column called 'first goal' which indicates the time the first goal was scored. I had to do this in two steps. The first was to create
the column and say if the muinute of the first home goal is less than the muinute of the first away goal then give the minute of the first home
goal and vice versa. However, in columns home_goal_1 and away_goal_1, where a team has not scored they have been assigned the value zero. This
means we get cases where is a home team wins 2-0, the first goal value will be given as zero. Therefore we need to take a second step where 
if home_goal_1 does not equal zero and away goal equals zero and first_goal equals zero then the value should be home_goal_1 and vice versa.  
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019 <- matches_2018_2019 %>% 
  mutate(first_goal = ifelse(home_goal_1 < away_goal_1,home_goal_1,
                             ifelse(away_goal_1 < home_goal_1,away_goal_1,"NA")))


matches_2018_2019 <- matches_2018_2019 %>% 
  mutate(first_goal = ifelse(home_goal_1 != 0 & away_goal_1 == 0  & first_goal == 0,home_goal_1,
                             ifelse(away_goal_1 != 0 & home_goal_1 == 0  & first_goal == 0,away_goal_1,first_goal)))

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 9: 
Created a column to state the combination of first goal scored and result outcome for home and away teams. Below are the meanings behind each
label:
fg_home_win_home = home team scores first and home win
fg_home_win_away = home team scores first and away win
fg_away_win_away = away team scores first and away win
fg_away_win_home = away team scores first and home win
fg_home_draw     = home team scores first and draw
fg_away_draw     = away team scores first and draw
no_score_draw    = 0-0 draw
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019 <- matches_2018_2019 %>% 
  mutate(result = ifelse(home_goal_1 == first_goal & home_team_goal_count > away_team_goal_count,"fg_home_win_home",
                         ifelse(home_goal_1 == first_goal & home_team_goal_count < away_team_goal_count,"fg_home_win_away",
                              ifelse(away_goal_1 == first_goal & home_team_goal_count < away_team_goal_count,"fg_away_win_away",
                              ifelse(away_goal_1 == first_goal & home_team_goal_count > away_team_goal_count,"fg_away_win_home",       
                              ifelse(home_goal_1 == first_goal & home_team_goal_count == away_team_goal_count,"fg_home_draw",
                              ifelse(away_goal_1 == first_goal & home_team_goal_count == away_team_goal_count,"fg_away_draw","no_score_draw")))))))


"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 11: 
I converted the first_goal column from character to numeric
"-------------------------------------------------------------------------------------------------------------------------------------------
  
matches_2018_2019$first_goal <- as.numeric(as.character(matches_2018_2019$first_goal))


"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 12: 
I did some exploratory analysis on first goals. I created a histogram to show the frequency of the minute where the first goal was scored
"-------------------------------------------------------------------------------------------------------------------------------------------

first_goal <- ggplot(matches_2018_2019,aes(x=first_goal))+geom_histogram(binwidth=1,color="#17192a",fill="#99ccff")
print(first_goal)
print(ggplotly(first_goal))

round2 = function(x,n=1){
  posneg = sign(x)
  z = abs(x)*10^n
  z = trunc(z)
  z = z/10^n
  z*posneg
}

first_goal <- matches_2018_2019 %>%
  tabyl(first_goal)

first_goal <- first_goal %>% mutate(valid_percent = (valid_percent),
                                    cumulative_perc = cumsum(valid_percent)) 

first_goal <- first_goal %>% mutate(valid_percent = round2(valid_percent*100),
                                    cumulative_perc = round2(cumulative_perc*100)) 

write.csv(first_goal,"first_goal.csv")

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 13:
More exploratory analysis. Firstly, I convereted the result column into factor. I then ordered the factors in my results column by the 
frequency of each factor - this was to order the following bar chart which I created using ggplot. The graph craeted shows the frequency of 
results outcome after a first goal is scored by home or away team.
"-------------------------------------------------------------------------------------------------------------------------------------------
  
matches_2018_2019$result <- as.factor(matches_2018_2019$result)

matches_2018_2019$result <- with(matches_2018_2019, reorder(result, result, function(x) -length(x)))

result_outcome <- ggplot(matches_2018_2019,aes(x=result))
result_outcome <- result_outcome+geom_bar(color="#17192a",fill="#99ccff")
result_outcome <- result_outcome+theme_classic()
#result_outcome <- result_outcome+ggtitle("Result outcome after a first goal scored for home or away team, Premier League 2018/19")
print(result_outcome)
print(ggplotly(result_outcome))

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 14:
More exploratory analysis. Firstly, combined the factors in the results column into a new column called 'win'. There were three factors in this
new column which were 'win','no win','NA'. Below shows what these mean:

win     = team scores first goal  and team wins
no win  = team scores first goal and team does not win (either loss or draw)
NA      = no score draw
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019 <- matches_2018_2019 %>% 
  mutate(win = ifelse(result == "fg_home_win_home"|result == "fg_away_win_away","win",
                      ifelse(result == "fg_home_win_away"|result =="fg_away_win_home"|result =="fg_home_draw"|result =="fg_away_draw","no win","NA")))

matches_2018_2019$win <- with(matches_2018_2019, reorder(win, win, function(x) -length(x)))

win_ratio <- ggplot(data=subset(matches_2018_2019,win != "NA"),aes(x=win))
win_ratio <- win_ratio+geom_bar(color="#17192a",fill="#99ccff")
win_ratio <- win_ratio+theme_classic()
#win_ratio <- win_ratio+ggtitle("Result outcome after a first goal scored, Premier League 2018/19")
print(win_ratio)
print(ggplotly(win_ratio))

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 15: 
More exploratory analysis on first goals. I created a histogram to show the frequency of the minute where the first goal was scored and 
whether the team who scored the first goal would win or not win the game.
"-------------------------------------------------------------------------------------------------------------------------------------------
  
first_goal_win <- ggplot(matches_2018_2019,aes(x=first_goal))
first_goal_win <- first_goal_win+geom_histogram(binwidth=1,color="#17192a",aes(fill=win))
print(first_goal_win)
print(ggplotly(first_goal_win))


matches_2018_2019 <- matches_2018_2019 %>% 
  mutate(outcome = ifelse(result == "fg_home_win_home"|result == "fg_away_win_away","win",
                      ifelse(result == "fg_home_win_away"|result =="fg_away_win_home","loss",
                             ifelse(result =="fg_home_draw"|result =="fg_away_draw","draw","NA"))))

first_goal_outcome <- ggplot(matches_2018_2019,aes(x=first_goal))
first_goal_outcome <- first_goal_outcome+geom_histogram(binwidth=1,color="#17192a",aes(fill=outcome))+scale_fill_brewer(palette = 1)
print(first_goal_outcome)
print(ggplotly(first_goal_outcome))





match_analysis <- match_analysis %>% arrange(match(result,c("fg_home_win_home","fg_away_win_away","fg_away_draw","fg_away_win_home","fg_home_draw","no_score_draw","fg_home_win_away")))


logistic_mod <- subset(matches_2018_2019,select=c("home_team_goal_count","away_team_goal_count","home_goal_1","away_goal_1","first_goal","win"))

logistic_mod <- mutate(goal_Scored = if(logistic_mod$win == "NA", 1,0))

