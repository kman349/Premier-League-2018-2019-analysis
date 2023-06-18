"-------------------------------------------------------------------------------------------------------------------------------------------
Author: Kar-Ming Man
Last updated: 06-04-2020
Purpose: The purpose of this code is see if the performance of teams threatened with relegation 'drops off' 
after reaching 40 points.
"-------------------------------------------------------------------------------------------------------------------------------------------
install.packages("condformat")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("tidyr")
install.packages("flextable")
"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 1:
Install the package dplyr to use the tools needed to manipulate the data
"-------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(flextable)
library(condformat)

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 2: 
I have downloaded data from https://footystats.org/download-stats-csv and imported the CSV files into temporary tables
Below are the details of each table:
league_2018_2019  - overall statistics on the premier league season 2018-2019 
matches_2018_2019 - statistics on each match played in the premier league season 2018-2019
teams_2018_2019   - statistics on each premier league team in the premier league season 2018-2019
players_2018_2019 - statistics on each premier league player in the premier league season 2018-2019
"-------------------------------------------------------------------------------------------------------------------------------------------

matches_2018_2019  <- read.csv('england-premier-league-matches-2018-to-2019-stats.csv')

no_of_points <- function(home_team_name,away_team_name,home_team_goal_count, away_team_goal_count){
  points <- ifelse (home_team_name == "Everton" & home_team_goal_count > away_team_goal_count, 3, 
                    ifelse (away_team_name == "Everton" & home_team_goal_count < away_team_goal_count, 3, 
                            ifelse (home_team_goal_count == away_team_goal_count,1,0)))
  return(points)
}

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 3:
I have created a table to specifically to look at different teams. I have looked at the following teams:
- Wolves
- Everton
- Leicester City
- West Ham
- Watford
- Crystal Palce
- Newcastle
- Bournemouth 
- Burnley

I have also filtered out the the following columns:
- date_GMT
- home_team_name
- home_team_goal_count
- away_team_name
- away_team_goal_count
"-------------------------------------------------------------------------------------------------------------------------------------------
Wolves <- matches_2018_2019 %>% filter(home_team_name == "Wolverhampton Wanderers"|away_team_name == "Wolverhampton Wanderers") %>% 
          select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
          mutate(gameweek = (1:38), Team = ("Wolverhampton Wanderers"),
                 points = ifelse (home_team_name == "Wolverhampton Wanderers" & home_team_goal_count > away_team_goal_count, 3, 
                          ifelse (away_team_name == "Wolverhampton Wanderers" & home_team_goal_count < away_team_goal_count, 3, 
                          ifelse (home_team_goal_count == away_team_goal_count,1,0))),
                 cumulative_points = cumsum(points))


Before_40_Avg_Wolves  <- Wolves %>%  filter(cumulative_points < 40) %>%  ## 1.50
  summarise(avg_points_b40 = round2(mean(points)))

Everton <- matches_2018_2019 %>% filter(home_team_name == "Everton"|away_team_name == "Everton") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Everton"),
         points = ifelse (home_team_name == "Everton" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "Everton" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

test <- matches_2018_2019 %>% filter(home_team_name == "Everton"|away_team_name == "Everton") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Everton"),
         points = no_of_points(home_team_name,away_team_name,home_team_goal_count,away_team_goal_count),
         cumulative_points = cumsum(points))


LeicesterCity <- matches_2018_2019 %>% filter(home_team_name == "Leicester City"|away_team_name == "Leicester City") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Leicester City"),
         points = ifelse (home_team_name == "Leicester City" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "Leicester City" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

WestHamUnited <- matches_2018_2019 %>% filter(home_team_name == "West Ham United"|away_team_name == "West Ham United") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("West Ham United"),
         points = ifelse (home_team_name == "West Ham United" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "West Ham United" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

Watford <- matches_2018_2019 %>% filter(home_team_name == "Watford"|away_team_name == "Watford") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Watford"),
         points = ifelse (home_team_name == "Watford" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "Watford" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

CrystalPalace <- matches_2018_2019 %>% filter(home_team_name == "Crystal Palace"|away_team_name == "Crystal Palace") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Crystal Palace"),
         points = ifelse (home_team_name == "Crystal Palace" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "Crystal Palace" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

Newcastle <- matches_2018_2019 %>% filter(home_team_name == "Newcastle United"|away_team_name == "Newcastle United") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Newcastle United"),
         points = ifelse (home_team_name == "Newcastle United" & home_team_goal_count > away_team_goal_count, 3, 
                          ifelse (away_team_name == "Newcastle United" & home_team_goal_count < away_team_goal_count, 3, 
                                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

Bournemouth <- matches_2018_2019 %>% filter(home_team_name == "AFC Bournemouth"|away_team_name == "AFC Bournemouth") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("	AFC Bournemouth"),
         points = ifelse (home_team_name == "AFC Bournemouth" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "AFC Bournemouth" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

Burnley <- matches_2018_2019 %>% filter(home_team_name == "Burnley"|away_team_name == "Burnley") %>% 
  select ('date_GMT','home_team_name','home_team_goal_count','away_team_name','away_team_goal_count') %>% 
  mutate(gameweek = (1:38), Team = ("Burnley"),
         points = ifelse (home_team_name == "Burnley" & home_team_goal_count > away_team_goal_count, 3, 
                  ifelse (away_team_name == "Burnley" & home_team_goal_count < away_team_goal_count, 3, 
                  ifelse (home_team_goal_count == away_team_goal_count,1,0))),
         cumulative_points = cumsum(points))

Teams <- rbind(Wolves,Everton,LeicesterCity,WestHamUnited,Watford,Newcastle,CrystalPalace,Bournemouth,Burnley)

"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 5:
Create a rounding function to round the figures
"-------------------------------------------------------------------------------------------------------------------------------------------
round2 = function(x,n=1){
  posneg = sign(x)
  z = abs(x)*10^n
  z = trunc(z)
  z = z/10^n
  z*posneg
}
"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 6:
I now want the mean average points across the whole season, the mean average points before reaching 40 
points and the mean average points after 40 points
"-------------------------------------------------------------------------------------------------------------------------------------------
##Wolves
Tot_Avg_Wolves        <- Wolves %>% summarise(avg_points = round2(mean(points)))

Before_40_Avg_Wolves  <- Wolves %>%  filter(cumulative_points < 40) %>%  ## 1.50
                            summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Wolves   <- Wolves %>%  filter(cumulative_points >= 40) %>% ## 1.50  
                            summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Wolves  <- Wolves %>%  filter(cumulative_points >= 40) %>%  
                                   count(Team)

Wolves_avg            <- cbind(Match_count_after_40_Wolves,Tot_Avg_Wolves,Before_40_Avg_Wolves,After_40_Avg_Wolves)


##Everton
Tot_Avg_Everton        <- Everton %>% summarise(avg_points = round2(mean(points))) ##	1.42

Before_40_Avg_Everton  <- Everton %>%  filter(cumulative_points < 40) %>%  ## 1.23
  summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Everton   <- Everton %>%  filter(cumulative_points >= 40) %>% ## 2.13 
  summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Everton  <- Everton %>%  filter(cumulative_points >= 40) %>%  
                                  count(Team)

Everton_avg            <- cbind(Match_count_after_40_Everton,Tot_Avg_Everton,Before_40_Avg_Everton,After_40_Avg_Everton)

##LeicesterCity
Tot_Avg_Leicester        <- LeicesterCity %>% summarise(avg_points = round2(mean(points))) ## 1.37

Before_40_Avg_Leicester  <- LeicesterCity %>%  filter(cumulative_points < 40) %>%  ## 1.27
                            summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Leicester   <- LeicesterCity %>%  filter(cumulative_points >= 40) %>% ## 1.75  
                            summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Leicester  <- LeicesterCity %>%  filter(cumulative_points >= 40) %>%  
                                     count(Team)

Leicester_avg            <- cbind(Match_count_after_40_Leicester,Tot_Avg_Leicester,Before_40_Avg_Leicester,After_40_Avg_Leicester)

##WestHam
Tot_Avg_WestHam          <- WestHamUnited %>% summarise(avg_points = round2(mean(points))) ## 1.37

Before_40_Avg_WestHam    <- WestHamUnited %>%  filter(cumulative_points < 40) %>%  ## 1.30
                            summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_WestHam     <- WestHamUnited %>%  filter(cumulative_points >= 40) %>% ## 1.63 
                            summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_WestHam  <- WestHamUnited %>%  filter(cumulative_points >= 40) %>%  
                                    count(Team)

WestHam_avg            <- cbind(Match_count_after_40_WestHam,Tot_Avg_WestHam,Before_40_Avg_WestHam,After_40_Avg_WestHam)


##Watford
Tot_Avg_Watford          <- Watford %>% summarise(avg_points = round2(mean(points))) ## 1.32

Before_40_Avg_Watford    <- Watford %>%  filter(cumulative_points < 40) %>%  ## 1.42
                            summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Watford     <- Watford %>%  filter(cumulative_points >= 40) %>% ## 1.08 
                            summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Watford <- Watford %>%  filter(cumulative_points >= 40) %>%  
                                  count(Team)

Watford_avg            <- cbind(Match_count_after_40_Watford,Tot_Avg_Watford,Before_40_Avg_Watford,After_40_Avg_Watford)


##CrystalPalace
Tot_Avg_CrystalPalace        <- CrystalPalace %>% summarise(avg_points = round2(mean(points))) ## 1.29

Before_40_Avg_CrystalPalace  <- CrystalPalace %>%  filter(cumulative_points < 40) %>% ## 1.15 
  summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_CrystalPalace   <- CrystalPalace %>%  filter(cumulative_points >= 40) %>% ## 2.50
  summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_CrystalPalace <- CrystalPalace %>%  filter(cumulative_points >= 40) %>%  
                                        count(Team)

CrystalPalace_avg            <- cbind(Match_count_after_40_CrystalPalace,Tot_Avg_CrystalPalace,Before_40_Avg_CrystalPalace,After_40_Avg_CrystalPalace)

##Newcastle
Tot_Avg_Newcastle        <- Newcastle %>% summarise(avg_points = round2(mean(points))) ## 1.18

Before_40_Avg_Newcastle  <- Newcastle %>%  filter(cumulative_points < 40) %>%  ## 1.12
  summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Newcastle   <- Newcastle %>%  filter(cumulative_points >= 40) %>% ## 1.75  
  summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Newcastle <- Newcastle %>%  filter(cumulative_points >= 40) %>%  
                                        count(Team)

Newcastle_avg            <- cbind(Match_count_after_40_Newcastle,Tot_Avg_Newcastle,Before_40_Avg_Newcastle,After_40_Avg_Newcastle)


##Bournemouth
Tot_Avg_Bournemouth        <- Bournemouth %>% summarise(avg_points = round2(mean(points))) ##	1.05

Before_40_Avg_Bournemouth  <- Bournemouth %>%  filter(cumulative_points < 40) %>%  ##	1.15
  summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Bournemouth   <- Bournemouth %>%  filter(cumulative_points >= 40) %>% ## 1.40
  summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Bournemouth <- Bournemouth %>%  filter(cumulative_points >= 40) %>%  
                                      count(Team)

Bournemouth_avg            <- cbind(Match_count_after_40_Bournemouth,Tot_Avg_Bournemouth,Before_40_Avg_Bournemouth,After_40_Avg_Bournemouth)

##Burnley
Tot_Avg_Burnley        <- Burnley %>% summarise(avg_points = round2(mean(points))) ## 1.18

Before_40_Avg_Burnley  <- Burnley %>%  filter(cumulative_points < 40) %>% ## 1.15 
  summarise(avg_points_b40 = round2(mean(points)))

After_40_Avg_Burnley   <- Burnley %>%  filter(cumulative_points >= 40) %>% ## 0.25  
  summarise(avg_points_a40 = round2(mean(points)))

Match_count_after_40_Burnley <- Burnley %>%  filter(cumulative_points >= 40) %>%  
                                      count(Team)

Burnley_avg <- cbind(Match_count_after_40_Burnley,Tot_Avg_Burnley,Before_40_Avg_Burnley,After_40_Avg_Burnley)


Average_Points <- rbind(Wolves_avg,Everton_avg,Leicester_avg,WestHam_avg,Watford_avg,Newcastle_avg,CrystalPalace_avg,Bournemouth_avg,Burnley_avg)
Average_Points <- Average_Points %>% mutate(Difference = avg_points_a40-avg_points_b40)


Average_Points <- rename(Average_Points,"Team"=team,
                                        "Number of matches on or after 40 points"=n, 
                                        "Average points"=avg_points,
                                        "Average points before 40 points"=avg_points_b40,
                                        "Average points on or after 40 points"=avg_points_a40)

Average_Points <- flextable(Average_Points)
Average_Points <- fontsize(Average_Points, size = 16, part = "all")
Average_Points <- autofit(Average_Points,add_w = 0)
Average_Points <- color(Average_Points, i = ~ Difference < 0,
                        j = ~ Difference, 
                        color="red")
Average_Points <- color(Average_Points, i = ~ Difference > 0,
                        j = ~ Difference, 
                        color="green")
print(Average_Points)



"-------------------------------------------------------------------------------------------------------------------------------------------
STEP 7:
I now want to create a graph showing the points accumulation throughout the season
"-------------------------------------------------------------------------------------------------------------------------------------------
Teams_chart <-ggplot(Teams,aes(x=gameweek,y=cumulative_points,group=Team))+
              geom_line(aes(color=Team),lwd=1.2)+theme(text = element_text(size = 20),panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(),panel.background = element_blank())+
              xlab("Gameweek")+ylab("Cumulative Points")

print(Teams_chart)+scale_color_manual(values=c("#ffa500","#f6546a","#1567d8","#ebe20a","#b45476","#000000","#ccff00","#00ced1","#800000"))
            
"--------------------------------------------------------------------------------------------------
NOT NEEDED BUT USEFUL
condformat(Average_Points) %>%
  rule_text_color(Difference,ifelse(Difference < 0, red,ifelse(Difference > 0, green, "")))
"--------------------------------------------------------------------------------------------------



