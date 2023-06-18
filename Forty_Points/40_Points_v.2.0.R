#Author: Kar-Ming Man
#Last updated: 05-03-2023
#Purpose: The purpose of this code is see if the performance of teams threatened with relegation 'drops off' after reaching 40 points.

# 1.Load packages ----
library(pacman)
p_load(dplyr,ggplot2,ggthemes,tidyr,flextable,condformat,purrr)


# 2.I have downloaded data from https://footystats.org/download-stats-csv and imported the CSV files into temporary tables ----
#Below are the details of each table:
#league_2018_2019  - overall statistics on the premier league season 2018-2019 
#matches_2018_2019 - statistics on each match played in the premier league season 2018-2019
#teams_2018_2019   - statistics on each premier league team in the premier league season 2018-2019
#players_2018_2019 - statistics on each premier league player in the premier league season 2018-2019

matches_2018_2019  <- read.csv('england-premier-league-matches-2018-to-2019-stats.csv')

# 3. Create rounding function ----
round2 = function(x,n=1){
  posneg = sign(x)
  z = abs(x)*10^n
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# 4. Create lists ----
df_names <-c("wolverhampton_wanderers","everton","leicester_city","west_ham_united",
          "watford","crystal_palace","newcastle_united","afc_bournemouth","burnley")

teams <-c("Wolverhampton Wanderers","Everton","Leicester City","West Ham United",
              "Watford","Crystal Palace","Newcastle United","AFC Bournemouth","Burnley")


# 4. Run for loop to create separate data frames for specified teams in lists ----
for(i in 1:length(teams)){ 
  other <- subset(matches_2018_2019, home_team_name == teams[i]|away_team_name == teams[i]) %>%
    mutate(points = ifelse (home_team_name %in% teams[i] & home_team_goal_count > away_team_goal_count, 3, 
                            ifelse (home_team_name %in% teams[i] & home_team_goal_count < away_team_goal_count, 0,
                                    ifelse (away_team_name %in% teams[i] & home_team_goal_count > away_team_goal_count, 0,
                                            ifelse (away_team_name %in% teams[i] & home_team_goal_count < away_team_goal_count, 3,
                                                  ifelse (home_team_goal_count == away_team_goal_count,1,0))))),
           cumulative_points = cumsum(points),
           avg_points = round2(mean(points)),
           avg_points_b40 = round2(mean(points[cumulative_points < 40])),
           avg_points_a40 = round2(mean(points[cumulative_points >= 40])),
           count_match_a40 = ifelse (cumulative_points >= 40,1,0),
           gameweek = (1:38),
           team = teams[i])
        assign(paste0(df_names[i]), other,envir = .GlobalEnv)
}

# 5. Create summary table ----
df_list <- list(wolverhampton_wanderers,everton,leicester_city,west_ham_united,
                watford,crystal_palace,newcastle_united,afc_bournemouth,burnley)

avg_points <- do.call(rbind, (lapply(df_list, function(x) x[1,]))) %>% 
  select(team,count_match_a40,avg_points,avg_points_b40,avg_points_a40) %>% 
  mutate(Difference = avg_points_a40-avg_points_b40) %>% 
  rename("Team"=team,
         "Number of matches on or after 40 points"=count_match_a40, 
         "Average points"=avg_points,
         "Average points before 40 points"=avg_points_b40,
         "Average points on or after 40 points"=avg_points_a40) %>% 
  flextable() %>% 
  fontsize (size = 16, part = "all") %>% 
  autofit(add_w = 0) %>% 
  color(i = ~ Difference < 0,j = ~ Difference, color="red") %>% 
  color(i = ~ Difference > 0,j = ~ Difference, color="green") %>% 
  print()

# 6. Create chart ----
all_teams <- rbind (wolverhampton_wanderers,everton,leicester_city,west_ham_united,watford,crystal_palace,newcastle_united,afc_bournemouth,burnley)

teams_chart <-ggplot(all_teams,aes(x=gameweek,y=cumulative_points,group=team))+
              geom_line(aes(color=team),lwd=1.2)+theme(text = element_text(size = 20),panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(),panel.background = element_blank())+
              xlab("Gameweek")+ylab("Cumulative Points")+
              scale_color_manual(values=c("#ffa500","#f6546a","#1567d8","#ebe20a","#b45476","#000000","#ccff00","#00ced1","#800000"))

print(teams_chart)     

for (i in 1:length(df_list)) {
    chart <- ggplot(data = df_list [[i]],aes(x=gameweek,y=cumulative_points,group=team))+
    geom_line(aes(color=team),lwd=1.2)+theme(text = element_text(size = 20),panel.grid.major = element_blank(),
                                             panel.grid.minor = element_blank(),panel.background = element_blank())+
    xlab("Gameweek")+ylab("Cumulative Points")+
    scale_color_manual(values=c("#ffa500","#f6546a","#1567d8","#ebe20a","#b45476","#000000","#ccff00","#00ced1","#800000"))+
    print(chart)
}



"--------------------------------------------------------------------------------------------------
NOT NEEDED BUT USEFUL
condformat(Average_Points) %>%
  rule_text_color(Difference,ifelse(Difference < 0, red,ifelse(Difference > 0, green, "")))
  
  home_points <- function(home_team_goals, away_team_goals){
  points <- ifelse (home_team_goals > away_team_goals, 3, 
                    ifelse (home_team_goals < away_team_goals, 0, 
                            ifelse (home_team_goals == away_team_goals,1,0)))
  return(points)
}

away_points <- function(home_team_goals, away_team_goals){
  points <- ifelse (home_team_goals > away_team_goals, 0, 
                    ifelse (home_team_goals < away_team_goals, 3, 
                            ifelse (home_team_goals == away_team_goals,1,0)))
  return(points)
}

cumalative_points <- function(points){
  cumalative_points <- cumsum(coalesce(points,0))+ points*0
  return(cumalative_points)
}

lapply(df_list, function(x) {x$points <- home_points(x$home_team_goal_count,x$away_team_goal_count)})

test <- df_list %>% map(~ mutate(.,  points = ifelse (home_team_goal_count > away_team_goal_count, 3, 
                                               ifelse (home_team_goal_count < away_team_goal_count, 0, 
                                                       ifelse (home_team_goal_count == away_team_goal_count,1,0)))))

for (i in 1:dim(matches_2018_2019)[1]) {
  if (matches_2018_2019$home_team_name[i] %in% teams) {
    matches_2018_2019$points[i] <- home_points(matches_2018_2019$home_team_goal_count,matches_2018_2019$away_team_goal_count)[i]
  } 
  else if 
    (matches_2018_2019$away_team_name[i] %in% teams) {
      matches_2018_2019$points[i] <- away_points(matches_2018_2019$home_team_goal_count,matches_2018_2019$away_team_goal_count)[i]
  }
  else {
    matches_2018_2019$points[i] <- as.numeric("")
  }
}
for(i in 1:length(home_team)){other <- subset(matches_2018_2019, home_team_name == home_team[i]) %>%
    mutate(points = ifelse (home_team_name %in% home_team[i] & home_team_goal_count > away_team_goal_count, 3, 
                            ifelse (home_team_name %in% home_team[i] & home_team_goal_count < away_team_goal_count, 0,
                                    ifelse (away_team_name %in% home_team[i] & home_team_goal_count > away_team_goal_count, 0,
                                            ifelse (away_team_name %in% home_team[i] & home_team_goal_count < away_team_goal_count, 3,
                                                    ifelse (home_team_goal_count == away_team_goal_count,1,0))))),
           cumulative_points = cumsum(points),
           avg_points = round2(mean(points)),
           avg_points_b40 = round2(mean(points[cumulative_points < 40])),
           avg_points_a40 = round2(mean(points[cumulative_points >= 40])),
           count_match_a40 = ifelse (cumulative_points >= 40,1,0),
           gameweek = (1:19),
           team = home_team[i])
  assign(paste0(home_team[i]), other,envir = .GlobalEnv)
}

home_team <- c(unique(matches_2018_2019$home_team_name))

--------------------------------------------------------------------------------------------------"



