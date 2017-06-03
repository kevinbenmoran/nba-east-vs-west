library(dplyr)
library(ggplot2)
library(readr)
library(ggvis)
library(plotly)

# set plotly credentials
Sys.setenv("plotly_username"="kevinbenmoran")
Sys.setenv("plotly_api_key"="pMublbsa52vABKArlfNI")


### functions
head_to_head <- function(data, team_x, team_y, year_list) {
  df <- filter(data, 
               year_id %in% year_list, 
               is_playoffs == 0, 
               team_id == team_x & opp_id == team_y)
  x_win <- sum(df$game_result == "W")
  y_win <- sum(df$game_result == "L")
  cat(team_x,": ",x_win, " wins","\n", team_y,": ",y_win, " wins", sep = "")
}
head_to_head(data, "GSW", "LAL", c(2012,2013,2014))


records <- function(data, year_list = c(1947:2015)) {
  df <- filter(data, year_id %in% year_list, is_playoffs == 0) %>%
    group_by(fran_id, home_east, home_west) %>% 
    summarize(played = n(), 
              wins = sum(game_result == "W"), 
              win_perc = round(wins/n(), 2),
              east_wins = sum(game_result == "W" & away_east == 1),
              west_wins = sum(game_result == "W" & away_west == 1),
              east_perc = round(east_wins/sum(away_east == 1), 2),
              west_perc =  round(west_wins/sum(away_west == 1), 2)) %>%
    arrange(desc(win_perc))
  return(df)
}

records_by_year <- function(data, year_list = c(1947:2015)) {
  df <- filter(data, year_id %in% year_list, is_playoffs == 0) %>%
    group_by(fran_id, year_id, home_east, home_west) %>% 
    summarize(played = n(), 
              wins = sum(game_result == "W"), 
              win_perc = round(wins/n(), 2),
              east_wins = sum(game_result == "W" & away_east == 1),
              west_wins = sum(game_result == "W" & away_west == 1),
              east_perc = round(east_wins/sum(away_east == 1), 2),
              west_perc =  round(west_wins/sum(away_west == 1), 2)) %>%
    arrange(desc(win_perc))
  return(df)
}

playoffs <- function(data, year_list = c(1947:2015)) {
  df <- filter(data, year_id %in% year_list, is_playoffs == 1) %>%
    group_by(fran_id, home_east, home_west) %>% 
    summarize(played = n(), 
              wins = sum(game_result == "W"), 
              win_perc = round(wins/n(), 2),
              east_wins = sum(game_result == "W" & away_east == 1),
              west_wins = sum(game_result == "W" & away_west == 1),
              east_perc = round(east_wins/sum(away_east == 1), 2),
              west_perc =  round(west_wins/sum(away_west == 1), 2)) %>%
    arrange(desc(win_perc))
  return(df)
}

# set working directory

# read in data
data <- read_csv("nbaallelo.csv")

# create list of current teams
conf <- unique(data %>%
                 filter(year_id == 2015) %>%
                 select(team_id))

# set variables for which conference the teams are in, for conference based analysis
conf$east <- c(0,0,0,1,0,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,1,1,0,1,0)
conf$west <- c(1,1,1,0,1,1,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,1,1,0,0,1,0,1)

# add conference details for first the away, and then the home team
data <- merge(data, conf, by.x = "opp_id", by.y = "team_id")
data <- merge(data, conf, by.x = "team_id", by.y = "team_id")
names(data)[c(24,25,26,27)] <- c("away_east", "away_west", "home_east", "home_west")
glimpse(games2015)


    
records(data)
playoffs(data)

# look at 2015 records

records2015 <- games2015 %>%
  group_by(team_id, home_east, home_west) %>% 
  summarize(played = n(), 
            wins = sum(game_result == "W"), 
            win_perc = round(wins/82, 2),
            east_wins = sum(game_result == "W" & away_east == 1),
            west_wins = sum(game_result == "W" & away_west == 1),
            east_perc = round(east_wins/sum(away_east == 1), 2),
            west_perc =  round(west_wins/sum(away_west == 1), 2))

# from now on, will use function
records(data, year = c(2015))

# look at overall winning percentage against east and west teams
mean(records2015$east_perc)
mean(records2015$west_perc)
# it seems like eastern teams are slightly easier to beat
# however, if the east is in fact garbage, there will be garbage teams playing against garbage teams for a higher amount 
# (52 vs 30) of games, so the winning percentage may be higher than it should be

mean(subset(records2015, home_west == 1)$east_perc)
mean(subset(records2015, home_east == 1)$west_perc)

# It seems that west teams have a significantly higher win percentage against east teams than vice versa
# 58.53% vs 41.53%

reg_season <- records(data)
# all time west > east percentage
filter(reg_season, west_perc >= east_perc)

# last 5 years
filter(records(data, c(2013:2015)), west_perc >= east_perc)

# last 20 years
records20 <- records(data, c(1995:2015))
records_by_year_last_20 <- records_by_year(data, c(1995:2015))

# plot overall east vs west percentage, each data point reperesenting a team
records2015 %>% ggvis(~east_perc, ~west_perc) %>% layer_points()

p <- ggplot(records2015, aes(x = east_perc, y = west_perc)) +
  geom_point(aes(color = factor(home_east), text = fran_id)) +
  geom_smooth()

p20 <- ggplot(records_by_year_last_20, aes(x = east_perc, y = west_perc)) +
  geom_point(aes(color = factor(home_east), text = paste(fran_id, ": ", year_id, sep = ""))) +
  geom_smooth() +
  scale_fill_discrete(name ="Conference", labels = c("West", "East"))
  

p
p20

ggplotly(p)
ggplotly(p20)

# plotly practice

pal <- c("#ff6666", "#4d94ff")
px <- plot_ly(records_by_year_last_20, 
        x = ~east_perc, 
        y = ~west_perc, 
        color = ~factor(home_east),
        colors = pal,
        type = 'scatter',
        mode = 'markers',
        hoverinfo = 'text',
        text = ~paste('Team: ', fran_id, "\n",
                      'Year: ', year_id, "\n",
                      'East Win %: ', east_perc, "\n",
                      'West Win %: ', west_perc))


chart_link = plotly_POST(px, filename="scatter/nba")
chart_link




