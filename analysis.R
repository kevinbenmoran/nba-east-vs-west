library(dplyr)
library(ggplot2)
library(readr)

# set working directory

data <- read_csv("nbaallelo.csv")
glimpse(data)

gsw2015 <- filter(data, team_id == "GSW", year_id == 2015, is_playoffs == 0)
sum(gsw2015$game_result == "W")



records2015

conf <- unique(data %>%
  filter(year_id == 2015) %>%
  select(team_id))

# set variables for conference
conf$east <- c(0,0,0,1,0,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,1,1,0,1,0)
conf$west <- c(1,1,1,0,1,1,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,1,1,0,0,1,0,1)

# combine with 2015 data set
games2015 <- filter(data, year_id == 2015, is_playoffs == 0)
games2015 <- merge(games2015, conf, by.x = "opp_id", by.y = "team_id")
games2015 <- merge(games2015, conf, by.x = "team_id", by.y = "team_id")
names(games2015)[c(24,25,26,27)] <- c("away_east", "away_west", "home_east", "home_west")
glimpse(games2015)


records2015 <- games2015 %>%
  group_by(team_id, home_east, home_west) %>% 
  summarize(played = n(), 
            wins = sum(game_result == "W"), 
            win_perc = round(wins/82, 2),
            east_wins = sum(game_result == "W" & away_east == 1),
            west_wins = sum(game_result == "W" & away_west == 1),
            east_perc = round(east_wins/sum(away_east == 1), 2),
            west_perc =  round(west_wins/sum(away_west == 1), 2))

# look at overall winning percentage against east and west teams
mean(records2015$east_perc)
mean(records2015$west_perc)
# it seems like eastern teams are slightly easier to beat
# however, if the east is in fact garbage, there will be garbage teams playing against garbage teams for a higher amount 
# (52 vs 30) of games, so the winning percentage may be higher than it should be

mean(subset(records2015, home_west == 1)$east_perc)

