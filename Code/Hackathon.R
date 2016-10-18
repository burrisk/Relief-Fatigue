# Load Packages
library(dplyr)

# Load Data

context = read.csv("Context.csv")
batterSplits = read.csv("Batting_Splits.csv")
batterStats = read.csv("Batting_Statistics.csv")
pitchFX = read.csv("Pitchfx.csv")
pitcherSplits = read.csv("Pitching_Splits.csv")
leagueAv = read.csv("League_Averages.csv")



pitchFX = pitchFX %>%
  select(game_id,game_date,inning,top_inning_sw:pre_outs,pitcher_id,
         initial_speed:init_accel_z) %>%
  group_by(game_id,pitcher_id)  %>% arrange(inning)

firstPitches = pitchFX %>% filter(inning == 1 & pre_balls == 0
                                 & pre_strikes == 0 & pre_outs == 0)

starters = unique(firstPitches$pitcher_id)
pitchFX = pitchFX %>% mutate(starter = pitcher_id %in% starters) 

pitchFX = arrange(pitchFX,inning)
hist((pitchFX %>% filter(starter == 0))$inning)

relievers = filter(pitchFX,starter == 0)
starters = filter(pitchFX,starter == 1)

## Reliever Analysis

fastball.types = c("FA","FC","FF","FT","SI","FS")
offspeed.types = c("SL","KC","CH","CU")
relievers = relievers %>% filter(pitch_type %in% fastball.types 
                   | pitch_type %in% offspeed.types) %>%
  mutate(fastball = pitch_type %in% fastball.types)


# Calculate Average Velocity of Reliever Fastball
relieverID = unique(relievers$pitcher_id)


relievers.fastballs = relievers %>% filter(fastball == TRUE) %>%
  group_by(pitcher_id) %>%
  summarise(mean_vel = mean(initial_speed)) %>%
  left_join(relievers, by = 'pitcher_id')

relievers = relievers.fastballs
rm(relievers.fastballs)

# Calculate Stuff
relievers = relievers %>%
  mutate(abs_break_x = abs(break_x)) %>%
  mutate(abs_break_z = abs(break_z)) %>%
  mutate(totalMov = sqrt(break_z^2 + break_x^2))

#Fastball Stuff
relievers.fastballs = relievers %>%
  filter(fastball == T) 

relievers.fastballs = relievers.fastballs %>%
  mutate(stuff = scale(relievers.fastballs$initial_speed) + 
                         scale(relievers.fastballs$totalMov)) %>%
  mutate(velStuff = scale(relievers.fastballs$initial_speed)
  ) %>%
  mutate(movStuff = scale(relievers.fastballs$totalMov))



relievers.offspeed = relievers %>%
  filter(fastball == FALSE)

relievers.offspeed = relievers.offspeed %>%
  mutate(stuff = scale(relievers.offspeed$initial_speed) + 
           scale(relievers.offspeed$totalMov) +
           scale(relievers.offspeed$mean_vel - relievers.offspeed$initial_speed)
           ) %>%
  mutate(velStuff = scale(relievers.offspeed$initial_speed) + 
           scale(relievers.offspeed$mean_vel - relievers.offspeed$initial_speed)
  ) %>%
  mutate(movStuff = scale(relievers.offspeed$totalMov))


relievers = rbind(relievers.fastballs,relievers.offspeed)

names <- pitcherStats %>%
  group_by(pitcher_id,Name) %>%
  summarise(num_teams = n())

new_dset <- relievers %>%
  left_join(names,by = 'pitcher_id') %>%
  group_by(pitcher_id,Name) %>%
  summarise(mean_stuff = mean(stuff))

new_dset = as.data.frame(new_dset[1:568,1:3])
new_dset = new_dset %>%
  arrange(desc(mean_stuff))

stuff = rbind(new_dset,starter_new_dset)

#Plots 
pitcherStats = read.csv("Pitching_Statistics.csv")

pitcherStats = pitcherStats %>%
  filter(TBF > 100) %>%
  group_by(pitcher_id) %>%
  summarise(fip = mean(FIP))

stuff = left_join(stuff,pitcherStats, by = 'pitcher_id')

plot(stuff$mean_stuff,stuff$fip, xlab = "Mean Stuff", 
     ylab = "FIP", main = "FIP vs Stuff (All Pitchers)",
     pch = 19, cex = .7, xlim = c(-4,3))
summary(lm(stuff$fip~stuff$mean_stuff))

write.csv(new_dset,file = "new_dataset.csv")
           
games_r_dset <- relievers %>%
  left_join(names,by = 'pitcher_id') %>%
  group_by(pitcher_id,game_id) %>%
  summarise(mean_stuff_game = mean(stuff))

games_r_dset = as.data.frame(games_r_dset[1:35216,1:3])

write.csv(games_r_dset,file = "stuff_dataset.csv")
#WOBA Plots



