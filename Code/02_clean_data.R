# Load Packages
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)


# Load Raw Data
load("Data/pitch_raw.Rdata")

# Change date to R Date Object
pitch$date <- as.Date(gsub("_", "-", pitch$date))

# Create MLB Schedule Data Frame
years <- 2008:2016
opening <- as.Date(c("2008-03-31", "2009-04-05", "2010-04-04", "2011-03-31", "2012-03-28", "2013-03-31", "2014-03-31",
                     "2015-04-05", "2016-04-03"))
end.reg.season <- as.Date(c("2008-09-30", "2009-10-06","2010-10-05","2011-09-29", "2012-10-04", "2013-09-30",
                            "2014-09-29", "2015-10-05", "2016-10-03"))
mlb.schedule <- data.frame(opening, end.reg.season)
colnames(mlb.schedule) <- c("start", "end")
rownames(mlb.schedule) <- years

# Define which pitches are in regular season and postseason games
y <- format(pitch$date, "%Y")
pitch$reg_season <- as.numeric(pitch$date >= mlb.schedule[y,"start"]) & 
  (pitch$date <= mlb.schedule[y,"end"])
pitch$post_season <- as.numeric(pitch$date > mlb.schedule[y,"end"])
pitch$year <- y

# Exclude Spring Training games and Years not in Study
studied.years <- c(2014:2016)
pitch <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(as.numeric(year) %in% studied.years)

# Create Balls and Strikes Variables
pitch$b <- substr(pitch$count, 1, 1)
pitch$s <- substr(pitch$count, 3, 3)

# Find fastest pitch for each pitcher in a given year
fastest.pitch.dat <- pitch %>%
  filter(!is.na(start_speed)) %>%
  group_by(pitcher, year, pitch_type, pitcher_name) %>%
  filter(n() > 50) %>%
  summarise(avg_fast_speed = mean(start_speed)) %>%
  arrange(desc(avg_fast_speed)) %>%
  ungroup() %>%
  group_by(pitcher, year, pitcher_name) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pitch_type)

pitch <- inner_join(pitch, fastest.pitch.dat, by = 
                      c("pitcher", "year", "pitcher_name")) %>%
  mutate(diff_speed = start_speed - avg_fast_speed) %>%
  mutate(pitch_type = ifelse(pitch_type=="KC","CU",pitch_type))#Combine KC and CU

# Create Swing and Whiff Indicators
swinging <- c("In play, no out", "Foul", "In play, run(s)", "Swinging Strike", 'Foul Tip',
              "Foul (Runner Going)", "Swinging Strike (Blocked)")
whiff <- c("Swinging Strike", "Swinging Strike (Blocked)")

pitch$swinging <- as.numeric(pitch$des %in% swinging)
pitch$whiff <- as.numeric(pitch$des %in% whiff)

### Dataset for Starting Pitchers
starters.dat <- pitch %>%
  filter(inning <= 1) 

starter_games <- starters.dat %>%
  group_by(gameday_link, pitcher, pitcher_name) %>%
  summarise(n = n()) %>%
  dplyr::select(-n)

starters_pitch <- inner_join(pitch, starter_games)

starters_pitch <- starters_pitch %>%
  group_by(gameday_link, pitcher) %>%
  filter(sum(is.na(sv_id)) == 0 & sum(sv_id == "") == 0) %>%
  arrange(sv_id) %>%
  mutate(prev_pitches = row_number() - 1) %>%
  ungroup() %>%
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

save(starters_pitch, file = "Data/starters_pitch.Rdata")

### Dataset for Relief Pitchers
# Remove years for pitchers who threw fewer than 300 ptiches
kPitchCutoff = 300

relief_pitch <- pitch %>%
  filter(!(pitcher %in% starters.dat$pitcher)) %>%
  group_by(pitcher, year) %>%
  filter(n() >= kPitchCutoff) %>%
  ungroup() %>%
  mutate(pitcher = factor(pitcher))


# Calculate number of pitches thrown in each day
kSecondsInDay = 24*3600
kLookbackDays = 7


#Get a dataset with row for each pitcher/game for num.pitches
player.game = relief_pitch %>%
  group_by(date, gameday_link,pitcher, pitcher_name) %>%
  summarise(num.pitches = n())%>%
  mutate(num.date = as.numeric(strptime(date, format = "%Y-%m-%d"))/kSecondsInDay,
         year = substr(date,1,4)) %>%
  arrange(pitcher, num.date)


#Dataset for num.pitches thrown in last 7 days
npitch = matrix(0, nrow = nrow(player.game), ncol = kLookbackDays)
colnames(npitch) = paste0("npitch",1:kLookbackDays)
for(i in 1:nrow(player.game)){
  for(j in 1:kLookbackDays){
    index = ifelse((i-j)>0, i-j, NA) 
    if(!is.na(index) && #is lookback valid
       (player.game$pitcher[i] == player.game$pitcher[(i-j)]) && #same pitcher?
       (player.game$num.date[i] - player.game$num.date[(i-j)] <= kLookbackDays)){ #within a week?
      d = (player.game$num.date[i] - player.game$num.date[i-j]) #which day in late 7
      npitch[i,d] = player.game$num.pitches[i-j]
    }
  }
}

relief_games <- as.data.frame(cbind(as.matrix(player.game),npitch)) %>%
  dplyr::select(-num.date) %>%
  ungroup() %>%
  mutate(pitcher = factor(pitcher), date = as.Date(date))

relief_pitch <- left_join(relief_pitch, relief_games) %>%
  ungroup() %>%
  arrange(pitcher, gameday_link) %>%
  mutate(date = as.Date(date), pitcher = factor(pitcher)) %>%
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

save(relief_games, file = "Data/relief_games.Rdata")
save(relief_pitch, file = "Data/relief_pitch.Rdata")

### Dataset for Pitch Quality

# Remove uncommon pitcher/pitch type combos
pitch_quality <- pitch %>%
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

# Remove In-Play Bunts
inplay <- c("In play, no out", "In play, run(s)")
bunt.events <- c("Bunt Lineout", "Bunt Popout", "Bunt Groundout", "Sac Bunt",
                 "Sacrifice Bunt DP")

pitch.swing <- pitch %>%
  filter(swinging == 1) %>%
  filter(!(des %in% inplay & event %in% bunt.events))

# Dataset with only full swings- used to model whiff rates
write.csv(pitch.swing,file = "Data/pitch_swing.csv")



