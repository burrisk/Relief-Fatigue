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
studied.years <- c(2012:2016)
pitch <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(as.numeric(year) %in% studied.years)

# Create Balls and Strikes Variables
pitch$b <- substr(pitch$count, 1, 1)
pitch$s <- substr(pitch$count, 3, 3)

# Remove years for pitchers who had greater than two starts
max.starts <- 0
starters.dat <- pitch %>%
  filter(!is.na(sv_id)) %>%
  group_by(gameday_link) %>%
  arrange(sv_id) %>%
  filter(row_number() == 1) %>%
  dplyr::select(gameday_link, pitcher, year)

starts.year.dat <- starters.dat %>%
  group_by(pitcher, year) %>%
  summarise(n_starts = n()) %>%
  filter(n_starts > max.starts)

pitch.split <- split(pitch, pitch$year)
pitcher.exclude.split <- split(starts.year.dat, starts.year.dat$year)
pitch.split <- lapply(1:length(studied.years), function(i){
  exclude.vec <- pitcher.exclude.split[[i]]$pitcher
  pitch.split[[i]] %>%
    filter(!(pitcher %in% exclude.vec))
})

pitch.relief.all <- do.call(rbind, pitch.split)
rm(list=setdiff(ls(), "pitch.relief.all"))


# TODO (Jake) Take the dataset pitch.relief.all and get npitch1:npitch7 for each pitcher 
# and date.  Only include relievers who throw more than 100 pitches in a given year


kSecondsInDay = 24*3600
kPitchCutoff = 100
kLookbackDays = 7

#Get a dataset with row for each pitcher/game for num.pitches
player.game = pitch.relief.all %>%
  group_by(date, gameday_link,pitcher, pitcher_name) %>%
  summarise(num.pitches = n())%>%
  mutate(num.date = as.numeric(strptime(date, format = "%Y-%m-%d"))/kSecondsInDay,
         year = substr(date,1,4))

#Only relievers who threw 100 pitches in a given year
valid.pitcher <- pitch.relief.all %>%
  group_by(pitcher,year) %>%
  summarise(tot.pitches = n()) %>%
  filter(tot.pitches >= kPitchCutoff) %>%
  select(-tot.pitches) %>%
  inner_join(player.game)



#Dataset for num.pitches thrown in last 7 days
npitch = matrix(0, nrow = nrow(valid.pitcher), ncol = kLookbackDays)
colnames(npitch) = paste0("npitch",1:kLookbackDays)
for(i in 1:nrow(valid.pitcher)){
  for(j in 1:kLookbackDays){
    index = ifelse((i-j)>0, i-j, NA) 
    if(!is.na(index) && #is lookback valid
       (valid.pitcher$pitcher[i] == valid.pitcher$pitcher[(i-j)]) && #same pitcher?
       (valid.pitcher$num.date[i] - valid.pitcher$num.date[(i-j)] <= kLookbackDays)){ #within a week?
      d = (valid.pitcher$num.date[i] - valid.pitcher$num.date[i-j]) #which day in late 7
      npitch[i,d] = valid.pitcher$num.pitches[i-j]
    }
  }
}

relief.lookback <- as.data.frame(cbind(valid.pitcher,npitch)) %>%
  select(-num.date)

rm(valid.pitcher,npitch,player.game,d,i,index,j)

# 

# Find fastest average pitch for each reliever in a given year
fastest.pitch.dat <- pitch.relief.all %>%
  filter(!is.na(start_speed)) %>%
  filter(!(pitch_type %in% c("PO", "AB", "EP", "SC", "UN", "FO"))) %>%
  group_by(pitcher, year, pitch_type) %>%
  filter(n() >= 3) %>%
  summarise(avg_fast_speed = mean(start_speed)) %>%
  arrange(desc(avg_fast_speed)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pitch_type)

pitch.relief.all <- inner_join(pitch.relief.all, fastest.pitch.dat, by = 
                                     c("pitcher", "year"))

# Create Swing and Whiff Indicators
swinging <- c("In play, no out", "Foul", "In play, run(s)", "Swinging Strike", 'Foul Tip',
              "Foul (Runner Going)", "Swinging Strike (Blocked)")
whiff <- c("Swinging Strike", "Swinging Strike (Blocked)")

pitch.relief.all$swinging <- as.numeric(pitch.relief.all$des %in% swinging)
pitch.relief.all$whiff <- as.numeric(pitch.relief.all$des %in% whiff)

# Only include common pitches
keep.pitches <- c("CH", "CU", "FC", "FF", "FS", "FT", "KC", "SI", "SL")
pitches.kept <- pitch.relief.all %>%
  filter(pitch_type %in% keep.pitches)


# This is the dataset where we estimate nastiness of each pitch
save(pitches.kept, file = "Data/AllStandardPitches.Rdata")



pitch.swing <- pitches.kept %>%
  filter(swinging == 1)

# Remove In-Play Bunts
inplay <- c("In play, no out", "In play, run(s)")
bunt.events <- c("Bunt Lineout", "Bunt Popout", "Bunt Groundout", "Sac Bunt",
                 "Sacrifice Bunt DP")
pitch.swing <- pitch.swing %>%
  filter(!(des %in% inplay & event %in% bunt.events))

# Dataset with only full swings- used to model whiff rates
save(pitch.swing, file = "Data/Swings.Rdata")



