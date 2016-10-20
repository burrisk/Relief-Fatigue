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

# TODO (Jake) Take the dataset pitch.relief.all and get npitch1:npitch7 for each pitcher and date






# 

# Create Swing and Whiff Indicators
swinging <- c("In play, no out", "Foul", "In play, run(s)", "Swinging Strike", 'Foul Tip',
              "Foul (Runner Going)", "Swinging Strike (Blocked)")
whiff <- c("Swinging Strike", "Swinging Strike (Blocked)")

pitch.relief.all$swinging <- as.numeric(pitch.relief.all$des %in% swinging)
pitch.relief.all$whiff <- as.numeric(pitch.relief.all$des %in% whiff)

pitch.relief.swing <- pitch.relief.all %>%
  filter(swinging == 1)

# Remove In-Play Bunts
inplay <- c("In play, no out", "In play, run(s)")
bunt.events <- c("Bunt Lineout", "Bunt Popout", "Bunt Groundout", "Sac Bunt",
                 "Sacrifice Bunt DP")
pitch.relief.swing <- pitch.relief.swing %>%
  filter(!(des %in% inplay & event %in% bunt.events))

# Only include common pitches
keep.pitches <- c("CH", "CU", "FC", "FF", "FS", "FT", "KC", "SI", "SL")
pitch.relief.swing <- pitch.relief.swing %>%
  filter(pitch_type %in% keep.pitches)

save(pitch.relief.swing, file = "Data/SwingingData.Rdata")


