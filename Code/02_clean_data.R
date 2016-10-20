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
pitch$reg_season <- as.numeric(pitch$date >= mlb.schedule[y,"start"]) & (pitch$date <= mlb.schedule[y,"end"])
pitch$post_season <- as.numeric(pitch$date > mlb.schedule[y,"end"])
pitch$year <- y

# Exclude Spring Training games and Years not in Study
studied.years <- c(2012:2016)
pitch_cleaner <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(as.numeric(year) %in% studied.years)
  


