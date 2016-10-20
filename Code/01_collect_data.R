# Load Packages
list.of.packages <- c("dplyr","RSQLite")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(RSQLite)
library(dplyr)

# Set Working Directory
# setwd("~/Dropbox/GS/Research/Sports/Baseball/Relief-Fatigue")

# Connect to Database
my_db <- src_sqlite("Data/pitchRx.sqlite3", create = FALSE)

# At-Bat Data
at_bat <- my_db %>%
  tbl("atbat") %>%
  select(gameday_link, batter, pitcher, num, event,
                 inning_side, inning, batter_name, pitcher_name,
                 date,o)

# Pitch Data
pitch <- my_db %>%
  tbl("pitch") %>%
  select(des, type, x, y, start_speed, end_speed, sz_top,
               sz_bot, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y,
               break_angle, break_length, pitch_type, spin_dir, spin_rate,
               inning_side, inning, num, count, zone, gameday_link, sv_id)


# Merge the two datasets
pitch <- inner_join(at_bat, pitch, by = c("num", "gameday_link", "inning",
                                          "inning_side"))

# Collect the Data as an R object  
pitch <- collect(pitch) 
save(pitch, file = "Data/pitch_raw.Rdata")
