# Set working directory
# setwd("~/Dropbox/GS/Research/Sports/Baseball/Relief-Fatigue")

# Load Packages
list.of.packages <- c("pitchRx","RSQLite", "dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(pitchRx)
library(RSQLite)
library(dplyr)

# Scrape Data
my_db <- src_sqlite("Data/pitchRx.sqlite3", create = TRUE)
scrape(start = "2008-01-01", end = "2017-01-01", connect = my_db$con)





