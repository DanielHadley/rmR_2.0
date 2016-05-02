# setwd("/Users/dphnrome/Documents/Git/rmR_2.0/")
# setwd("C:/Users/dhadley/Documents/GitHub/rmR_2.0")
setwd("/home/pi/Github/rmR_2.0")

library(dplyr)
library(lubridate)
library(leafletR)
library(httr)

today <- Sys.Date()
yesterday <- today - 1
lastWeek <- today - 7
tenDaysAgo <- today - 10

todayText <- format(today, "%B %d")
yesterdayText <- format(yesterday, "%B %d")
lastWeekText <- format(lastWeek, "%B %d")
thisYear <- format(today, "%Y")
todayUSA <- format(today, '%m-%d-%Y')
thisWeekNumber <- week(today)


#### Chicago ####

allData <- read.csv("./data/Chicago_rats.csv")

# Make some variables 
allData <- allData %>%
  mutate(date = as.Date(Creation.Date, format="%m/%d/%Y")) %>%
  mutate(week = week(date),
         year = year(date))

## Yesterday 
d <- allData %>%
  filter(date == yesterday)

# Num calls
chiYesterday <- nrow(d)

## Last week 
d <- allData %>%
  filter(date >= lastWeek)

# Num calls
chiLastWeek <- nrow(d)


# older than 2009 is bad comparison
# and get rid of this year,
pastYears <- allData %>%
  filter(year > 2009 & year < thisYear) 

# comparison average
averageForThisTime <- pastYears %>%
  group_by(year, week) %>%
  summarise(Events = n()) %>%
  filter(week == thisWeekNumber-2 |
           week == thisWeekNumber-1 |
           week == thisWeekNumber |
           week == thisWeekNumber+1 |
           week == thisWeekNumber+2)

chiAverageForThisTime <- round(mean(averageForThisTime$Events))


# Now make geojson and save it to ratmaps
dailyMap_chi <- d %>% 
  select(Latitude, Longitude, date) %>% 
  filter(!is.na(Latitude)) %>%
  rename(latitude = Latitude, longitude = Longitude)


# Convert
toGeoJSON(dailyMap_chi, "daily_chicago", "tmp/")

# and upload to myjson
PUT(url = "https://api.myjson.com/bins/3f9uo", body = upload_file("./tmp/daily_chicago.geojson"))




#### NYC ####

allData <- read.csv("./data/NYC_rats.csv")

# Make some variables 
allData <- allData %>%
  mutate(dateTime = mdy_hms(Created.Date, tz='EST')) %>%
  mutate(date = as.Date(dateTime)) %>%
  mutate(week = week(date),
         year = year(date))

## Yesterday 
d <- allData %>%
  filter(date == yesterday)

# Num calls
nycYesterday <- nrow(d)


## Last week 
d <- allData %>%
  filter(date >= lastWeek)

# Num calls
nycLastWeek <- nrow(d)


# older than 2009 is bad comparison
# and get rid of this year,
pastYears <- allData %>%
  filter(year > 2009 & year < thisYear) 

# comparison average
averageForThisTime <- pastYears %>%
  group_by(year, week) %>%
  summarise(Events = n()) %>%
  filter(week == thisWeekNumber-2 |
           week == thisWeekNumber-1 |
           week == thisWeekNumber |
           week == thisWeekNumber+1 |
           week == thisWeekNumber+2)

nycAverageForThisTime <- round(mean(averageForThisTime$Events))


# Now make geojson and save it to ratmaps
dailyMap_nyc <- d %>% 
  select(Latitude, Longitude, date) %>% 
  filter(Latitude != "NA") %>%
  rename(latitude = Latitude, longitude = Longitude)


# Convert 
toGeoJSON(dailyMap_nyc, "daily_nyc", "tmp/")

# and upload to myjson
PUT(url = "https://api.myjson.com/bins/4z7qo", body = upload_file("./tmp/daily_nyc.geojson"))




#### Boston ####

allData <- read.csv("./data/Boston_rats.csv")

# Make some variables 
allData <- allData %>%
  mutate(dateTime = mdy_hms(OPEN_DT, tz='EST')) %>%
  mutate(date = as.Date(dateTime)) %>%
  mutate(week = week(date),
         year = year(date))

## Yesterday 
d <- allData %>%
  filter(date == yesterday)

# Num calls
bosYesterday <- nrow(d)


## Last week 
d <- allData %>%
  filter(date >= lastWeek)

# Num calls
bosLastWeek <- nrow(d)


# older than 2009 is bad comparison
# and get rid of this year,
pastYears <- allData %>%
  filter(year > 2009 & year < thisYear) 

# comparison average
averageForThisTime <- pastYears %>%
  group_by(year, week) %>%
  summarise(Events = n()) %>%
  filter(week == thisWeekNumber-2 |
           week == thisWeekNumber-1 |
           week == thisWeekNumber |
           week == thisWeekNumber+1 |
           week == thisWeekNumber+2)

bosAverageForThisTime <- round(mean(averageForThisTime$Events))


# Now make geojson and save it to ratmaps
dailyMap_bos <- d %>% 
  select(LATITUDE, LONGITUDE, date) %>% 
  filter(LATITUDE != "NA") %>%
  rename(latitude = LATITUDE, longitude = LONGITUDE)


# Convert 
toGeoJSON(dailyMap_bos, "daily_boston", "../ratmaps/geo/")

# and upload to myjson
PUT(url = "https://api.myjson.com/bins/zlzk", body = upload_file("./tmp/daily_boston.geojson"))




#### SF ####

allData <- read.csv("./data/SanFrancisco_rats.csv")

# Make some variables 
allData <- allData %>%
  mutate(dateTime = mdy_hms(Opened, tz='EST')) %>%
  mutate(date = as.Date(dateTime)) %>%
  mutate(week = week(date),
         year = year(date))

## Yesterday 
d <- allData %>%
  filter(date == yesterday)

# Num calls
sfYesterday <- nrow(d)


## Last week 
d <- allData %>%
  filter(date >= lastWeek)

# Num calls
sfLastWeek <- nrow(d)


# older than 2009 is bad comparison
# and get rid of this year,
pastYears <- allData %>%
  filter(year > 2009 & year < thisYear) 

# comparison average
averageForThisTime <- pastYears %>%
  group_by(year, week) %>%
  summarise(Events = n()) %>%
  filter(week == thisWeekNumber-2 |
           week == thisWeekNumber-1 |
           week == thisWeekNumber |
           week == thisWeekNumber+1 |
           week == thisWeekNumber+2)

sfAverageForThisTime <- round(mean(averageForThisTime$Events))


# Now make geojson and save it to ratmaps
dailyMap_sf <- allData %>% 
  filter(year == 2015) %>%
  separate(Point, c("y", "x"), ",") %>%
  mutate(y = gsub("\\(", "", y), x = gsub("\\)", "", x)) %>%
  mutate(longitude = as.numeric(x), latitude = as.numeric(y)) %>%
  select(latitude, longitude, date)
  
# Convert and upload to our server
toGeoJSON(dailyMap_sf, "daily_sf", "tmp/")