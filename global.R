# Deployed to
# https://cschutten.shinyapps.io/project/

library(ggplot2)
library(stringr)
library(readr)
library(dplyr)
library(maps)
library(rsconnect)
library(DT)

## Read in file names & create lists of stations based on pollutant ##
stations <- read_csv("Data/__Stations.csv")
files <- list.files(path="Data",pattern="^CZ0")
PM2.5 <- stations[stations$EoICode %in% str_sub(files[str_detect(files, "PM2.5")], 1, 7),8]
PM10 <- stations[stations$EoICode %in% str_sub(files[str_detect(files, "PM10")], 1, 7),8]
NO2 <- stations[stations$EoICode %in% str_sub(files[str_detect(files, "NO2")], 1, 7),8]
SO2 <- stations[stations$EoICode %in% str_sub(files[str_detect(files, "SO2")], 1, 7),8]
PM2.5 <- PM2.5[order(PM2.5$StationName),]
PM10 <- PM10[order(PM10$StationName),]
NO2 <- NO2[order(NO2$StationName),]
SO2 <- SO2[order(SO2$StationName),]

pollutants <- c("Fine particulates (PM2.5)"="PM2.5", "Particulates (PM10)"="PM10",
                "Sulphur dioxide (SO2)"="SO2", "Nitrogen dioxide (NO2)"="NO2")

aggregations <- c("None"="none", 
                  "Daily Average"="daily_avg", 
                  "Yearly Average"="yearly_avg",
                  "Daily Maxima"="daily_max", 
                  "Number of hours per day threshold is exceeded"="thresh_hours_per_day",
                  "Number of hours per year threshold is exceeded"="thresh_hours_per_year",
                  "Number of days per year the daily average exceeds given threshold"="thresh_days_per_year")

xaxis <- c("Calendar Time"="calendar",
           "Seasonal Effect"="year",
           "Weekly Effect"="week",
           "Daily Effect"="day")