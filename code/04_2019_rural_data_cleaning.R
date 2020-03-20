#### Data Cleaning for NY Urban/Rural Data
#### Mike He
#### April 3, 2019

#####1. Set working directory, loading packages, and reading in data####
setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/haqast/data")
options(mc.cores=parallel::detectCores())

library(tidyverse)
library(reshape)

rawdata <- read_csv("PctUrbanRural_County.csv")


#####2. Data Wrangling####
## filter,select states

dta <- filter(rawdata, STATENAME == "New York")
dta <- select(dta, COUNTYNAME, POP_COU, POPPCT_URBAN, AREAPCT_URBAN, POPPCT_RURAL, AREAPCT_RURAL)

## load haqast data
haqast <- read_csv("processed_haqast_v3.csv")

## Change "St. Lawrence to Saint Lawrence"
dta <- rename(dta, c(COUNTYNAME="county"))
dta$county[dta$county=="St. Lawrence"] <- "Saint Lawrence"
all.equal(unique(haqast$county), unique(dta$county))

## Merging
final <- left_join(haqast, dta, by = "county")
final %>% janitor::clean_names()

## Break down analysis into season
final$season <- "Spring"
final$season[final$month>=6 & final$month<=8] <- "Summer"
final$season[final$month>=9 & final$month<=11] <- "Autumn"
final$season[final$month==12 | final$month==1 | final$month==2] <- "Winter"

## Save as new file
write.csv(final, "processed_haqast_v4.csv")