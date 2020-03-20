## Processing Xiaomeng's Exposure Data for R

setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/haqast/data")
options(mc.cores=parallel::detectCores())

library(tidyverse)

exposure <- read_csv("data_for_r_v3.csv")
exposure <- janitor::clean_names(exposure)

exposure$newdate <- as.Date(exposure$date, format="%m/%d/%Y")
exposure$month   <- as.POSIXlt(exposure$newdate)$mon + 1    
exposure$year    <- as.POSIXlt(exposure$newdate)$year + 1900 

# Removing Lake Ontario (not a county)
exposure <- filter(exposure, county != "Lake Ontario")

outcome <- read_csv("ed_admisions_20022012.csv")
outcome <- janitor::clean_names(outcome)
outcome$newdate <- as.Date(outcome$admission_start_care_date, format="%m/%d/%Y")

meteor <- read_csv("nldas_county.csv")
meteor$newdate <- as.Date(meteor$date, format="%m/%d/%Y")

exposure <- merge(exposure, meteor, by=c("newdate", "county"), all = TRUE)

dta <- merge(exposure, outcome, by=c("newdate", "county"), all = TRUE)
write.csv(dta, "C:/Users/profu/Desktop/newExposure.csv")
