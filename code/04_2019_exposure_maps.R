## Plots for HAQAST5

setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/haqast/data")
options(mc.cores=parallel::detectCores())

library(tidyverse)
library(reshape)
library(splines)

dta <- read_csv("processed_haqast_v4.csv")
dta$date <- as.Date(as.character(dta$date), format="%m/%d/%Y")
dta$dow     <- as.POSIXlt(dta$date)$wday

## various Excel to R conversion problems; converting characters to numeric/integer
dta$pm_cdc <- as.numeric(dta$pm_cdc)
dta$pm_cdc_new <- as.numeric(dta$pm_cdc_new)
dta$resp_out <- as.integer(dta$resp_out)
dta$cvd_out <- as.integer(dta$cvd_out)


summary <- dta %>%
  group_by(county) %>%
  summarize(pm_aqs=mean(pm_aqs, na.rm = TRUE),
            pm_cmaq = mean(pm_cmaq_new, na.rm = TRUE),
            pm_fused = mean(pm_fused, na.rm = TRUE),
            pm_cdc = mean(pm_cdc_new, na.rm = TRUE),
            pm_emory = mean(pm_emory, na.rm = TRUE))



##########MAKING MAPS FOR RESULTS SLIDE#########
library(usmap)

mapdata <- usmap::us_map(regions = "county", include = c("New York"))
fips <- read_csv("fips_codes_ny.csv")
summary <- merge(summary, fips, by = "county")
summary$fips <- summary$fips + 36000

mid <- mean(summary$pm_aqs, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_aqs") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab", 
                       limits = c(2, 20)) + theme(legend.position = "bottom")

#mid <- mean(summary$pm_cmaq_new, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_cmaq") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 21))

#mid <- mean(summary$pm_fused, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_fused") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20))

#mid <- mean(summary$pm_cdc, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_cdc") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20))

mid <- mean(summary$pm_emory, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_emory") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20))



##########NEW PLOTS##########
mid <- mean(summary$pm_aqs, na.rm=TRUE)
plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_aqs") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab", 
                       limits = c(2, 20), guide = FALSE) # + theme(legend.position = "bottom")

plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_cmaq") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 21), guide = FALSE)

plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_fused") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20), guide = FALSE)

plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_cdc") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20), guide = FALSE)

plot_usmap(regions = "county", include = c("New York"), data = summary, values = "pm_emory") + 
  theme(legend.position="right") + labs(fill = expression("PM"[2.5]*" (μg/m"^3*")")) + 
  scale_fill_gradient2(midpoint = mid, low="blue", mid="white", high="red", space="Lab",
                       limits = c(2, 20), guide = FALSE)

