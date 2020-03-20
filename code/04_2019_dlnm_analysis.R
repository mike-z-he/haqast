#### DLNM Models using HAQAST Data
#### Mike He
#### April 25, 2019

setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/haqast/data")
options(mc.cores=parallel::detectCores())

library(tidyverse)
library(mgcv)
library(dlnm)
library(splines)
library(gplots)
library(MuMIn)
library(dplyr)
library(reshape)

dta <- read_csv("processed_haqast_v4.csv")
dta$date <- as.Date(as.character(dta$date), format="%m/%d/%Y")
dta$dow     <- as.POSIXlt(dta$date)$wday

dta <- dta[order(dta$date),]
dta <- dta[order(dta$county, dta$date),]

## various Excel to R conversion problems; converting characters to numeric/integer
dta$pm_cdc <- as.numeric(dta$pm_cdc)
dta$pm_cdc_new <- as.numeric(dta$pm_cdc_new)
dta$resp_out <- as.integer(dta$resp_out)
dta$cvd_out <- as.integer(dta$cvd_out)

##### mak #######

hist(dta$pm_emory)
plot(dta$pm_emory, dta$cvd_in)

dta_m <- aggregate(cbind(pm_emory, pm_aqs, temp, rh) ~ month + year + county, data = dta, FUN = mean, na.rm = TRUE)
summary(dta_m)

cvd_m <- aggregate(cvd_in ~ month + year + county, data = dta, FUN = sum, na.rm = TRUE)

dta_m <- merge(cvd_m, dta_m, by = c("county", "year", "month"))

mod1 <- gam(cvd_in ~ pm_aqs + s(temp, fx=TRUE, k = 4) + s(rh, fx=TRUE, k = 4) + as.factor(month) + as.factor(year) + as.factor(county),
            data = dta_m, family = "quasipoisson")

plot(mod1, select=1)
summary(mod1)


dta_y <- aggregate(cbind(pm_emory, temp, rh) ~ year + county, data = dta, FUN = mean, na.rm = TRUE)
summary(dta_y)

cvd_y <- aggregate(cvd_in ~ year + county, data = dta, FUN = sum, na.rm = TRUE)

dta_y <- merge(cvd_y, dta_y, by = c("county", "year"))

mod2 <- gam(cvd_in ~ s(pm_emory) + as.factor(year) + as.factor(county),
            data = dta_y, family = "quasipoisson")

plot(mod2)
summary(mod2)


pacf(na.omit(dta$pm_aqs))
pacf(na.omit(dta$pm_emory))
pacf(na.omit(dta$pm_fused))

##################



## Keeping bottom 95%
dta <- filter(dta, pm_emory <= 20)
dta <- filter(dta, pm_fused <= 21)
dta <- filter(dta, pm_aqs <= 24)





##########DLNM MODELS##########
## Emory data for exposure only

## Crossbasis
cb_pm_lin_ns3 <- crossbasis(dta$pm_emory, lag=30, argvar=list(fun="lin"),
                            arglag=list(df=3), group=dta$county)
cb_pm_lin_ns4 <- crossbasis(dta$pm_emory, lag=30, argvar=list(fun="lin"),
                            arglag=list(df=4), group=dta$county)
cb_pm_lin_ns5 <- crossbasis(dta$pm_emory, lag=30, argvar=list(fun="lin"),
                            arglag=list(df=5), group=dta$county)

cb_temp_ns3_ns3 <- crossbasis(dta$temp, lag=30, argvar=list(df=3),
                              arglag=list(df=3), group=dta$county)
cb_rh_ns3_ns3 <- crossbasis(dta$rh, lag=30, argvar=list(df=3),
                            arglag=list(df=3), group=dta$county)


## Models
lin_ns3 <- glm(cvd_in ~  offset(log(pop_cou)) + cb_pm_lin_ns3 + cb_temp_ns3_ns3 + cb_rh_ns3_ns3 + as.factor(county) + 
             ns(as.numeric(date), df = 4*11) + as.factor(dow), data = dta, family = quasipoisson())
lin_ns4 <- glm(cvd_in ~  offset(log(pop_cou)) + cb_pm_lin_ns4 + cb_temp_ns3_ns3 + cb_rh_ns3_ns3 + as.factor(county) + 
                 ns(as.numeric(date), df = 4*11) + as.factor(dow), data = dta, family = quasipoisson())
lin_ns5 <- glm(cvd_in ~  offset(log(pop_cou)) + cb_pm_lin_ns5 + cb_temp_ns3_ns3 + cb_rh_ns3_ns3 + as.factor(county) + 
                 ns(as.numeric(date), df = 4*11) + as.factor(dow), data = dta, family = quasipoisson())


## Crosspred
pred.cvd_lin_3 <- crosspred(cb_pm_lin_ns3, lin_ns3, at=0:20, bylag=0.2, cumul=TRUE)
pred.cvd_lin_4 <- crosspred(cb_pm_lin_ns4, lin_ns4, at=0:20, bylag=0.2, cumul=TRUE)
pred.cvd_lin_5 <- crosspred(cb_pm_lin_ns5, lin_ns5, at=0:20, bylag=0.2, cumul=TRUE)


## Linear Plots
plot(pred.cvd_lin_3, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main=expression('Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 3df'))
plot(pred.cvd_lin_3, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
     main=expression('Cumulative Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 3df'))

plot(pred.cvd_lin_4, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main=expression('Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 4df'))
plot(pred.cvd_lin_4, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
     main=expression('Cumulative Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 4df'))

plot(pred.cvd_lin_5, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main=expression('Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 5df'))
plot(pred.cvd_lin_5, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
     main=expression('Cumulative Association with a 10-µg/m'^3*' Increase in PM'[2.5]*', Cardiovascular Mortality, 5df'))

