## HAQAST Analysis for January 2019

## This analysis uses version 3 of the dataset Xiaomeng provided, and runs the same models that were used for HAQAST5

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


##########Full Analysis##########
model_cmaq_new <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + ns(as.numeric(date), df = 11*4)
                      + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = dta, family = quasipoisson())
100*(exp(model_cmaq_new$coefficients[2]*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] - 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] + 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)

model_fused <- glm(cvd_in ~ pm_fused + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = dta, family = quasipoisson())
100*(exp(model_fused$coefficients[2]*10)-1)
100*(exp((summary(model_fused)$coef[2,1] - 1.96* summary(model_fused)$coef[2,2])*10)-1)
100*(exp((summary(model_fused)$coef[2,1] + 1.96* summary(model_fused)$coef[2,2])*10)-1)

model_cdc <- glm(cvd_in ~ pm_cdc + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = dta, family = quasipoisson())
100*(exp(model_cdc$coefficients[2]*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] - 1.96* summary(model_cdc)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] + 1.96* summary(model_cdc)$coef[2,2])*10)-1)

model_cdc_new <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = dta, family = quasipoisson())
100*(exp(model_cdc_new$coefficients[2]*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] - 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] + 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)

model_emory <- glm(cvd_in ~ pm_emory + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = dta, family = quasipoisson())
100*(exp(model_emory$coefficients[2]*10)-1)
100*(exp((summary(model_emory)$coef[2,1] - 1.96* summary(model_emory)$coef[2,2])*10)-1)
100*(exp((summary(model_emory)$coef[2,1] + 1.96* summary(model_emory)$coef[2,2])*10)-1)


##########Restricted on AQS Counties Only##########
aqsdta <- dta[which(dta$pm_aqs!="NA"),]
model_aqs <- glm(cvd_in ~ pm_aqs + as.factor(county) + ns(as.numeric(date), df = 11*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_aqs$coefficients[2]*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] - 1.96* summary(model_aqs)$coef[2,2])*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] + 1.96* summary(model_aqs)$coef[2,2])*10)-1)

model_cmaq_new <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + ns(as.numeric(date), df = 11*4)
                      + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_cmaq_new$coefficients[2]*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] - 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] + 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)

model_fused <- glm(cvd_in ~ pm_fused + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_fused$coefficients[2]*10)-1)
100*(exp((summary(model_fused)$coef[2,1] - 1.96* summary(model_fused)$coef[2,2])*10)-1)
100*(exp((summary(model_fused)$coef[2,1] + 1.96* summary(model_fused)$coef[2,2])*10)-1)

model_cdc <- glm(cvd_in ~ pm_cdc + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_cdc$coefficients[2]*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] - 1.96* summary(model_cdc)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] + 1.96* summary(model_cdc)$coef[2,2])*10)-1)

model_cdc_new <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_cdc_new$coefficients[2]*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] - 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] + 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)

model_emory <- glm(cvd_in ~ pm_emory + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = aqsdta, family = quasipoisson())
100*(exp(model_emory$coefficients[2]*10)-1)
100*(exp((summary(model_emory)$coef[2,1] - 1.96* summary(model_emory)$coef[2,2])*10)-1)
100*(exp((summary(model_emory)$coef[2,1] + 1.96* summary(model_emory)$coef[2,2])*10)-1)


##########Complete Case Analysis##########
complete.dta <- dta[which(dta$pm_aqs!="NA" & dta$pm_cmaq_new!="NA" & dta$pm_fused!="NA"
                          & dta$pm_cdc!="NA" & dta$pm_cdc_new!="NA" & dta$pm_emory!="NA"),]
model_aqs <- glm(cvd_in ~ pm_aqs + as.factor(county) + ns(as.numeric(date), df = 11*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_aqs$coefficients[2]*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] - 1.96* summary(model_aqs)$coef[2,2])*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] + 1.96* summary(model_aqs)$coef[2,2])*10)-1)

model_cmaq_new <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + ns(as.numeric(date), df = 11*4)
                      + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_cmaq_new$coefficients[2]*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] - 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] + 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)

model_fused <- glm(cvd_in ~ pm_fused + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_fused$coefficients[2]*10)-1)
100*(exp((summary(model_fused)$coef[2,1] - 1.96* summary(model_fused)$coef[2,2])*10)-1)
100*(exp((summary(model_fused)$coef[2,1] + 1.96* summary(model_fused)$coef[2,2])*10)-1)

model_cdc <- glm(cvd_in ~ pm_cdc + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_cdc$coefficients[2]*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] - 1.96* summary(model_cdc)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc)$coef[2,1] + 1.96* summary(model_cdc)$coef[2,2])*10)-1)

model_cdc_new <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + ns(as.numeric(date), df = 9*4)
                 + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_cdc_new$coefficients[2]*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] - 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] + 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)

model_emory <- glm(cvd_in ~ pm_emory + as.factor(county) + ns(as.numeric(date), df = 11*4)
                   + ns(temp, df = 3) + ns(rh, df = 3) + as.factor(dow), data = complete.dta, family = quasipoisson())
100*(exp(model_emory$coefficients[2]*10)-1)
100*(exp((summary(model_emory)$coef[2,1] - 1.96* summary(model_emory)$coef[2,2])*10)-1)
100*(exp((summary(model_emory)$coef[2,1] + 1.96* summary(model_emory)$coef[2,2])*10)-1)


##########New Plot of Confidence Intervals##########
ci <- read_csv("confidence_intervals_no_old_cdc.csv")
ci$model <- factor(ci$model, levels = c("AQS", "CMAQ", "Fused", "CDC", "Emory"))

ggplot(ci, aes(x=model, y=pi, color = ci$type, min = 0, max = 2.2)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_discrete(name="Analysis Type", breaks=c("all", "aqs", "cca"),
                       labels=c("All Data", "AQS Only", "Complete Case")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")
