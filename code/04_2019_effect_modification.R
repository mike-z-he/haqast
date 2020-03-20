#### Testing Spatial-Temporal Effect Modification in HAQAST
#### Mike He
#### April 3, 2019

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

## Weighted Average?
dta$pop_rural <- dta$pop_cou * (dta$poppct_rural/100)
dta$pop_urban <- dta$pop_cou * (dta$poppct_urban/100)



####1. Spatial Effect-Modification####
## Note that all analyses performed for "full-analysis" only

model_aqs <- glm(cvd_in ~ pm_aqs + pm_aqs:pop_rural + as.factor(county) + 
                   ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_aqs)

model_cmaq_new <- glm(cvd_in ~ pm_cmaq_new + pm_cmaq_new:pop_rural + as.factor(county) + 
                   ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_cmaq_new)

model_fused <- glm(cvd_in ~ pm_fused + pm_fused:pop_rural + as.factor(county) + 
                   ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_fused)

model_cdc_new <- glm(cvd_in ~ pm_cdc_new + pm_cdc_new:pop_rural + as.factor(county) + 
                   ns(as.numeric(date), df = 9*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_cdc_new)

model_emory <- glm(cvd_in ~ pm_emory + pm_emory:pop_rural + as.factor(county) + 
                   ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_emory)


####2. Temporal Effect-Modification####
dta$season <- factor(as.character(dta$season), levels = c("Spring", "Summer", "Autumn", "Winter"))

model_aqs <- glm(cvd_in ~ pm_aqs + pm_aqs:as.factor(season) + as.factor(county) + 
                   ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta, family = quasipoisson())
summary(model_aqs)

model_cmaq_new <- glm(cvd_in ~ pm_cmaq_new + pm_cmaq_new:as.factor(season) + as.factor(county) + 
                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta, family = quasipoisson())
summary(model_cmaq_new)

model_fused <- glm(cvd_in ~ pm_fused + pm_fused:as.factor(season) + as.factor(county) + 
                     ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta, family = quasipoisson())
summary(model_fused)

model_cdc_new <- glm(cvd_in ~ pm_cdc_new + pm_cdc_new:as.factor(season) + as.factor(county) + 
                       ns(as.numeric(date), df = 9*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta, family = quasipoisson())
summary(model_cdc_new)

model_emory <- glm(cvd_in ~ pm_emory + pm_emory:as.factor(season) + as.factor(county) + 
                     ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta, family = quasipoisson())
summary(model_emory)

## Note to self: test significance using anova
anova(model_aqs, model_aqs2, test = "Chisq")
anova(model_cmaq_new, model_cmaq_new2, test = "Chisq")
anova(model_fused, model_fused2, test = "Chisq")
anova(model_cdc_new, model_cdc_new2, test = "Chisq")
anova(model_emory, model_emory2, test = "Chisq")

####3. Plotting Seasonal Confidence Intervals####
## Once again, these are ONLY for all available data (aqs for AQS, all for the other exposures)

## Spring
100*(exp(model_aqs$coefficients[2]*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] - 1.96* summary(model_aqs)$coef[2,2])*10)-1)
100*(exp((summary(model_aqs)$coef[2,1] + 1.96* summary(model_aqs)$coef[2,2])*10)-1)

100*(exp(model_cmaq_new$coefficients[2]*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] - 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cmaq_new)$coef[2,1] + 1.96* summary(model_cmaq_new)$coef[2,2])*10)-1)

100*(exp(model_fused$coefficients[2]*10)-1)
100*(exp((summary(model_fused)$coef[2,1] - 1.96* summary(model_fused)$coef[2,2])*10)-1)
100*(exp((summary(model_fused)$coef[2,1] + 1.96* summary(model_fused)$coef[2,2])*10)-1)

100*(exp(model_cdc_new$coefficients[2]*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] - 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc_new)$coef[2,1] + 1.96* summary(model_cdc_new)$coef[2,2])*10)-1)

100*(exp(model_emory$coefficients[2]*10)-1)
100*(exp((summary(model_emory)$coef[2,1] - 1.96* summary(model_emory)$coef[2,2])*10)-1)
100*(exp((summary(model_emory)$coef[2,1] + 1.96* summary(model_emory)$coef[2,2])*10)-1)


## Summer
vcov <- vcov(model_aqs)
coef <- model_aqs$coefficients[2]+model_aqs$coefficients[nrow(vcov)-2]
se <- sqrt(vcov[2,2] + vcov[(nrow(vcov)-2),(nrow(vcov)-2)] + 2*vcov[2,nrow(vcov)-2])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cmaq_new)
coef <- model_cmaq_new$coefficients[2]+model_cmaq_new$coefficients[nrow(vcov)-2]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-2,nrow(vcov)-2] + 2*vcov[2,nrow(vcov)-2])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_fused)
coef <- model_fused$coefficients[2]+model_fused$coefficients[nrow(vcov)-2]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-2,nrow(vcov)-2] + 2*vcov[2,nrow(vcov)-2])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cdc_new)
coef <- model_cdc_new$coefficients[2]+model_cdc_new$coefficients[nrow(vcov)-2]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-2,nrow(vcov)-2] + 2*vcov[2,nrow(vcov)-2])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_emory)
coef <- model_emory$coefficients[2]+model_emory$coefficients[nrow(vcov)-2]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-2,nrow(vcov)-2] + 2*vcov[2,nrow(vcov)-2])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)


## Autumn
vcov <- vcov(model_aqs)
coef <- model_aqs$coefficients[2]+model_aqs$coefficients[nrow(vcov)-1]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-1,nrow(vcov)-1] + 2*vcov[2,nrow(vcov)-1])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cmaq_new)
coef <- model_cmaq_new$coefficients[2]+model_cmaq_new$coefficients[nrow(vcov)-1]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-1,nrow(vcov)-1] + 2*vcov[2,nrow(vcov)-1])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_fused)
coef <- model_fused$coefficients[2]+model_fused$coefficients[nrow(vcov)-1]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-1,nrow(vcov)-1] + 2*vcov[2,nrow(vcov)-1])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cdc_new)
coef <- model_cdc_new$coefficients[2]+model_cdc_new$coefficients[nrow(vcov)-1]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-1,nrow(vcov)-1] + 2*vcov[2,nrow(vcov)-1])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_emory)
coef <- model_emory$coefficients[2]+model_emory$coefficients[nrow(vcov)-1]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov)-1,nrow(vcov)-1] + 2*vcov[2,nrow(vcov)-1])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

##Winter
vcov <- vcov(model_aqs)
coef <- model_aqs$coefficients[2]+model_aqs$coefficients[nrow(vcov)]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov),nrow(vcov)] + 2*vcov[2,nrow(vcov)])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cmaq_new)
coef <- model_cmaq_new$coefficients[2]+model_cmaq_new$coefficients[nrow(vcov)]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov),nrow(vcov)] + 2*vcov[2,nrow(vcov)])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_fused)
coef <- model_fused$coefficients[2]+model_fused$coefficients[nrow(vcov)]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov),nrow(vcov)] + 2*vcov[2,nrow(vcov)])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_cdc_new)
coef <- model_cdc_new$coefficients[2]+model_cdc_new$coefficients[nrow(vcov)]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov),nrow(vcov)] + 2*vcov[2,nrow(vcov)])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

vcov <- vcov(model_emory)
coef <- model_emory$coefficients[2]+model_emory$coefficients[nrow(vcov)]
se <- sqrt(vcov[2,2] + vcov[nrow(vcov),nrow(vcov)] + 2*vcov[2,nrow(vcov)])
100*(exp(coef*10)-1)
100*(exp((coef - 1.96* se)*10)-1)
100*(exp((coef + 1.96* se)*10)-1)

##Plot
ci <- read_csv("confidence_intervals_season.csv")
ci$model <- factor(ci$model, levels = c("AQS*", "CMAQ*", "Fused*", "CDC*", "Emory*"))
ci$type <- factor(ci$type, levels = c("all", "spring", "summer", "autumn", "winter"))

ggplot(ci, aes(x=model, y=pi, color = ci$type, min = -3, max = 5)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization by Season")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_discrete(name="Analysis Type", breaks=c("all", "spring", "summer", "autumn", "winter"),
                       labels=c("All Data", "Spring", "Summer", "Autumn", "Winter")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")



#### 4.Spatial Effect Modification using Stratified Models####
dta$poppct_rural.q <- cut(dta$poppct_rural, breaks = quantile(dta$poppct_rural, c(0,0.25,0.50,0.75,1)),
                          include.lowest = TRUE)

dta$poppct_rural.q2 <- ifelse(dta$poppct_rural.q == "(64.2,100]", 3, dta$poppct_rural.q)


model_aqs.q1 <- glm(cvd_in ~ pm_aqs + as.factor(county) + 
                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())
model_aqs.q2 <- glm(cvd_in ~ pm_aqs  + as.factor(county) + 
                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_aqs.q3 <- glm(cvd_in ~ pm_aqs  + as.factor(county) + 
                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

model_cmaq.q1 <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + 
                      ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                      as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())
model_cmaq.q2 <- glm(cvd_in ~ pm_cmaq_new  + as.factor(county) + 
                      ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                      as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_cmaq.q3 <- glm(cvd_in ~ pm_cmaq_new  + as.factor(county) + 
                      ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                      as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

model_fused.q1 <- glm(cvd_in ~ pm_fused + as.factor(county) + 
                       ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())
model_fused.q2 <- glm(cvd_in ~ pm_fused  + as.factor(county) + 
                       ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_fused.q3 <- glm(cvd_in ~ pm_fused  + as.factor(county) + 
                       ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

model_cdc.q1 <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + 
                        ns(as.numeric(date), df = 9*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())
model_cdc.q2 <- glm(cvd_in ~ pm_cdc_new  + as.factor(county) + 
                        ns(as.numeric(date), df = 9*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_cdc.q3 <- glm(cvd_in ~ pm_cdc_new  + as.factor(county) + 
                        ns(as.numeric(date), df = 9*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                        as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

model_emory.q1 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                         as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())
model_emory.q2 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                         as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_emory.q3 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                         as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

100*(exp(model_aqs.q3$coefficients[2]*10)-1)
100*(exp((summary(model_aqs.q3)$coef[2,1] - 1.96* summary(model_aqs.q3)$coef[2,2])*10)-1)
100*(exp((summary(model_aqs.q3)$coef[2,1] + 1.96* summary(model_aqs.q3)$coef[2,2])*10)-1)

100*(exp(model_cmaq.q3$coefficients[2]*10)-1)
100*(exp((summary(model_cmaq.q3)$coef[2,1] - 1.96* summary(model_cmaq.q3)$coef[2,2])*10)-1)
100*(exp((summary(model_cmaq.q3)$coef[2,1] + 1.96* summary(model_cmaq.q3)$coef[2,2])*10)-1)

100*(exp(model_fused.q3$coefficients[2]*10)-1)
100*(exp((summary(model_fused.q3)$coef[2,1] - 1.96* summary(model_fused.q3)$coef[2,2])*10)-1)
100*(exp((summary(model_fused.q3)$coef[2,1] + 1.96* summary(model_fused.q3)$coef[2,2])*10)-1)

100*(exp(model_cdc.q3$coefficients[2]*10)-1)
100*(exp((summary(model_cdc.q3)$coef[2,1] - 1.96* summary(model_cdc.q3)$coef[2,2])*10)-1)
100*(exp((summary(model_cdc.q3)$coef[2,1] + 1.96* summary(model_cdc.q3)$coef[2,2])*10)-1)

100*(exp(model_emory.q3$coefficients[2]*10)-1)
100*(exp((summary(model_emory.q3)$coef[2,1] - 1.96* summary(model_emory.q3)$coef[2,2])*10)-1)
100*(exp((summary(model_emory.q3)$coef[2,1] + 1.96* summary(model_emory.q3)$coef[2,2])*10)-1)


##Plot
ci <- read_csv("confidence_intervals_rural_quartiles.csv")
ci$model <- factor(ci$model, levels = c("AQS", "CMAQ", "Fused", "CDC", "Emory"))
ci$type <- factor(ci$type, levels = c("all", "q1", "q2", "q3"))

ggplot(ci, aes(x=model, y=pi, color = ci$type, min = -13, max = 10)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization by % Rural")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_discrete(name="Analysis Type", breaks=c("all", "q1", "q2", "q3"),
                       labels=c("All Data", "1st Quartile", "2nd Quartile", "3-4th Quartile")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")





####CODE FOR CONTINUOUS PLOT####
## Original dataset was created and modified using Marianthi's graph code
## We determined that the effect modification is non-linear, so this code is obsolete
ci2 <- read_csv("confidence_intervals_resp_rural.csv")

ggplot(ci2, aes(x=percent_rural, min = -2, max = 3)) +
  geom_line(aes(y = aqs_pi, colour = "red"), size = 2) + 
  geom_ribbon(aes(ymin = aqs_lpi, ymax=aqs_upi), alpha=0.1) +
  geom_line(aes(y = cmaq_pi, colour = "blue"), size = 2) +
  geom_ribbon(aes(ymin = cmaq_lpi, ymax=cmaq_upi), alpha=0.1) +
  geom_line(aes(y = fused_pi, colour = "green"), size = 2) +
  geom_ribbon(aes(ymin = fused_lpi, ymax=fused_upi), alpha=0.1) +
  geom_line(aes(y = cdc_pi, colour = "orange"), size = 2) +
  geom_ribbon(aes(ymin = cdc_lpi, ymax=cdc_upi), alpha=0.1) +
  geom_line(aes(y = emory_pi, colour = "purple"), size = 2) +
  geom_ribbon(aes(ymin = emory_lpi, ymax=emory_upi), alpha=0.1) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" Effect Modification by Rural %, Respiratory")) +
  labs(y = "% Increase in Respiratory Hospitalization", x = "Population Rural (per 1000 people)") + 
  scale_colour_discrete(name="Data Source", breaks=c("red", "blue", "green", "orange", "purple"),
                        labels=c("AQS", "CMAQ", "Fused", "CDC", "Emory")) +
  # geom_rug() +
  theme(text = element_text(size=15))


### mak test

ci2 <- read_csv("confidence_intervals_rural.csv")

ggplot(ci2, aes(x=percent_rural)) +
  geom_line(aes(y = aqs_pi, colour = "red"), size = 1.5) + 
  geom_ribbon(aes(ymin = aqs_lpi, ymax=aqs_upi), alpha=0.05, fill = "red") +
  geom_line(aes(y = cmaq_pi, colour = "blue"), size = 1.5) +
  geom_ribbon(aes(ymin = cmaq_lpi, ymax=cmaq_upi), alpha=0.05, fill = "blue") +
  geom_line(aes(y = fused_pi, colour = "green"), size = 1.5) +
  geom_ribbon(aes(ymin = fused_lpi, ymax=fused_upi), alpha=0.05, fill="green") +
  geom_line(aes(y = cdc_pi, colour = "orange"), size = 1.5) +
  geom_ribbon(aes(ymin = cdc_lpi, ymax=cdc_upi), alpha=0.05, fill = "orange") +
  geom_line(aes(y = emory_pi, colour = "purple"), size = 1.5) +
  geom_ribbon(aes(ymin = emory_lpi, ymax=emory_upi), alpha=0.05, fill = "purple") +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" Effect Modification by Rural %")) +
  labs(y = "% Increase in CVD Hospitalization", x = "% Rural") + 
  scale_colour_discrete(name="Data Source", breaks=c("red", "blue", "green", "orange", "purple"),
                        labels=c("AQS", "CMAQ", "Fused**", "CDC**", "Emory**")) +
  # geom_rug() +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")

### end mak test

ggplot(ci2, aes(x=percent_rural)) +
  geom_line(aes(y = aqs_pi, colour = "aqs")) + 
  geom_line(aes(y = cmaq_pi, colour = "cmaq")) +
  geom_line(aes(y = fused_pi, colour = "fused")) +
  geom_line(aes(y = cdc_pi, colour = "cdc")) +
  geom_line(aes(y = emory_pi, colour = "emory")) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" Effect Modification by Rural %")) +
  labs(y = "% Change in CVD Hospitalization", x = "% Rural") + 
  scale_colour_discrete(name="Data Source", breaks=c("aqs", "cmaq", "fused", "cdc", "emory"),
                        labels=c("AQS", "CMAQ", "Fused", "CDC", "Emory")) +
  theme(text = element_text(size=15))

## To do this one, must convert data from wide to long format
## Can do this if necessary
ggplot(ci2, aes(x=percent_rural)) +
  geom_line(aes(y = aqs_pi), colour = "red") + 
  geom_line(aes(y = cmaq_pi), colour = "blue") +
  geom_line(aes(y = fused_pi), colour = "green") +
  geom_line(aes(y = cdc_pi), colour = "orange") +
  geom_line(aes(y = emory_pi), colour = "purple") +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" Effect Modification by Rural %")) +
  labs(y = "% Change in CVD Hospitalization", x = "% Rural") + 
  scale_colour_discrete(name="Data Source", breaks=c("red", "blue", "green", "orange", "purple"),
                        labels=c("AQS", "CMAQ", "Fused", "CDC", "Emory")) +
  theme(text = element_text(size=15))
