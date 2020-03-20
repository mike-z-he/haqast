#### Year-Stratified Models in HAQAST
#### Mike He
#### July 25, 2019

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


## Break data down into 2-3 year chunks; breaks avoid 1 year data since CDC missing 2002 and 2012
dta_02_04 <- dta[which((dta$year==2002) | (dta$year==2003) | (dta$year==2004)),]
dta_05_06 <- dta[which((dta$year==2005) | (dta$year==2006)),]
dta_07_09 <- dta[which((dta$year==2007) | (dta$year==2008) | (dta$year==2009)),]
dta_10_12 <- dta[which((dta$year==2010) | (dta$year==2011) | (dta$year==2012)),]



####AQS####
aqs_02_04 <- glm(cvd_in ~ pm_aqs + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_02_04, family = quasipoisson())
summary(aqs_02_04)
100*(exp(aqs_02_04$coefficients[2]*10)-1)
100*(exp((summary(aqs_02_04)$coef[2,1] - 1.96* summary(aqs_02_04)$coef[2,2])*10)-1)
100*(exp((summary(aqs_02_04)$coef[2,1] + 1.96* summary(aqs_02_04)$coef[2,2])*10)-1)

aqs_05_06 <- glm(cvd_in ~ pm_aqs + as.factor(county) + 
                   ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_05_06, family = quasipoisson())
summary(aqs_05_06)
100*(exp(aqs_05_06$coefficients[2]*10)-1)
100*(exp((summary(aqs_05_06)$coef[2,1] - 1.96* summary(aqs_05_06)$coef[2,2])*10)-1)
100*(exp((summary(aqs_05_06)$coef[2,1] + 1.96* summary(aqs_05_06)$coef[2,2])*10)-1)

aqs_07_09 <- glm(cvd_in ~ pm_aqs + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_07_09, family = quasipoisson())
summary(aqs_07_09)
100*(exp(aqs_07_09$coefficients[2]*10)-1)
100*(exp((summary(aqs_07_09)$coef[2,1] - 1.96* summary(aqs_07_09)$coef[2,2])*10)-1)
100*(exp((summary(aqs_07_09)$coef[2,1] + 1.96* summary(aqs_07_09)$coef[2,2])*10)-1)

aqs_10_12 <- glm(cvd_in ~ pm_aqs + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_10_12, family = quasipoisson())
summary(aqs_10_12)
100*(exp(aqs_10_12$coefficients[2]*10)-1)
100*(exp((summary(aqs_10_12)$coef[2,1] - 1.96* summary(aqs_10_12)$coef[2,2])*10)-1)
100*(exp((summary(aqs_10_12)$coef[2,1] + 1.96* summary(aqs_10_12)$coef[2,2])*10)-1)



####CMAQ####
cmaq_02_04 <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_02_04, family = quasipoisson())
summary(cmaq_02_04)
100*(exp(cmaq_02_04$coefficients[2]*10)-1)
100*(exp((summary(cmaq_02_04)$coef[2,1] - 1.96* summary(cmaq_02_04)$coef[2,2])*10)-1)
100*(exp((summary(cmaq_02_04)$coef[2,1] + 1.96* summary(cmaq_02_04)$coef[2,2])*10)-1)

cmaq_05_06 <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + 
                   ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_05_06, family = quasipoisson())
summary(cmaq_05_06)
100*(exp(cmaq_05_06$coefficients[2]*10)-1)
100*(exp((summary(cmaq_05_06)$coef[2,1] - 1.96* summary(cmaq_05_06)$coef[2,2])*10)-1)
100*(exp((summary(cmaq_05_06)$coef[2,1] + 1.96* summary(cmaq_05_06)$coef[2,2])*10)-1)

cmaq_07_09 <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_07_09, family = quasipoisson())
summary(cmaq_07_09)
100*(exp(cmaq_07_09$coefficients[2]*10)-1)
100*(exp((summary(cmaq_07_09)$coef[2,1] - 1.96* summary(cmaq_07_09)$coef[2,2])*10)-1)
100*(exp((summary(cmaq_07_09)$coef[2,1] + 1.96* summary(cmaq_07_09)$coef[2,2])*10)-1)

cmaq_10_12 <- glm(cvd_in ~ pm_cmaq_new + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_10_12, family = quasipoisson())
summary(cmaq_10_12)
100*(exp(cmaq_10_12$coefficients[2]*10)-1)
100*(exp((summary(cmaq_10_12)$coef[2,1] - 1.96* summary(cmaq_10_12)$coef[2,2])*10)-1)
100*(exp((summary(cmaq_10_12)$coef[2,1] + 1.96* summary(cmaq_10_12)$coef[2,2])*10)-1)



####Fused####
fused_02_04 <- glm(cvd_in ~ pm_fused + as.factor(county) + 
                    ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                    as.factor(dow), data = dta_02_04, family = quasipoisson())
summary(fused_02_04)
100*(exp(fused_02_04$coefficients[2]*10)-1)
100*(exp((summary(fused_02_04)$coef[2,1] - 1.96* summary(fused_02_04)$coef[2,2])*10)-1)
100*(exp((summary(fused_02_04)$coef[2,1] + 1.96* summary(fused_02_04)$coef[2,2])*10)-1)

fused_05_06 <- glm(cvd_in ~ pm_fused + as.factor(county) + 
                    ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                    as.factor(dow), data = dta_05_06, family = quasipoisson())
summary(fused_05_06)
100*(exp(fused_05_06$coefficients[2]*10)-1)
100*(exp((summary(fused_05_06)$coef[2,1] - 1.96* summary(fused_05_06)$coef[2,2])*10)-1)
100*(exp((summary(fused_05_06)$coef[2,1] + 1.96* summary(fused_05_06)$coef[2,2])*10)-1)

fused_07_09 <- glm(cvd_in ~ pm_fused + as.factor(county) + 
                    ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                    as.factor(dow), data = dta_07_09, family = quasipoisson())
summary(fused_07_09)
100*(exp(fused_07_09$coefficients[2]*10)-1)
100*(exp((summary(fused_07_09)$coef[2,1] - 1.96* summary(fused_07_09)$coef[2,2])*10)-1)
100*(exp((summary(fused_07_09)$coef[2,1] + 1.96* summary(fused_07_09)$coef[2,2])*10)-1)

fused_10_12 <- glm(cvd_in ~ pm_fused + as.factor(county) + 
                    ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                    as.factor(dow), data = dta_10_12, family = quasipoisson())
summary(fused_10_12)
100*(exp(fused_10_12$coefficients[2]*10)-1)
100*(exp((summary(fused_10_12)$coef[2,1] - 1.96* summary(fused_10_12)$coef[2,2])*10)-1)
100*(exp((summary(fused_10_12)$coef[2,1] + 1.96* summary(fused_10_12)$coef[2,2])*10)-1)



####CDC####
cdc_02_04 <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + 
                     ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta_02_04, family = quasipoisson())
summary(cdc_02_04)
100*(exp(cdc_02_04$coefficients[2]*10)-1)
100*(exp((summary(cdc_02_04)$coef[2,1] - 1.96* summary(cdc_02_04)$coef[2,2])*10)-1)
100*(exp((summary(cdc_02_04)$coef[2,1] + 1.96* summary(cdc_02_04)$coef[2,2])*10)-1)

cdc_05_06 <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + 
                     ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta_05_06, family = quasipoisson())
summary(cdc_05_06)
100*(exp(cdc_05_06$coefficients[2]*10)-1)
100*(exp((summary(cdc_05_06)$coef[2,1] - 1.96* summary(cdc_05_06)$coef[2,2])*10)-1)
100*(exp((summary(cdc_05_06)$coef[2,1] + 1.96* summary(cdc_05_06)$coef[2,2])*10)-1)

cdc_07_09 <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + 
                     ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta_07_09, family = quasipoisson())
summary(cdc_07_09)
100*(exp(cdc_07_09$coefficients[2]*10)-1)
100*(exp((summary(cdc_07_09)$coef[2,1] - 1.96* summary(cdc_07_09)$coef[2,2])*10)-1)
100*(exp((summary(cdc_07_09)$coef[2,1] + 1.96* summary(cdc_07_09)$coef[2,2])*10)-1)

cdc_10_12 <- glm(cvd_in ~ pm_cdc_new + as.factor(county) + 
                     ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                     as.factor(dow), data = dta_10_12, family = quasipoisson())
summary(cdc_10_12)
100*(exp(cdc_10_12$coefficients[2]*10)-1)
100*(exp((summary(cdc_10_12)$coef[2,1] - 1.96* summary(cdc_10_12)$coef[2,2])*10)-1)
100*(exp((summary(cdc_10_12)$coef[2,1] + 1.96* summary(cdc_10_12)$coef[2,2])*10)-1)



####Emory####
emory_02_04 <- glm(cvd_in ~ pm_emory + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_02_04, family = quasipoisson())
summary(emory_02_04)
100*(exp(emory_02_04$coefficients[2]*10)-1)
100*(exp((summary(emory_02_04)$coef[2,1] - 1.96* summary(emory_02_04)$coef[2,2])*10)-1)
100*(exp((summary(emory_02_04)$coef[2,1] + 1.96* summary(emory_02_04)$coef[2,2])*10)-1)

emory_05_06 <- glm(cvd_in ~ pm_emory + as.factor(county) + 
                   ns(as.numeric(date), df = 2*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_05_06, family = quasipoisson())
summary(emory_05_06)
100*(exp(emory_05_06$coefficients[2]*10)-1)
100*(exp((summary(emory_05_06)$coef[2,1] - 1.96* summary(emory_05_06)$coef[2,2])*10)-1)
100*(exp((summary(emory_05_06)$coef[2,1] + 1.96* summary(emory_05_06)$coef[2,2])*10)-1)

emory_07_09 <- glm(cvd_in ~ pm_emory + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_07_09, family = quasipoisson())
summary(emory_07_09)
100*(exp(emory_07_09$coefficients[2]*10)-1)
100*(exp((summary(emory_07_09)$coef[2,1] - 1.96* summary(emory_07_09)$coef[2,2])*10)-1)
100*(exp((summary(emory_07_09)$coef[2,1] + 1.96* summary(emory_07_09)$coef[2,2])*10)-1)

emory_10_12 <- glm(cvd_in ~ pm_emory + as.factor(county) + 
                   ns(as.numeric(date), df = 3*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                   as.factor(dow), data = dta_10_12, family = quasipoisson())
summary(emory_10_12)
100*(exp(emory_10_12$coefficients[2]*10)-1)
100*(exp((summary(emory_10_12)$coef[2,1] - 1.96* summary(emory_10_12)$coef[2,2])*10)-1)
100*(exp((summary(emory_10_12)$coef[2,1] + 1.96* summary(emory_10_12)$coef[2,2])*10)-1)



####Plot####
ci <- read_csv("confidence_intervals_toxicity.csv")
ci$model <- factor(ci$model, levels = c("AQS", "CMAQ", "Fused", "CDC", "Emory"))
ci$type <- factor(ci$year, levels = c("02_04", "05_06", "07_09", "10_12"))

ggplot(ci, aes(x=model, y=pi, color = ci$type)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme(legend.position = "bottom") + ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization by Year")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_discrete(name="Analysis Type", breaks=c("02_04", "05_06", "07_09", "10_12"),
                       labels=c("2002-04", "2005-06", "2007-09", "2010-12")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")
