#### Plots for Manuscript
#### Mike He
#### June 26, 2019

setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/haqast/data")
options(mc.cores=parallel::detectCores())

library(tidyverse)


##########Figure 1##########
ci <- read_csv("confidence_intervals_no_old_cdc.csv")
ci$model <- factor(ci$model, levels = c("AQS", "CMAQ", "Fused", "CDC", "Emory"))

ggplot(ci, aes(x=model, y=pi, color = ci$type)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  #ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_grey(name="Analysis Type", breaks=c("all", "aqs", "cca"),
                       labels=c("All Data", "AQS Only", "Complete Case")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")


##########Figure 2##########
ci <- read_csv("confidence_intervals_season.csv")
ci$model <- factor(ci$model, levels = c("AQS", "CMAQ", "Fused", "CDC", "Emory"))
ci$type <- factor(ci$type, levels = c("all", "spring", "summer", "autumn", "winter"))

ggplot(ci, aes(x=model, y=pi, color = ci$type)) +
  geom_errorbar(aes(ymin=lpi, ymax=upi, width = 0.5), position = position_dodge2(), size=1.2) +
  geom_point(position = position_dodge2(width = 0.5), size=2.5) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  #ggtitle(expression("PM"[2.5]*" and Cardiovascular Hospitalization by Season")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = expression("PM"[2.5]*" Data Source")) + 
  scale_color_grey(name="Analysis Type", breaks=c("all", "spring", "summer", "autumn", "winter"),
                       labels=c("All Data", "Spring", "Summer", "Autumn", "Winter")) +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")



##########Figure 3##########
ci2 <- read_csv("confidence_intervals_rural.csv")

ggplot(ci2, aes(x=percent_rural)) +
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
  theme_bw() +
  theme(legend.position = "bottom") + 
  #ggtitle(expression("PM"[2.5]*" Effect Modification by Rural %, Cardiovascular")) +
  labs(y = expression("Percent Increase (Per 10 μg/m"^3*")"), x = "Population Rural (×1000 people)") + 
  scale_colour_grey(name="Data Source", breaks=c("red", "blue", "green", "orange", "purple"),
                        labels=c("AQS", "CMAQ", "Fused", "CDC", "Emory")) +
  # geom_rug() +
  theme(text = element_text(size=15)) +
  geom_hline(yintercept=0, linetype="dashed")
