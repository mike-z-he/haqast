## Marianthi's Code for Spatial Effect Modification - April 2019
## To be used with the effect_modification script

## Run Spatial Effect Modification models, and edit coef/vcov below

rurcoef <- model_emory$coefficients
rurvcov <- vcov(model_emory)

rur.res <- matrix(NA, 101, 5)

for (i in 0:100){
  
  rur.res[(i+1),1] <- rurcoef[2] + i*1000*rurcoef[length(rurcoef)]
  rur.res[(i+1),2] <- sqrt(rurvcov[2,2] + (i*1000)^2*rurvcov[length(rurcoef),length(rurcoef)] + 2*1000*i*rurvcov[2,length(rurcoef)])
  
  
}

rur.res[,3] <- 100*(exp(10*rur.res[,1])-1)
rur.res[,4] <- 100*(exp(10*(rur.res[,1] - 1.96*rur.res[,2]))-1)
rur.res[,5] <- 100*(exp(10*(rur.res[,1] + 1.96*rur.res[,2]))-1)


plot(0:100, rur.res[,3], type = 'l', ylim=c(-0.5, 1.2))
lines(0:100, rur.res[,4], lty = 3)
lines(0:100, rur.res[,5], lty = 3)
abline(h=0, col="gray")


hist(dta$poppct_rural)

## Export
dta <- as.data.frame(rur.res)
write.csv(dta, file = "emory_rural.csv")



dta$poppct_rural.q <- cut(dta$poppct_rural, breaks = quantile(dta$poppct_rural, c(0,0.25,0.50,0.75,1)),
                                                              include.lowest = TRUE)

dta$poppct_rural.q2 <- ifelse(dta$poppct_rural.q == "(64.2,100]", 3, dta$poppct_rural.q)

sum(table(dta$poppct_rural.q2)) == dim(dta)[1]
model_emory.q <- glm(cvd_in ~ pm_emory + pm_emory:as.factor(poppct_rural.q2) + as.factor(county) + 
                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta, family = quasipoisson())
summary(model_emory.q)
summary(model_emory.q)$coefficients[2]

model_emory.q21 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                       ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                       as.factor(dow), data = dta[which(dta$poppct_rural.q2==1),], family = quasipoisson())

model_emory.q22 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                         as.factor(dow), data = dta[which(dta$poppct_rural.q2==2),], family = quasipoisson())
model_emory.q23 <- glm(cvd_in ~ pm_emory  + as.factor(county) + 
                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
                         as.factor(dow), data = dta[which(dta$poppct_rural.q2==3),], family = quasipoisson())

summary(model_emory.q21)$coefficients[2,]
summary(model_emory.q22)$coefficients[2,]
summary(model_emory.q23)$coefficients[2,]



# model_emory.q2 <- gam(cvd_in ~ s(pm_emory, by = poppct_rural.q) + as.factor(county) + 
#                        ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
#                        as.factor(dow), data = dta, family = quasipoisson())
# 
# 
# library(mgcv)
# model_emory.q3 <- gam(cvd_in ~ te(pm_emory, poppct_rural) + as.factor(county) + 
#                         ns(as.numeric(date), df = 11*4) + ns(temp, df = 3) + ns(rh, df = 3) + 
#                         as.factor(dow), data = dta, family = quasipoisson())
# 
# 
