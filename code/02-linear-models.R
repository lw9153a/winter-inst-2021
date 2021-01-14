library(here)
library(tidyverse)
library(Metrics)
library(car)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_reg <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                  + perc_renew + perc_ff + margin_2016, data = climate)
summary(climate_reg)

climate_reg2 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_white 
                   + perc_black + perc_latino + perc_asian + margin_2016, 
                   data = climate)
summary(climate_reg2)

climate_reg3 <- lm(gwvoteimp ~ human + cp + gov_party 
                   + co2_emissions + perc_renew + perc_ff 
                   + pop + perc_white + perc_black + perc_latino 
                   + perc_asian + margin_2016, data = climate)
summary(climate_reg3)


selcri<-function(lmout)
{
  n <- length(lmout$fit)
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(adj.rsq, aic, bic, press)
}

selcri(climate_reg)
selcri(climate_reg2)
selcri(climate_reg3)

fitted_reg <- climate_reg$fitted.values
fitted_reg2 <- climate_reg2$fitted.values
fitted_reg3 <- climate_reg3$fitted.values

rmse(climate$gwvoteimp, fitted_reg)
rmse(climate$gwvoteimp, fitted_reg2)
rmse(climate$gwvoteimp, fitted_reg3)

ggplot(data = climate)+
  geom_point(aes(x = fitted_reg, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

ggplot(data = climate)+
  geom_point(aes(x = fitted_reg2, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

ggplot(data = climate)+
  geom_point(aes(x = fitted_reg3, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

climate_reg4 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_black 
                   + perc_latino + perc_asian + margin_2016, 
                   data = climate)
summary(climate_reg4)

climate_reg5 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + perc_black 
                   + perc_latino + perc_asian + margin_2016, 
                   data = climate)

summary(climate_reg5)

selcri(climate_reg2)
selcri(climate_reg4)
selcri(climate_reg5)
