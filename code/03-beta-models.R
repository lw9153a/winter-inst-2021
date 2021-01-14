library(here)
library(tidyverse)
library(betareg)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_beta <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_white 
                   + perc_black + perc_latino + perc_asian, 
                   data = climate)
summary(climate_beta)

climate_beta2 <- lm(gwvoteimp ~ human + cp + gov_party 
                   + co2_emissions + perc_renew + perc_ff 
                   + pop + perc_white + perc_black + perc_latino 
                   + perc_asian, data = climate)
summary(climate_beta2)

selcri<-function(lmout)
{
  n <- length(lmout$fit)
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(adj.rsq, aic, bic, press)
}

selcri(climate_beta)
selcri(climate_beta2)

fitted_beta <- climate_beta$fitted.values
fitted_beta2 <- climate_beta2$fitted.values

rmse(climate$gwvoteimp, fitted_beta)
rmse(climate$gwvoteimp, fitted_beta2)

ggplot(data = climate)+
  geom_point(aes(x = fitted_beta, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

ggplot(data = climate)+
  geom_point(aes(x = fitted_beta2, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

