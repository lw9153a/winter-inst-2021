library(here)
library(tidyverse)
library(broom)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_reg <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                  + perc_renew + perc_ff, data = climate)
summary(climate_reg)

climate_reg2 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_white 
                   + perc_black + perc_latino + perc_asian, 
                   data = climate)
summary(climate_reg2)

climate_reg3 <- lm(gwvoteimp ~ human + cp + gov_party 
                   + co2_emissions + perc_renew + perc_ff 
                   + pop + perc_white + perc_black + perc_latino 
                   + perc_asian, data = climate)
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
