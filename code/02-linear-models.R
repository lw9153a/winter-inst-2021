library(here)
library(tidyverse)
library(broom)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_reg <- lm(gwvoteimp ~ cp + gov_party + co2_emissions + perc_renew + perc_ff, data = climate)
summary(climate_reg)


selcri<-function(lmout)
{
  n <- length(lmout$fit)
  rsq <- summary(lmout)$r.sq
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(rsq, adj.rsq, aic, bic, press)
}
