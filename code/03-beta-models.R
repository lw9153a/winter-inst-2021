library(here)
library(tidyverse)
library(betareg)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_beta <- betareg(gwvoteimp ~ cp + gov_party + co2_emissions
                        + perc_renew + perc_ff + perc_black 
                        + perc_latino + perc_asian + margin_2016, 
                        data = climate)
summary(climate_beta)

selcri<-function(lmout)
{
  n <- length(lmout$fit)
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  cbind(adj.rsq, aic, bic)
}

selcri(climate_beta)

fitted_beta <- climate_beta$fitted.values

rmse(climate$gwvoteimp, fitted_beta)

ggplot(data = climate)+
  geom_point(aes(x = fitted_beta, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

vif(climate_beta)
mean(vif(climate_beta))
