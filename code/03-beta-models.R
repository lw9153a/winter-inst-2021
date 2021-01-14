library(here)
library(tidyverse)
library(betareg)

climate <- read_csv(here("data", "clean_climate.csv"))

climate_beta <- betareg(gwvoteimp ~ perc_renew + perc_black 
                        + perc_latino + perc_asian + margin_2016, 
                        data = climate)
summary(climate_beta)

fitted_beta <- climate_beta$fitted.values

rmse(climate$gwvoteimp, fitted_beta)

ggplot(data = climate)+
  geom_point(aes(x = fitted_beta, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

vif(climate_beta)
mean(vif(climate_beta))
