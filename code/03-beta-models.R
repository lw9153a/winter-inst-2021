# Load packages
library(here)
library(tidyverse)
library(betareg)
library(car)

#Load in data
climate <- read_csv(here("data", "clean_climate.csv"))

# Beta model with the same predictors as the final linear model
climate_beta <- betareg(gwvoteimp ~ perc_renew + perc_black + pop
                        + perc_latino + perc_asian + margin_2016, 
                        data = climate)
summary(climate_beta)

# Pulling out the fitted values for the beta model
fitted_beta <- climate_beta$fitted.values

# Calculating the rmse for the beta model
rmse(climate$gwvoteimp, fitted_beta)

# Plotting the fitted versus the actual values for the beta model
ggplot(data = climate)+
  geom_point(aes(x = fitted_beta, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Checking for issues for multicollinearity, there don't appear to be any
vif(climate_beta)
mean(vif(climate_beta))

## The Linear model seemed to be just as good at predicting and had relatively the same 
## r-squared value as the beta model, so we decided to stick with the linear model
