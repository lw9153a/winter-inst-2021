# Load packages
library(here)
library(tidyverse)
library(Metrics)
library(car)

#Load in data
climate <- read_csv(here("data", "clean_climate.csv"))

## Started with 3 base models

# First model w/out demographics
climate_reg <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                  + perc_renew + perc_ff + margin_2016, data = climate)
summary(climate_reg)

# Second model w/ demographics
climate_reg2 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_white 
                   + perc_black + perc_latino + perc_asian + margin_2016, 
                   data = climate)
summary(climate_reg2)

# Third model w/ demographics and human
climate_reg3 <- lm(gwvoteimp ~ human + cp + gov_party 
                   + co2_emissions + perc_renew + perc_ff 
                   + pop + perc_white + perc_black + perc_latino 
                   + perc_asian + margin_2016, data = climate)
summary(climate_reg3)

# Function to calculate selection criteria (AIC, BIC, etc)
selcri<-function(lmout)
{
  n <- length(lmout$fit)
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  cbind(adj.rsq, aic, bic)
}

# Calculating the selection criteria for the 3 models
selcri(climate_reg)
selcri(climate_reg2)
selcri(climate_reg3)

# Pulling out the fitted values for each models to calculate the rmse
fitted_reg <- climate_reg$fitted.values
fitted_reg2 <- climate_reg2$fitted.values
fitted_reg3 <- climate_reg3$fitted.values

# Calculating the rmse for the 3 models
rmse(climate$gwvoteimp, fitted_reg)
rmse(climate$gwvoteimp, fitted_reg2)
rmse(climate$gwvoteimp, fitted_reg3)

# Model 1: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 2: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg2, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 3: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg3, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 1: Checking for issues with multicollinearity
vif(climate_reg)
mean(vif(climate_reg))

# Model 2: Checking for issues with multicollinearity
vif(climate_reg2)
mean(vif(climate_reg2))

# Model 3: Checking for issues with multicollinearity
vif(climate_reg3)
mean(vif(climate_reg3))

## Went on to try and address issues with multicollinearity

# Fourth model without percentage of white
climate_reg4 <- lm(gwvoteimp ~ cp + gov_party + co2_emissions
                   + perc_renew + perc_ff + pop + perc_black 
                   + perc_latino + perc_asian + margin_2016, 
                   data = climate)
summary(climate_reg4)

# Comparing the selection criteria for model 2 (the best of the original 3) and model 4
selcri(climate_reg2)
selcri(climate_reg4)

# Pulling out the fitted values for model 4
fitted_reg4 <- climate_reg4$fitted.values

# Comparing the rmse for model 2 and model 4
rmse(climate$gwvoteimp, fitted_reg2)
rmse(climate$gwvoteimp, fitted_reg4)

# Model 2: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg2, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 4: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg4, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 2: Checking for issues with multicollinearity
vif(climate_reg2)
mean(vif(climate_reg2))

# Model 4: Checking for issues with multicollinearity
vif(climate_reg4)
mean(vif(climate_reg4))

## We can see that most of the multicollinearity issues appear to be resolved
## We move on to improve the models prediction and selection critera (AIC and BIC)

# Fifth model: Remove carbon price and governor party
climate_reg5<- lm(gwvoteimp ~ perc_renew + perc_ff + perc_black + pop
                  + perc_latino + perc_asian + margin_2016, 
                  data = climate)
summary(climate_reg5)

# Sixth model: Remove carbon price and governor party
climate_reg6<- lm(gwvoteimp ~ perc_renew + perc_black + pop
                  + perc_latino + perc_asian + margin_2016, 
                  data = climate)
summary(climate_reg6)

# Comparing the selection criteria for model 4, model 5, and model 6
selcri(climate_reg4)
selcri(climate_reg5)
selcri(climate_reg6)

# Pulling out the fitted values for model 5 and model 6
fitted_reg5 <- climate_reg5$fitted.values
fitted_reg6 <- climate_reg6$fitted.values

# Comparing the rmse for model 4, model 5, and model 6
rmse(climate$gwvoteimp, fitted_reg4)
rmse(climate$gwvoteimp, fitted_reg5)
rmse(climate$gwvoteimp, fitted_reg6)

## We can see that model 5 isn't going to be as good as the other two in terms of selection criteria

# Model 4: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg4, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

# Model 6: Plotting the fitted values vs the actual values
ggplot(data = climate)+
  geom_point(aes(x = fitted_reg6, y = gwvoteimp))+
  geom_abline()+
  theme_bw()

## We ended up with model 6 due to the it dealt with the multicollinarity as well as having
## the best AIC, BIC, and RMSE