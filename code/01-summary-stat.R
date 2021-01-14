# Load packages
library(here)
library(tidyverse)

# Load in data
climate <- read_csv(here("data", "clean_climate.csv"))

## Univariate Analysis

# Examining GDP variable
ggplot(climate)+
  geom_histogram(aes(x = gdp))+
  theme_bw()

summary(climate$gdp)

# Examining CO2 emissions variable
ggplot(climate)+
  geom_histogram(aes(x = co2_emissions))+
  theme_bw()

summary(climate$co2_emissions)

# Examining % renewable variable
ggplot(climate)+
  geom_histogram(aes(x = perc_renew), bins = 15)+
  theme_bw()

summary(climate$perc_renew)

# Examining % fossil fuel variable
ggplot(climate)+
  geom_histogram(aes(x = perc_ff))+
  theme_bw()

summary(climate$perc_ff)


## Bivariate Analysis

# Filtering out variables not used in our models
climate%>%
  select(-c(state, state_abb, gov_party, cp_2019, total_energy, renew, ff, humanOppose, nuclear)) ->
  climate_num

# Correlation matrix and plot
cor(climate_num)
pairs(climate_num[,1:13])

# Carbon price v. gwvoteimp
ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = gwvoteimp, fill = as.factor(cp)))+
  theme_bw()

# Carbon price v. % fossil fuel
ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = perc_ff, fill = as.factor(cp)))+
  theme_bw()

# Carbon price v. % renewable 
ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = perc_renew, fill = as.factor(cp)))+
  theme_bw()

# Carbon price vs. % of state who thinks climate change is caused by humans
ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = human, fill = as.factor(cp)))+
  theme_bw()

# % of state who thinks climate change is caused by humans vs. gwvoteimp
ggplot(data = climate) +
  geom_point(aes(x = human, y = gwvoteimp))+
  theme_bw()



