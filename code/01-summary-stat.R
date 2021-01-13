library(here)
library(tidyverse)
library(PerformanceAnalytics)

climate <- read_csv(here("data", "clean_climate.csv"))

# Univariate Analysis

ggplot(climate)+
  geom_histogram(aes(x = gdp))+
  theme_bw()

summary(climate$gdp)

ggplot(climate)+
  geom_histogram(aes(x = co2_emissions))+
  theme_bw()

summary(climate$co2_emissions)

ggplot(climate)+
  geom_histogram(aes(x = perc_renew), bins = 15)+
  theme_bw()

summary(climate$perc_renew)

ggplot(climate)+
  geom_histogram(aes(x = perc_ff))+
  theme_bw()

summary(climate$perc_ff)


# Bivariate Analysis

ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = gwvoteimp, fill = as.factor(cp)))+
  theme_bw()

ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = perc_ff, fill = as.factor(cp)))+
  theme_bw()

ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = perc_renew, fill = as.factor(cp)))+
  theme_bw()

ggplot(data = climate) +
  geom_boxplot(aes(x = as.factor(cp), y = human, fill = as.factor(cp)))+
  theme_bw()

ggplot(data = climate) +
  geom_point(aes(x = human, y = gwvoteimp))+
  theme_bw()

climate%>%
  select(-c(state, state_abb, gov_party, cp_2019, total_energy, renew, ff)) ->
  climate_num

cor(climate_num)

chart.Correlation(climate_num[,1:10], histogram=TRUE, pch="+")

