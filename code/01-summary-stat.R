library(here)
library(tidyverse)

climate <- read_csv(here("data", "climate_clean.csv"))

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

