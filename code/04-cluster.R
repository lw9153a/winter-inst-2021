# Loading packages
library(here)
library(tidyverse)
library(ClusterR)

# Loading in data
climate <- read_csv(here("data", "clean_climate.csv"))

# Selecting the numeric variables we used in creating our models and
# making gdp and co2 emissions per capita
climate %>%
  select(state, cp_2019, gdp, co2_emissions, perc_renew, perc_ff, pop, perc_white,
         perc_black, perc_latino, perc_asian, human, margin_2016)%>%
  mutate(gdp_pc = (gdp*1000000)/pop)%>%
  mutate(co2_pc = (co2_emissions*1000000)/pop)%>%
  select(-c(co2_emissions, gdp, pop))->
  climate_num

# Converting the data frame into a matrix
climate_matrix <- as.matrix(climate_num[,-1])

# Scaling the data
climate_matrix <- scale(climate_matrix)

# Adding in the states as row names
rownames(climate_matrix) <- climate_num$state
climate_matrix

# Calculating the distance
climate_dist <- dist(climate_matrix, method = "euclidean")

# Plotting the cluster diagram
climate_hag <- hclust(climate_dist, method = "complete")
plot(climate_hag)
