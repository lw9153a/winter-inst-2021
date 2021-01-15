library(here)
library(tidyverse)
library(ClusterR)

climate <- read_csv(here("data", "clean_climate.csv"))

climate %>%
  select(state, cp_2019, gdp, co2_emissions, perc_renew, perc_ff, pop, perc_white,
         perc_black, perc_latino, perc_asian, human, margin_2016)%>%
  mutate(gdp_pc = (gdp*1000000)/pop)%>%
  mutate(co2_pc = (co2_emissions*1000000)/pop)%>%
  select(-c(co2_emissions, gdp, pop))->
  climate_num

climate_matrix <- as.matrix(climate_num[,-1])
climate_matrix <- scale(climate_matrix)
rownames(climate_matrix) <- climate_num$state
climate_matrix

climate_dist <- dist(climate_matrix, method = "euclidean")

climate_hag <- hclust(climate_dist, method = "complete")
plot(climate_hag)
