# Load packages
library(here)
library(tidyverse)

# Load in data
election <- read_csv(here("data", "raw_2016_election.csv"))
state <- read_csv(here("data", "raw_state.csv"))
yale <- read_csv(here("data", "raw_yale.csv"))

# Clean yale data set
yale%>%
  filter(GeoType == "State")%>%
  select(GeoName, gwvoteimp, human, humanOppose) ->
  yale

# Clean state data set
state%>%
  mutate(cp = ifelse(cp_2019 > 0, 1, 0))%>%
  mutate(perc_white = round(perc_white * 100, digits = 0))%>%
  mutate(perc_black = round(perc_black * 100, digits = 0))%>%
  mutate(perc_latino = round(perc_latino * 100, digits = 0))%>%
  mutate(perc_asian = round(perc_asian * 100, digits = 0))%>%
  mutate(perc_renew = round(perc_renew * 100, digits = 0))%>%
  mutate(perc_ff = round(perc_ff * 100, digits = 0))->
  state

# Merge state and yale data set
climate <- inner_join(state, yale, by = c("state" = "GeoName"))

# Merge climate and election data set
climate <- inner_join(climate, election, by = c("state" = "state"))

# Transform gwvoteimp into a proportion
climate%>%
  mutate(gwvoteimp = gwvoteimp/100) ->
  climate

# Save clean merged data set as a new csv in repo
write_csv(x = climate, path = here("data", "clean_climate.csv"))
