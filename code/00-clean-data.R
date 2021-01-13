library(here)
library(tidyverse)

election <- read_csv(here("data", "2016_election.csv"))
state <- read_csv(here("data", "state_raw.csv"))
yale <- read_csv(here("data", "yale_data_raw.csv"))

yale%>%
  filter(GeoType == "State")%>%
  select(GeoName, gwvoteimp) ->
  yale

state%>%
  mutate(cp = ifelse(cp_2019 > 0, 1, 0)) ->
  state

climate <- inner_join(state, yale, by = c("state" = "GeoName"))

climate <- inner_join(climate, election, by = c("state" = "state"))

