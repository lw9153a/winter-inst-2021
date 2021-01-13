library(here)
library(tidyverse)

election <- read_csv(here("data", "raw_2016_election.csv"))
state <- read_csv(here("data", "raw_state.csv"))
yale <- read_csv(here("data", "raw_yale.csv"))

yale%>%
  filter(GeoType == "State")%>%
  select(GeoName, gwvoteimp, human, humanOppose) ->
  yale

state%>%
  mutate(cp = ifelse(cp_2019 > 0, 1, 0))%>%
  mutate(perc_white = perc_white * 100)%>%
  mutate(perc_black = perc_black * 100)%>%
  mutate(perc_latino = perc_latino * 100)%>%
  mutate(perc_asian = perc_asian * 100)%>%
  mutate(perc_renew = perc_renew * 100)%>%
  mutate(perc_ff = perc_ff * 100)->
  state

climate <- inner_join(state, yale, by = c("state" = "GeoName"))

climate <- inner_join(climate, election, by = c("state" = "state"))

write_csv(x = climate, path = here("data", "clean_climate.csv"))
