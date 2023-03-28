#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#RI-2019-29 (1st mvmt)
#starts on 2019-12-02, ends on 2019-12-03
# foray loop steps: 13579628721
# foray loop points: none
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 13579628721, "Dispersal (winter)", primary_step_state))

#RI-2019-29 (2nd mvmt)
#starts on 2020-02-09, ends on 2020-02-11
# foray loop steps: 13877262855, 13877262856
# foray loop points: 13877262856
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(13877262855, 13877262856), "Dispersal (winter)", primary_step_state),
         primary_point_state = if_else(event_id == 13877262856, "Dispersal (winter)", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
