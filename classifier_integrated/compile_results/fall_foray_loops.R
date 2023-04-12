#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#RI-2020-31
#starts on 2020-08-29, ends on 2020-12-01
# foray loop steps: 17060579721, 18145087133
# foray loop points: 18145087133
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(17060579721, 18145087133), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(18145087133), "Foray loop", primary_point_state))

#NY-2018-03
#starts on 2018-10-06, ends on 2018-10-16
# foray loop steps: 7570774634, 7662851838, 7662851839
# foray loop points: 7662851838, 7662851839
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(7570774634, 7662851838, 7662851839), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(7662851838, 7662851839), "Foray loop", primary_point_state))

#VA-2021-92
#starts on 2021-9-20, ends on 2021-10-10
# foray loop steps: 20247710616, 20247710619, 20334121982, 20334121986
# foray loop points: 20247710619, 20334121982, 20334121986
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(20247710616, 20247710619, 20334121982, 20334121986), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(20247710619, 20334121982, 20334121986), "Foray loop", primary_point_state))


saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
