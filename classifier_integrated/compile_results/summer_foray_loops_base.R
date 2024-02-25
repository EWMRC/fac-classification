#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation_base.rds"))

#VA-2018-03
#starts on 2018-06-20, ends on 2018-07-30
# foray loop steps: 6930492970, 6930492971, 6930492974, 6930492975, 6930492976, 6930493284, 6930493285, 6930493286
# foray loop points: 6930492971, 6930492974, 6930492975, 6930492976, 6930493284, 6930493285, 6930493286

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(6930492970, 6930492971, 6930492974, 6930492975, 6930492976, 6930493284, 6930493285, 6930493286), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(6930492971, 6930492974, 6930492975, 6930492976, 6930493284, 6930493285, 6930493286), "Foray loop", primary_point_state))


#MD-2020-15
#starts on 2020-07-26, ends on 2020-08-01
# foray loop steps: 15467902387, 15638261200
# foray loop points: 15638261200

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(15467902387, 15638261200), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id == 15638261200, "Foray loop", primary_point_state))

#GA-2021-18
#starts on 2021-07-25, ends on 2021-09-03
# foray loop steps: 19738660818, 19862859308, 19862859310, 19794563577, 19794563581, 19862859319
# foray loop points: 19862859308, 19862859310, 19794563577, 19794563581, 19862859319

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(19738660818, 19862859308, 19862859310, 19794563577, 19794563581, 19862859319), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(19862859308, 19862859310, 19794563577, 19794563581, 19862859319), "Foray loop", primary_point_state))

#VA-2018-02
#starts on 2018-06-26, ends on 2018-07-05
# foray loop steps: 6930493632, 6930493633
# foray loop points: 6930493633

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(6930493632, 6930493633), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id == 6930493633, "Foray loop", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation_base.rds"))
