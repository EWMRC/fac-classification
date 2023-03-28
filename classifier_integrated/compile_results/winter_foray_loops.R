#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#ME-2018-08
#starts on 2018-12-23, ends on 2018-12-26
# foray loop steps: 8352992079, 8352992080
# foray loop points: 8352992080

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(8384255835, 8352992079, 8352992080), "Foray loop (winter)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(8352992079, 8352992080), "Foray loop (winter)", primary_point_state))

#PA-2018-01
#starts on 2018-12-18, ends on 2018-12-22
# foray loop steps: 8262529843, 8262529844, 8340308849
# foray loop points: 8262529844, 8340308849
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(8262529843, 8262529844, 8340308849), "Foray loop (winter)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(8262529844, 8340308849), "Foray loop (winter)", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
