#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#FL-2021-01
#starts on 2021-05-19, ends on 2021-05-27
# foray loop steps: 18843090289, 18843090301
# foray loop points: 18843090301

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(18843090289, 18843090301), "Foray loop (spring)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(18843090301), "Foray loop (spring)", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
