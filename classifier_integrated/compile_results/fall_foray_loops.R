#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#RI-2020-31
#starts on 2020-08-29, ends on 2020-12-01
# foray loop steps: 17060579721, 18145087133
# foray loop points: 18145087133

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(17060579721, 18145087133), "Foray loop (fall)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(18145087133), "Foray loop (fall)", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
