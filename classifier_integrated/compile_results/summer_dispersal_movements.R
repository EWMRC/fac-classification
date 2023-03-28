#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#VA-2019-23
#No dispersal points, just a step
#starts on (7/10/2019) , ends on 7/14/2019
#11644874412
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 11644874412, "Dispersal (summer)", primary_step_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
