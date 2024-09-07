#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#VA-2019-23
#No dispersal points, just a step
#starts on 7/10/2019, ends on 7/14/2019
#11644874412
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 11644874412, "Dispersal", primary_step_state))

#VA-2020-52
#No dispersal points, just a step
#starts on 2020-06-19, ends on 2020-06-28
#15096830096
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 15096830096, "Dispersal", primary_step_state))

#WV-2024-47
#replacing spring mig with a dispersal
#32983659939
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 32983659939, "Dispersal", primary_step_state))

#WV-2024-55
#replacing spring mig with a dispersal
#33815123757
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 33815123757, "Dispersal", primary_step_state))


saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
