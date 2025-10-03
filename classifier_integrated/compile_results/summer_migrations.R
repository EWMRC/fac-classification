#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#SC-2020-13
#No dispersal points, just a step
#starts on 2020-07-13, ends on 2020-08-22
# migratory steps: 15467902365, 15467902384, 15715084396, 15715084404, 15715084421
# migratory points: 15467902384, 15715084396, 15715084404, 15715084421

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(15467902330, 15467902365, 15467902384, 15715084396, 15715084404, 15715084421), "Migratory (summer)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(15467902365, 15467902384, 15715084396, 15715084404, 15715084421), "Migratory (summer)", primary_point_state))

#SC-2021-23
# starts on 2021-05-27, ends on 2021-07-08
# migratory steps: 18897319135
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id == 18897319135, "Migratory (summer)", primary_step_state))

#AL-2021-11
# starts on 2021-06-20, ends on 2021-07-25
# migratory steps: 19589114101, 19589114109
# migratory points: 19589114109
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(19589114101, 19589114109), "Migratory (summer)", primary_step_state),
         primary_point_state = if_else(event_id == 19589114109, "Migratory (summer)", primary_point_state))

#WV-2024-27
# starts on 2024-07-13, ends on 
# migratory steps: 34106450785, 34200333436, 34200333443
# migratory points: 34200333436, 34200333443
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(34106450785, 34200333436, 34200333443), "Migratory (summer)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(34200333436, 34200333443), "Migratory (summer)", primary_point_state))

#WV-2024-41
# starts on 2024-07-07, ends on 
# migratory steps: 
# migratory points: 
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(34016350099, 34106450750, 34106450775), "Migratory (summer)", primary_step_state),
         primary_point_state = if_else(event_id %in% c(34106450750, 34106450775), "Migratory (summer)", primary_point_state))

saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
