#delineated manually based on criteria in state_definitions.txt
library(tidyverse)

amwo_data_reconstructed <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

#ME-2018-08
#starts on 2018-12-23, ends on 2018-12-26
# foray loop steps: 8352992079, 8352992080
# foray loop points: 8352992080

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(8384255835, 8352992079, 8352992080), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(8352992079, 8352992080), "Foray loop", primary_point_state))

#PA-2018-01
#starts on 2018-12-18, ends on 2018-12-22
# foray loop steps: 8262529843, 8262529844, 8340308849
# foray loop points: 8262529844, 8340308849
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(8262529843, 8262529844, 8340308849), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(8262529844, 8340308849), "Foray loop", primary_point_state))

#NY-2018-04 (removing foray loop as the bird doesn't settle)
#starts on 2018-11-21, ends on 2018-11-24
# foray loop steps: 8037640949, 8037640950, 8007051304
# foray loop points: 8037640950, 8007051304
# amwo_data_reconstructed <- amwo_data_reconstructed %>% 
#   mutate(primary_step_state = if_else(event_id %in% c(8037640949, 8037640950, 8007051304), "Foray loop (winter)", primary_step_state),
#          primary_point_state = if_else(event_id %in% c(8037640950, 8007051304), "Foray loop (winter)", primary_point_state))

#RI-2021-46
#starts on 2022-02-10, ends on 2022-02-16
# foray loop steps: 21279049764, 21279049767, 21279049774
# foray loop points: 21279049767, 21279049774
amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(21279049764, 21279049767, 21279049774), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(21279049767, 21279049774), "Foray loop", primary_point_state))



#NJ-2018-03
#starts on 2019-1-10, ends on 2019-1-23
# foray loop steps: 8807074660, 8807074661
# foray loop points: 8807074661

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(8807074660, 8807074661), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(8807074661), "Foray loop", primary_point_state))


#PA-2019-15
#starts on 2019-12-10, ends on 2019-12-16
# foray loop steps: 13598268172, 13598268173, 13607659369
# foray loop points: 13598268173, 13607659369

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(13598268172, 13598268173, 13607659369), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(13598268173, 13607659369), "Foray loop", primary_point_state))

#NY-2018-07
#starts on 2019-01-29, ends on 2019-02-13
# foray loop steps: 9064971965, 9126872530
# foray loop points: 9126872530

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(9064971965, 9126872530), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(9126872530), "Foray loop", primary_point_state))

#RI-2020-44
#starts on 2021-01-29, ends on 2021-02-12
# foray loop steps: 17786229916, 17786229926, 17786229933, 17786229963, 17786229979, 17786230000, 17833286376, 17833286393
# foray loop points: 17786229926, 17786229933, 17786229963, 17786229979, 17786230000, 17833286376, 17833286393

amwo_data_reconstructed <- amwo_data_reconstructed %>% 
  mutate(primary_step_state = if_else(event_id %in% c(17786229916, 17786229926, 17786229933, 17786229963, 17786229979, 17786230000, 17833286376, 17833286393), "Foray loop", primary_step_state),
         primary_point_state = if_else(event_id %in% c(17786229926, 17786229933, 17786229963, 17786229979, 17786230000, 17833286376, 17833286393), "Foray loop", primary_point_state))


saveRDS(amwo_data_reconstructed, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
