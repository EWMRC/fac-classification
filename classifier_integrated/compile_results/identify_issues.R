fall_classified <- readRDS(here::here("classifier_fall", "fall_all_4_state_model.rds"))
spring_male_classified <- readRDS(here::here("classifier_spring", "spring_male_5_state_model.rds"))
spring_female_classified <-readRDS(here::here("classifier_spring", "spring_female_4_state_model.rds"))


fall_short_mig <- fall_classified %>% 
  filter(step_state == 2) %>%
  filter(step < 30.2) %>% 
  nrow()

fall_long_stationary <- fall_classified %>% 
  filter(step_state %in% c(1,3,4)) %>%
  filter(step > 30.2) %>% 
  nrow()

spring_f_short_mig <- spring_female_classified %>% 
  filter(step_state == 2) %>%
  filter(step < 30.2) %>% 
  nrow()

spring_f_long_stationary <- spring_female_classified %>% 
  filter(step_state %in% c(1,3,4)) %>%
  filter(step > 30.2) %>% 
  nrow()

spring_m_short_mig <- spring_male_classified %>% 
  filter(step_state == 2) %>%
  filter(step < 30.2) %>% 
  nrow()

spring_m_long_stationary <- spring_male_classified %>% 
  filter(step_state %in% c(1,3,4)) %>%
  filter(step > 30.2) %>% 
  nrow()
