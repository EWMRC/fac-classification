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