fall_short_mig <- fall_classified %>% 
  filter(step_state == 2) %>%
  filter(step < 30.2) %>% View()
  nrow()

fall_long_stationary <- fall_classified %>% 
  filter(step_state %in% c(1,3,4)) %>%
  filter(step > 30.2) %>% 
  nrow()
