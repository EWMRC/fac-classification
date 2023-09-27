acc_combined %>% 
  group_by(season,model_type, test) %>% 
  summarise(min_err = min(percent_error), max_err = max(percent_error)) %>% 
  View()
