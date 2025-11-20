library(tidyverse)
library(readxl)

amwo_data <- readRDS(file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

nonmig_events <- read_excel(here::here("movement_stats", "nonmig_classifications.xlsx"), 
                            col_types = c("text", "date", "date", "text")) %>% 
  rename(animal_name = ID) %>% 
  mutate(non_migratory_at_some_point = 1,
         normal_before_date = if_else(is.na(normal_before_date), mdy("1/1/1970"), normal_before_date), # replace NAs with dummy dates
         normal_after_date = if_else(is.na(normal_after_date), mdy("1/1/2100"), normal_after_date),
         Notes = NULL)

amwo_data <- amwo_data %>% 
  left_join(nonmig_events) %>% 
  mutate(non_migratory_at_some_point = if_else(is.na(non_migratory_at_some_point), 0, 1))

amwo_data$primary_step_state <- amwo_data %>% 
  dplyr::select(time, non_migratory_at_some_point, normal_before_date, normal_after_date, primary_step_state) %>% 
  pmap_chr(.f = function(time, non_migratory_at_some_point, normal_before_date, normal_after_date, primary_step_state){
    if(non_migratory_at_some_point == 1){
      if(time >= normal_before_date & time < normal_after_date) {
        return("Non-migratory")
      } else {
        return(primary_step_state)
      }
    } else {
      return(primary_step_state)
    }
  })

amwo_data$primary_point_state <- amwo_data %>%
  dplyr::select(time, non_migratory_at_some_point, normal_before_date, normal_after_date, primary_point_state) %>% 
  pmap_chr(.f = function(time, non_migratory_at_some_point, normal_before_date, normal_after_date, primary_point_state){
    if(non_migratory_at_some_point == 1){
      if(time >= normal_before_date & time <= normal_after_date) {
        return("Non-migratory")
      } else {
        return(primary_point_state)
      }
    } else {
      return(primary_point_state)
    }
  })

amwo_data <- amwo_data %>% 
  dplyr::select(-non_migratory_at_some_point, -normal_before_date, -normal_after_date)

saveRDS(amwo_data, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
