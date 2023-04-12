library(tidyverse)
library(lubridate)

amwo_data_reconstructed <- readRDS(file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))

nest_attempts <- read_csv(here::here("nests", "combinednests25all337nests.csv"), 
                          col_types = cols(first_date = col_date(format = "%Y-%m-%d"), 
                                           last_date = col_date(format = "%Y-%m-%d"), 
                                           attempt_start = col_date(format = "%Y-%m-%d"), 
                                           attempt_end = col_date(format = "%Y-%m-%d"))) %>% 
  transmute(animal_name = burst,
            attempt_start = attempt_start,
            attempt_end = attempt_end)

amwo_data_reconstructed_w_duplicates <- amwo_data_reconstructed %>% 
  left_join(nest_attempts)

amwo_data_reconstructed_w_duplicates$nesting_attempt <- amwo_data_reconstructed_w_duplicates %>% 
  dplyr::select(time, attempt_start, attempt_end) %>% 
  pmap(.f = function(time, attempt_start, attempt_end){
    nesting_state <- "Not nesting"
    if(!is.na(attempt_start)){ #if there's a nesting attempt for this row
      if(as_date(time) >= attempt_start & as_date(time) <= attempt_end){ #if a location falls within the time period for this nesting attempt
        nesting_state <- "Nesting"
      }
    }
    return(nesting_state)
  }) %>% 
  unlist() %>%
  factor(levels = c("Nesting", "Not nesting"), ordered = TRUE)

amwo_data_reconstructed_w_nests <- amwo_data_reconstructed_w_duplicates %>% 
  group_by(event_id) %>%
  arrange(nesting_attempt, .by_group = TRUE) %>%
  group_modify(.f = function(x, ...){head(x, n = 1)}) %>%
  dplyr::select(-attempt_start, -attempt_end) %>% 
  ungroup()

saveRDS(amwo_data_reconstructed_w_nests, file = here::here("classifier_integrated", "fac_primary_state_delineation.rds"))