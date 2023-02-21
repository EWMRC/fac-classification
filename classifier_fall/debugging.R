library(tidyverse)
library(lubridate)
library(move)
library(momentuHMM)
library(leaflet)
library(RColorBrewer)
library(sf)
library(furrr)
library(here)
library(crawl)

load(file=here("classifier_fall", "fall_all_data.RData"))

id_list <- unique(amwo_hmm$ID) %>% as.character()

stateNames <- c("pre","migration", "stopover", "post")
dist <- list(step="gamma", angle="wrpcauchy", y = "norm", julian_day = "norm", dist_start = "bern", step_500 = "bern", breeding_abundance = "bern", log_mean_dist_7 = "norm", residence_time = "norm") #,

fixPar <- list(beta=matrix(c(NA, -1000, -1000, # pre-migration to transitions, can't go pre- to post
                             -1000, NA, NA,  #once entered migration can't go back to pre
                             -1000, NA, -1000, #Once entered stopover, can only go back to migration
                             -1000, -1000, -1000), nrow = 1, byrow = TRUE), #once entered post-migration, can't leave
               step_500 = c(NA, 0.999999999, NA, NA)) 

Par0_m1 <- list(step=c(0.260422621, 1.222709e+02, 0.43958050, 0.305660933, #mean in km
                       0.400384082, 1.379686e+02, 0.70153050, 0.499270487), # SD in km
                       #0.003361887, 3.196105e-67, 0.00308734, 0.001950549), #zeromass ie how many zeroes expected in the distribution
                angle=c(9.972823e-05, -0.003611636, 0.06917188, -0.001081856, #mean
                        9.331280e-01, 0.927557223, 0.16633055, 0.779827256), #concentration
                y = c(42.903653, 38.560696, 37.939696, 33.785863,
                      3.072624, 4.316646, 3.494462, 2.197339),
                julian_day = c(231.74162, 265.07195, 277.61999, 313.77507, #priors pulled from the model w/o julian day
                               25.46262, 26.45834, 26.12413, 24.97701),
                log_mean_dist_7 = c(4.062360, 9.989322, 5.012713, 3.652682, #mean
                                    1.603215, 2.796832, 2.738608, 1.571083),
                residence_time = c(56.20244, 8.167779, 26.30733, 71.02887, #mean
                                   27.94377, 18.146496, 17.45733, 20.27082),
                dist_start = c(0.01667985, 0.9264421, 0.9762556, 0.999999), #prob
                step_500 = c(0.002585776, 0.99999, 7.754479e-09, 0.003630367), #prob
                breeding_abundance = c(0.002562285, 0.2628888, 0.3822839, 0.6908077)) #sd

fit_and_predict <- function(.x, ks){
  set.seed(8)
  m1 <- fitHMM(data=.x, nbStates=4, dist=dist, Par0 = Par0_m1,
               estAngleMean = list(angle=TRUE),
               fixPar=fixPar,
               knownStates = ks,
               #formula = ~julian_day, #state 2 to 3. page 47 of vignette shows how to create the beta matrix. Formula for transition probabilities
               stateNames = stateNames)#nlmPar=list(print.level=2)
  
  .x$step_state <- viterbi(m1)
  
  #step state shows the bird's behavior between points, point state shows the birds behavior at points
  .x$point_state <- map(1:nrow(.x),  function(i){
    if(i == 1){
      return(1) #for the first row, return 1 
      return(1)
    } else if(.x[i,"step_state"] == .x[i-1,"step_state"]){ #is the current step state the same as the last step state? Return the current step state
      return(.x[i,"step_state"])
    } else if(.x[i,"step_state"] == 2 & .x[i-1,"step_state"] == 1){ #is the current step state mig and the previous step pre-mig? Return pre-mig
      return(1) 
    } else if(.x[i,"step_state"] == 2 & .x[i-1,"step_state"] == 3){#is the current step state mig and the previous step stopover? Return stopover
      return(3) 
    } else{ #in all other circumstances, return the current step state
      return(.x[i,"step_state"])
    }
    
  }) %>% unlist()
  
  #step state shows the bird's behavior between points, point state shows the birds behavior at points
  
  return(list(model = m1, results = .x))
}

iterate_ids_male <- function(id_iter){
  print(as.character(id_iter))
  
  amwo_hmm_clipped <- amwo_hmm %>% #remove locations for the ID in question after the cutoff date
    filter(ID %in% id_iter)
  
  knownStates <- amwo_hmm_clipped %>%
    group_by(ID) %>%
    mutate(ranks = row_number()) %>%
    mutate(known = ifelse(ranks == "1", 1, NA)) %>%
    pull(known)
  
  results_modified <- fit_and_predict(amwo_hmm_clipped, ks = knownStates)
}

#scrap <- map(id_list, iterate_ids_male)

scrap <- iterate_ids_male(id_list[1:120]) #works!
scrap2 <- iterate_ids_male(id_list[121:241]) #fails
scrap3 <- iterate_ids_male(id_list[121:180]) #fails
scrap4 <- iterate_ids_male(id_list[181:241]) #works!
scrap5 <- iterate_ids_male(id_list[121:150]) #works!
scrap6 <- iterate_ids_male(id_list[151:180]) #fails
scrap7 <- iterate_ids_male(id_list[151:165]) #fails
scrap8 <- iterate_ids_male(id_list[166:180]) #fails
scrap9 <- iterate_ids_male(id_list[151:157]) #fails

scrap10 <- iterate_ids_male(id_list[158:165]) #bugs out (no zeros) come back to me

scrap11 <- iterate_ids_male(id_list[166:172]) #fails
scrap12 <- iterate_ids_male(id_list[173:180]) #works!

scrap13 <- iterate_ids_male(id_list[166:169]) #bugs out (no zeros) come back to me

scrap14 <- iterate_ids_male(id_list[170:172]) #fails
scrap15 <- iterate_ids_male(id_list[170]) #works!
scrap16 <- iterate_ids_male(id_list[171]) #bugs out (no zeros) come back to me
scrap17 <- iterate_ids_male(id_list[172]) #bugs out (no zeros) come back to me

scrap18 <- iterate_ids_male(id_list[151:154]) #bugs out (no zeros) come back to me
scrap19 <- iterate_ids_male(id_list[155:157]) #fails
scrap19 <- iterate_ids_male(id_list[155]) #works!
scrap19 <- iterate_ids_male(id_list[156])#bugs out (no zeros) come back to me
scrap19 <- iterate_ids_male(id_list[157])#bugs out (no zeros) come back to me

c("RI-2020-33-2020", "RI-2020-34-2020", "RI-2020-35-2020", "RI-2020-42-2020", "RI-2020-43-2020", "RI-2020-44-2020", "RI-2021-48-2021", "RI-2021-49-2021", "RI-2020-30-2020", "RI-2020-32-2020")

#no zeroes
scrap10 <- iterate_ids_male(id_list[158:165]) #fails
scrap10 <- iterate_ids_male(id_list[158:161]) #fails
scrap10 <- iterate_ids_male(id_list[158]) #fails
scrap10 <- iterate_ids_male(id_list[159]) #fails
scrap10 <- iterate_ids_male(id_list[160]) #fails
scrap10 <- iterate_ids_male(id_list[161]) #works!

scrap10 <- iterate_ids_male(id_list[161:165]) #works!

#set first step as migratory for these birds? Or allow known state to float?

scrap13 <- iterate_ids_male(id_list[166:169]) #fails
scrap13 <- iterate_ids_male(id_list[166]) #fails
scrap13 <- iterate_ids_male(id_list[167]) #fails
scrap13 <- iterate_ids_male(id_list[168]) #fails
scrap13 <- iterate_ids_male(id_list[169]) #works!

scrap16 <- iterate_ids_male(id_list[171]) #fails
scrap17 <- iterate_ids_male(id_list[172]) #fails

scrap19 <- iterate_ids_male(id_list[156])#fails
scrap19 <- iterate_ids_male(id_list[157])#fails
