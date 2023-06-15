a <- fall_classified %>% 
  pull(ID) %>% 
  unique() %>% 
  length()

b <- spring_male_classified %>% 
  pull(ID) %>% 
  unique() %>% 
  length()

c <- spring_female_classified %>% 
  pull(ID) %>% 
  unique() %>% 
  length()

a+b+c

d <- fall_classified %>% 
  pull(animal_name)

e <- spring_male_classified %>% 
  pull(animal_name)

f <- spring_female_classified %>% 
  pull(animal_name)

g <- c(d,e,f) %>% 
  unique()
