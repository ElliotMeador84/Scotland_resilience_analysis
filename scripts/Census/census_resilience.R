library(tidyverse)
source('~/Downloads/all_functions.R')
source('scripts/Census/census_import.R')
load('data/post_code_SIMD.RData')

census.files <- 
  census_read('car', grab = 5, read = T) %>% 
  set_names('car_or_van_availability_by_sex_by_age')

age_range <- 
  paste(c('50 to 64', 
          '65 and over'))


df <- 
  census.files$car_or_van_availability_by_sex_by_age %>% 
  rename(gender = x2, 
         age = x3) %>% 
  filter(datazone != 'Scotland', 
         gender == 'All people in households:', 
         age %in% age_range) %>% 
  select(-gender)
  

census_prop_no_car_elderly <- df %>% 
  select(datazone, 
         age,
         all_people_in_households,
         number_of_cars_or_vans_in_household_no_cars_or_vans) %>% 
  rename(no_car = 4) %>% 
  mutate(prop_no_car = no_car/all_people_in_households) %>% 
  group_by(datazone, age) %>% 
  summarise(median = median(prop_no_car, na.rm = T)) %>% 
  ungroup()


resilience_census <- census_prop_no_car_elderly %>% 
  spread(age, median) %>% 
  rename(prop_no_car_50_64 = 2, 
         prop_no_car_65_over = 3)








bravo <- census_read('car_or_van', grab = 4, read = T)

charlie <- bravo[[1]] %>% 
  select(1:5) %>% 
  rename(gender = x2, 
         num_people = x3) %>% 
  filter(datazone != 'Scotland', 
         gender == 'All people in households:') %>% 
  select(-gender) %>% 
  rename(highly_limited = 4)



echo <- charlie %>% 
  mutate(prop_limited_lot = highly_limited/all_people_in_households) %>% 
  group_by(datazone, num_people) %>% 
  summarise(prop_limited_lot = median(prop_limited_lot, na.rm = T)) %>% 
  ungroup()

echo %>% 
  count(num_people, sort = T)

resilience_census <- echo %>% 
  spread(num_people, prop_limited_lot) %>% 
  clean_names() %>%
  select(1,2) %>% 
  rename(prop_limited_health_no_car = 2) %>% 
  right_join(resilience_census)


save(resilience_census, 
     file = 'data/census_2011_so/resilience/resilience_census.RData')










