library(tidyverse)
library(janitor)
load('data/resilience_indicators_data.RData')
source('~/Downloads/all_functions.R')

post_code_lookup <- 
  read_csv('~/Downloads/spd-pc-index-csv-cut-19-1/LargeUser.csv') %>% 
  clean_names()

post_dz <- post_code_lookup %>% 
  select(postcode, 
         datazone = data_zone2011code, 
         rural_urban = urban_rural8fold2016code) %>% 
  mutate(postcode = str_to_upper(postcode), 
         postcode = str_squish(postcode), 
         postcode = str_sub(postcode, 1,8)) %>% 
  arrange(postcode)
  

post_dz %>% 
  count(postcode, datazone, rural_urban) %>% 
  arrange(-n)




resilience_indicators_rural_urban <- 
  resilience_indicators_data %>% 
  mutate(postcode = str_to_upper(postcode), 
         postcode = str_squish(postcode), 
     postcode = str_sub(postcode, 1,8)) %>% 
  left_join(post_dz) 



save(resilience_indicators_rural_urban, 
     file = 'data/resilience_indicators_rural_urban.RData')





resilience_indicators_rural_urban %>% 
  nrow()

resilience_indicators_rural_urban %>% 
  drop_na(rural_urban) %>% 
  distinct() %>% 
  View()








