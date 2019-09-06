library(tidyverse)
load('data/resilience_indicators_data.RData')
source('~/Downloads/all_functions.R')

resilience_indicators_data %>% 
  mutate(id = row_number()) %>% 
  select(id, 
         name, 
         feature_easting, 
         feature_northing,
         administrative_boundary,
         description) %>% 
  write_csv('data/outputs/resilience_indicators.csv')
