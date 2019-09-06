library(tidyverse)
library(scales)
library(janitor)
source('~/Downloads/all_functions.R')
file.edit('~/Documents/R/Scotland_NCR/scripts/resilience_points_df.R')
load('data/poi_march_2019_pross.RData')

 # pull final resilience indicators from other NCR folder
 
resilience_indictors_df <- read_csv('~/Documents/R/Scotland_NCR/data/resilience_points_df.csv')

resilience_points_v <- resilience_indictors_df %>% 
  pull(1) %>% 
  str_to_lower()




resilience_indicators_data <- poi_march_2019_pross %>% 
  mutate(description = 
           str_to_lower(description)) %>% 
  filter(description %in% resilience_points_v)
  
  
save(resilience_indicators_data, 
     file = 'data/resilience_indicators_data.RData')


























