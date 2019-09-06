library(tidyverse)
library(readr)
library(janitor)


poi_classifications <- 
  read_delim("license/docs/POI_CLASSIFICATIONS.txt", "|", 
             escape_double = FALSE, 
             trim_ws = TRUE) %>% 
  clean_names() %>% 
  mutate_if(is.character, str_to_sentence) %>% 
  select(-id)


poi_march_2019_pross %>% names()
  filter(str_detect(name, 'camp')) %>% 
  View()

str_length(poi_march_2019_pross$point_x_classification_code)
  

poi_classifications %>% 
  unite('point_x_classification_code', 
        'category_number_foreign_key',
        'class_number', 
        sep = '') 
  
  

















