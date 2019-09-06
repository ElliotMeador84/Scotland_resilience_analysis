library(tidyverse)
library(ggpubr)
library(scales)
library(janitor)
library(sp)
source('~/all_functions.R')
load('data/scotland_la_fortify.RData')
# Raw_POI_import -----------------

poi_march_2019 <- read_delim("data/poi_2959457/poi.csv",
                             "|",
                             escape_double = FALSE,
                             trim_ws = TRUE) %>%
  clean_names()


council_names <- scotland_la_fortify %>%
  count(id) %>%
  clean_council(id) %>%
  pull(id)


community_centre <- poi_march_2019 %>%
  mutate(name2 = str_to_lower(name)) %>%
  filter(
    str_detect(name2, 'community'),
    str_detect(name2, 'centre'),!str_detect(name2, 'bus stop')
  )



community_centre_poi <- community_centre %>%
  clean_council(administrative_boundary) %>%
  filter(administrative_boundary %in% council_names) %>%
  select(-name2)


# write_csv(community_centre_poi,
#           path  = 'data/community_centre_poi.csv')


filter_words <- paste('\\b',
      c('mail', 
        'newspaper', 
        'gazette', 
        'herald', 
        'inquirer', 
        'press'), 
      '\\b', collapse = '|', sep = '')



poi_march_2019 %>%
  transmute(name2 = str_to_lower(name)) %>%
  filter(
    str_detect(name2, filter_words)) %>% 
  View()












