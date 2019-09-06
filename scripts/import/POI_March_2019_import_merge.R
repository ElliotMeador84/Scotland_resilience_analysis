library(tidyverse)
library(ggpubr)
library(scales)
library(janitor)
library(sp)
source('~/Downloads/all_functions.R')

# Raw_POI_import -----------------

poi_march_2019 <- read_delim("data/poi_2959457/poi.csv",
                             "|",
                             escape_double = FALSE,
                             trim_ws = TRUE) %>%
  clean_names()

# POI_category_import ----------------

poi_categories_march_2019 <-
  read_delim(
    '~/Documents/R/Scotland_NCR/data/Download_POINTS_Scotland_1273785/poi_3041830/docs/POI CATEGORIES.txt',
    "|",
    escape_double = FALSE,
    trim_ws = TRUE
  ) %>%
  clean_names() %>%
  mutate(category_description = str_to_title(category_description))

# POI_classification_import -------------

poi_classification_march_2019 <-
  read_delim(
    "~/Documents/R/Scotland_NCR/data/Download_POINTS_Scotland_1273785/poi_3041830/docs/POI_CLASS_TO_SIC_LOOKUP.txt",
    "|",
    escape_double = FALSE,
    trim_ws = TRUE
  ) %>%
  clean_names()

# POI_groups_import ---------------

poi_groups_march_2019 <- 
  read_delim("~/Documents/R/Scotland_NCR/data/Download_POINTS_Scotland_1273785/poi_3041830/docs/POI GROUPS.txt",
             "|",
             escape_double = FALSE,
             trim_ws = TRUE) %>%
  clean_names()

# Merge & Save -------------------

poi_march_2019_pross <- poi_march_2019 %>%
  left_join(poi_classification_march_2019)%>% 
  mutate_at(vars(contains('_sic_')), 
            list(~parse_number(.))) 

save(poi_march_2019_pross, 
     file = 'data/poi_march_2019_pross.RData')

# Clean and merge the SIC classifications ---------------

load('~/Downloads/SIC_2007_Lookup.RData')

SIC_mission <- poi_march_2019_pross %>% 
  select(contains('_sic_2007')) %>% 
  gather(key, value) %>% 
  group_by(key) %>% 
  summarise(mis = sum(is.na(value))/n()) %>% 
  mutate(complete = 1-mis, 
         key = fct_reorder(key, complete)) 


(percent_SIC_Complete_g <- SIC_mission %>% 
    ggplot(aes(key, complete))+
    geom_segment(
      aes(x = key, 
          y = 0, 
          xend = key, 
          yend = complete), 
      size = 2.5)+
    geom_point(size = 5)+
    scale_y_continuous(labels = percent)+
    coord_flip()+
    labs(title = '\nPercent complete (non-NA) for each SIC column\n', 
         x = '', 
         y = '')+
    theme_pubr(x.text.angle = 45))

# Best to just use first & second SIC varies

load('data/poi_march_2019_pross.RData')

poi_march_2019_pross <- poi_march_2019_pross %>% 
  rename(sic2007_code = first_sic_2007) %>% 
  select(-c(second_sic_2007:seventh_sic_2007), 
         -c(first_sic_2003:seventh_sic_2003)) %>% 
  left_join(SIC_2007_Lookup)

save(poi_march_2019_pross, 
     file = 'data/poi_march_2019_pross.RData')

# Add longitude & Latitude ---------------

# load('data/poi_2959457/poi_march_2019_pross.RData')
# 
# dat <- poi_march_2019_pross %>% 
#   select(feature_easting, feature_northing)
# 
# east_north <- SpatialPoints(
#   dat,
#   proj4string=CRS("+init=epsg:27700"))
# 
# long_lat <- spTransform(
#   east_north, 
#   CRS("+init=epsg:4326")) %>% 
#   as_tibble() %>% 
#   purrr::set_names('long', 'lat')
# 
# poi_march_2019_pross <- 
#   bind_cols(poi_march_2019_pross, 
#             long_lat)
# 
# save(poi_march_2019_pross, 
#      file = 'data/poi_2959457/poi_march_2019_pross.RData')
# 
# # Add SIMD Indicators ---------------
# 
# load('data/SIMD_2016_process.RData')
# load('data/post_code_SIMD.RData')
# 
# poi_march_2019_pross <- 
#   poi_march_2019_pross %>% 
#   left_join(
#     post_code_SIMD %>% 
#       select(postcode = Postcode, 
#              datazone = DZ) 
#   ) %>% 
#   left_join(SIMD_2016_process) %>% 
#   distinct()
# 
# save(poi_march_2019_pross, 
#      file = 'data/poi_march_2019_pross.RData')



# Clean up local authority names -------------
load('~/Downloads/scotland_la_fortify.RData')

scotland_la <- scotland_la_fortify %>% 
  count(id) %>% 
  clean_council(id) %>% 
  pull(id)

poi_march_2019_pross <- poi_march_2019_pross %>% 
  clean_council(administrative_boundary) %>% 
  filter(administrative_boundary %in% scotland_la) 

save(poi_march_2019_pross, 
     file = 'data/poi_march_2019_pross.RData')



















