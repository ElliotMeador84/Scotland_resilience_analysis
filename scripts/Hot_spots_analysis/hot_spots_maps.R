library(tidyverse)
library(sf)
library(tmap)
library(scales)
library(knitr)
library(kableExtra)
load('data/resilience_points_join_data.RData')
source('scripts/theme_min_plus.R')
rural_class <- read_csv('~/Downloads/00544933 (1).csv')

dz_urban_rural <- read_csv('~/Downloads/datazone_rural_urban_2016.csv')

# Make boundaries -------------

SIMD_polygon <- st_read(
  '~/Downloads/SG_SIMD_2016/SG_SIMD_2016.shp'
)

argyll_polygon <- SIMD_polygon %>% 
  filter(str_detect(LAName, 'Argyll'))  

argyll_union <- st_union(argyll_polygon)

highland_polygon <- SIMD_polygon %>% 
  filter(str_detect(LAName, 'Highland'))  
  

highland_union <- st_union(highland_polygon)



# Resilience metric for Argyll & Highlands ----------


urban_datazone_n <- dz_urban_rural %>% 
  filter(UR8_2013_2014 == 1) %>% 
  count(local_authority, name = 'tot_dz')


# Community Centres
metric_rural_count <- metric_database %>% 
  add_count(local_authority,description, rural) %>% 
  select(-datazone) %>% 
  distinct() 

# the most rural places in each local authority
metric_rural_count %>%
  filter(str_detect(description, 
                    'halls and community')) %>% 
  filter(rural == 1)  %>% 
  left_join(urban_datazone_n) %>% 
  select(-description) %>% 
  mutate(ratio = n/tot_dz, 
         local_authority = 
           fct_reorder(local_authority, ratio))



# Map of resilience indicators -------------

# categories 
medical_cats <- 
  resilience_points_join_data %>% 
  count(description) %>% 
  pull(description) %>% 
  .[c(2, 3, 6, 12)] 

emergency_cats <- 
  resilience_points_join_data %>% 
  count(description) %>% 
  pull(description) %>% 
  .[c(1,4, 7, 11, 14, 16)] 


everyday_cats <- 
  resilience_points_join_data %>% 
  count(description) %>% 
  filter(!description %in% 
           c(medical_cats, 
             emergency_cats)) %>% 
  pull(description)



resilience_points_join_data <- 
  resilience_points_join_data %>% 
  mutate('Resilience Indicator' = case_when(
    description %in% medical_cats ~ 'Emergency medical',
    description %in% emergency_cats ~ 'Emergency rescue',
    T ~ 'Everyday'
  ))


 ### ARGYLL & BUTE
argyll_resilience_points <- 
  resilience_points_join_data %>% 
  filter(str_detect(LAName, 'Argyll')) 

# indicators

argyll_resilience_points %>% 
  as_tibble() %>% 
  count(resilince_indicator,, description, sort = T)

# map
argyll_resilinece_map <- 
  tm_shape(argyll_union)+
  tm_borders('black', 
             lwd = .125)+
  tm_fill(Greys[2])+
  tm_shape(argyll_polygon)+
  tm_borders('black', 
             lwd = .125)+
  tm_shape(argyll_resilience_points)+
  tm_dots('Resilience Indicator', 
          size = .75,
          border.col = 'black',
          border.lwd = 1, 
          legend.show = T, 
          palette = Spectral[c(1, 6, 10)])+
  tm_layout(title = 'Argyll & Bute Resilience Indicators',
            title.color = 'black',
            bg.color = 'white', #Set1[2]
            legend.show = T, 
            legend.position = c('left', 'center'),
            legend.frame = T,
            legend.text.size = 1,
            legend.title.size = 1.25,
            frame = 'black', 
            frame.double.line = T, 
            inner.margins = rep(.05, 4))+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .025),
    size = .75, 
    col = 'black', 
    align = 'left'
  )

tmap_save(argyll_resilinece_map, 
          filename = 'png/draft_final/argyll_resilinece_map.png', 
          width = 8.25, 
          height = 8.25)


 ### HIGHLANDS
highland_resilience_points <- 
  resilience_points_join_data %>% 
  filter(str_detect(LAName, 'Highland')) 

highland_resilience_points%>% 
  as_tibble() %>% 
  count(resilince_indicator, description, sort = T)


highland_resilinece_map <- 
  tm_shape(highland_union)+
  tm_borders('black', 
             lwd = .125)+
  tm_fill(Greys[2])+
  tm_shape(highland_polygon)+
  tm_borders('black', 
             lwd = .125)+
  tm_shape(highland_resilience_points)+
  tm_dots('Resilience Indicator', 
          size = .75,
          border.col = 'black',
          border.lwd = 1, 
          legend.show = T, 
          palette = Spectral[c(1, 6, 10)])+
  tm_layout(title = 'Highland Resilience Indicators',
            title.color = 'black',
            bg.color = 'white', #Set1[2]
            legend.show = T, 
            legend.position = c('left', 'top'),
            legend.frame = T,
            legend.text.size = 1,
            legend.title.size = 1.25,
            frame = 'black', 
            frame.double.line = T, 
            inner.margins = rep(.05, 4))+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .025),
    size = .5, 
    col = 'black', 
    align = 'left'
  )

tmap_save(highland_resilinece_map, 
          filename = 'png/draft_final/highland_resilinece_map.png', 
          width = 8.25, 
          height = 8.25)












