library(tidyverse)
library(tmap)
library(sf)
library(grid)
source('~/Downloads/all_functions.R')
load('data/resilience_indicators_data.RData')





scotland_admin <-
  st_read(
    'data/Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp'
  )


# pc_to_resilience <- st_read('~/Downloads/pc_to_resilience (1).gpkg')


description_category_df <- read_csv('data/description.csv')


resilience_indicators_sf <-
  resilience_indicators_data %>%
  left_join(description_category_df) %>% 
  select(-main_industry) %>%
  arrange(desc_cat) %>% 
  st_as_sf(coords = c("feature_easting",
                      "feature_northing")) %>%
  st_sf(crs = st_crs(scotland_admin))









SIMD_sf <- st_read(
  '~/Downloads/SG_SIMD_2016/SG_SIMD_2016.shp'
)





# get the boundaries
scot_border <- st_union(SIMD_sf)
scot_border_sim <- st_simplify(scot_border)

# Read simple features or layers from file or database
# on local authority
# pc_to_resilience_sf <- SIMD_sf %>% 
#   left_join(median_pc_to_resilience, 
#             by = c('DataZone' = 'datazone'))


######################
### All indicators ###
######################


# All of Scotland -----------



Scotland_res <- tm_shape(SIMD_sf)+
  tm_fill(Greys[2])+
  tm_shape(scot_border_sim)+
  tm_borders('black', 
             lwd = .05125)+
  tm_shape(resilience_indicators_sf)+
  tm_dots('desc_cat', 
          size = .1725, 
          alpha = .95,
          palette = '-Spectral', 
          legend.show = F)+
  tm_compass(type = '4star')+
  tm_scale_bar() +
  tm_facets(by = "desc_cat")



tmap_save(Scotland_res, 
          filename = 'pdf/Scotland_res.pdf',
          height = 8, 
          width = 12)


# Argyll & Bute --------------


argyll_border <- SIMD_sf %>% 
  filter(str_detect(LAName, 'Argyll and Bute'))


argyll_bute_res <- resilience_indicators_sf %>% 
  filter(str_detect(geographic_county, 'Argyll and Bute'))



tmap_style("beaver")

legend.pos <- c(0.05, 0.35)

ARGYLL_RES_FULL <- tm_shape(argyll_border)+
  tm_borders('black', 
             lwd = .125)+
  tm_fill(Greys[3])+
tm_shape(argyll_bute_res)+
  tm_dots('description', 
          size = .25)+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)\n\nLOCATIONS ARE APPROXIMATE",
      30
    ),
    position = c(.025, .05),
    size = .5
  )+
  tm_layout(title = 'Argyll & Bute',
            bg.color = Blues[1],
            legend.position = legend.pos, 
            frame.double.line = T, 
            inner.margins = c(0.05, 0.05, .125, 0.05))




tmap_save(ARGYLL_RES_FULL, 
          filename = 'pdf/ARGYLL_RES_FULL.pdf',
          height = 8.5, 
          width = 8)











# Aberdeenshire --------------






aberdeenshire_border <- SIMD_sf %>% 
  filter(str_detect(LAName, 'Aberdeenshire'))


aberdeenshire_res <- resilience_indicators_sf %>% 
  filter(str_detect(geographic_county, 'Aberdeenshire'))




legend.pos <- c(0.05, 0.5)

ABERDEENSHIRE_RES_FULL <- 
  tm_shape(aberdeenshire_border)+
  tm_fill(Greys[3])+
tm_shape(aberdeenshire_res)+
  tm_dots('description', size = .5)+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)\n\nLOCATIONS ARE APPROXIMATE",
      30
    ),
    position = c(.025, .05),
    size = .5
  )+
  tm_layout(title = 'Aberdeenshire',
            bg.color = Blues[1],
            legend.position = legend.pos, 
            frame.double.line = T, 
            inner.margins = c(0.05, 0.05, .125, 0.05))




tmap_save(ABERDEENSHIRE_RES_FULL, 
          filename = 'pdf/ABERDEENSHIRE_RES_FULL.pdf',
          height = 8.5, 
          width = 8)



##################
### Categories ###
##################



