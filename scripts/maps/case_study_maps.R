library(tidyverse)
library(tmap)
library(sf)
library(grid)
load('data/poi_march_2019_pross.RData')
load("data/resas_rural.RData")
load('data/Scotland_Resilience_Indicators.RData')
source('~/Downloads/all_functions.R')
load('data/resilience_indicators_data.RData')
# Read distance to community centre -------------

pc_to_resilience <- st_read('~/Downloads/pc_to_resilience (2).gpkg')


pc_to_resilience %>% 
  as_tibble() %>% 
  count(Name)



median_pc_to_resilience <- pc_to_resilience %>% 
  as_tibble() %>% 
  group_by(DataZone) %>% 
  summarise(median_distance = median(dist_km, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character)

SIMD_sf <- st_read(
  '~/Downloads/SG_SIMD_2016/SG_SIMD_2016.shp'
)

 # get the boundaries
# scot_border <- st_union(SIMD_sf)
# scot_border_sim <- st_simplify(scot_border)

# Read simple features or layers from file or database
# on local authority
pc_to_resilience_sf <- SIMD_sf %>% 
  left_join(median_pc_to_resilience, 
            by = c('DataZone' = 'datazone'))

# tm_shape(pc_to_resilience_sf)+
#   tm_fill('median_distance', 
#           palette = '-magma',
#           n = 8)

# Argyll & Bute ----------------


pc_to_resilience_sf <- SIMD_sf %>% 
 left_join(median_pc_to_resilience, 
          by = c('DataZone' = 'datazone'))


argyll_bute <- pc_to_resilience_sf %>% 
  filter(str_detect(LAName, 'Argyll and Bute'))

legend.pos <- c(0.05, 0.35)

argyll_bute_tmap <- tm_shape(argyll_bute)+
  tm_borders(col = 'grey', 
              lwd = .175)+
  tm_fill('median_distance', 
          colorNA = 'grey', 
          palette = 'Greens', 
          n = 8,
          title = 'Median km to Community\nDevelopment Indicator'
          )+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
   str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )+
  tm_layout(title = 'Argyll & Bute - Community Development Centres',
            bg.color = Blues[1],
            legend.position = legend.pos, 
            frame.double.line = T, 
            inner.margins = c(0.05, 0.05, .125, 0.05))




tmap_save(argyll_bute_tmap, 
          filename = 'png/argyll_bute_tmap.png',
          height = 8.5, 
          width = 8)


















# Highland ----------------


highland <- pc_to_resilience_sf %>% 
  filter(str_detect(LAName, 'Highland'))

legend.pos <- c(0.05, 0.75)

credit.pos <- c(.025, .55)

highland_map <- tm_shape(highland)+
  tm_borders(col = 'grey', 
             lwd = .175)+
  tm_fill('median_distance', 
          colorNA = 'grey', 
          palette = '-magma', 
          title = 'Median km to Community\nDevelopment Indicator'
  )+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = credit.pos,
    size = .75
  )+
  tm_layout(title = 'Highland - Community Development Centres',
            bg.color = Blues[1],
            legend.position = legend.pos, 
            frame.double.line = T, 
            inner.margins = c(0.05, 0.05, .125, 0.05))

# inset map

highland_inset_sf <- SIMD_sf %>% 
  mutate(highland = case_when(
  str_detect(LAName, 'Highland') ~ 'Highland', 
  T ~ 'Other')) 

highland_inset_sim <- st_simplify(highland_inset_sf)


highland_inset <- 
  tm_shape(scot_border_sim)+
   tm_borders()+
  tm_shape(highland_inset_sim)+
   tm_fill('highland', 
          palette = c('black','white'), 
          legend.show = F)



# save to .pdf

tmap_save(highland_map, 
          filename = 'pdf/highland_map.pdf',
          height = 8.5, 
          width = 8)

tmap_save(highland_inset, 
          filename = 'pdf/highland_inset.pdf',
          height = 8.5, 
          width = 8)

# Turn resilience points to SF ----------
scotland_admin <-
  st_read(
    'data/Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp'
  )



resilience_indicators_sf <-
  resilience_indicators_data %>%
  select(-main_industry) %>%
  st_as_sf(coords = c("feature_easting",
                      "feature_northing")) %>%
  st_sf(crs = st_crs(scotland_admin))



# Scotland Local Authority Map -----------


scotland_councils <- resas_rural %>%
  clean_council(la) %>%
  pull(la)

scotland_council_admin <- scotland_admin %>%
  clean_council(lad17nm) %>%
  filter(lad17nm %in% scotland_councils) %>%
  rename(local_authority = lad17nm) %>%
  left_join(resas_rural %>%
              clean_council(la) %>%
              select(local_authority = la,
                     RESAS = definition))

## make map

tm_shape(scotland_council_admin) +
  tm_fill('RESAS', palette = '-Spectral') +
  tm_shape(scotland_council_admin) +
  tm_borders(col = 'white',
             lwd = .15) +
  tm_layout(title = 'Scotland RESAS Rural',
            legend.text.size = .4) +
  tm_compass(type = '4star',
             position = c(0.065, 0.2),
             size = 2) +
  tm_scale_bar(position = c(0.75, 0.01),
               breaks = NULL,
               width = .15) +
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
      ),
    position = c(.05, .000),
    size = .5
  )










