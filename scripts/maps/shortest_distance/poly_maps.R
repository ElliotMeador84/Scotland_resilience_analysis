library(tidyverse)
library(ggraph)
library(tidygraph)
library(readxl)
library(janitor)
library(sf)
library(tmap)


# load resilience data

resilience_indicators_sf <- st_read('~/OneDrive - SRUC/Data/resilience/resilience_indicators_sf.gpkg')

# Load borders data --------

# load Scotland local authority
local_authority_border <- st_read('~/OneDrive - SRUC/Data/geographic/boundaries/local_authority_border.gpkg')

# load Scotland border
scotland_border <- st_read('~/OneDrive - SRUC/Data/geographic/boundaries/scotland_border.gpkg')


# Everyday Resilience -----------


every_day_resilience <- resilience_indicators_sf %>% 
  filter(resilience_type == 'Everyday')


every_day_map <- 
  tm_shape(scotland_border)+
  tm_borders(col = 'black')+
  tm_shape(every_day_resilience)+
  tm_fill('dist', 
          palette = '-magma', 
          n = 12, 
          title = 'Median distance (km)')+
  tm_shape(local_authority_border)+
  tm_borders(col = 'black')+
  tm_layout(title = 'Shortest Distance Analysis to Everyday Resilience Indicators', 
            legend.frame = T)+
  tm_scale_bar(position = c('right', 'bottom'))+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )


tmap_save(every_day_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/every_day_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')

# Emergency Resilience -----------


emergency_resilience <- resilience_indicators_sf %>% 
  filter(resilience_type == 'Emergency')


emergency_map <- 
  tm_shape(scotland_border)+
  tm_borders(col = 'black')+
  tm_shape(emergency_resilience)+
  tm_fill('dist', 
          palette = '-magma', 
          n = 12, 
          title = 'Median distance (km)')+
  tm_shape(local_authority_border)+
  tm_borders(col = 'black')+
  tm_layout(title = 'Shortest Distance Analysis to Emergency Resilience Indicators', 
            legend.frame = T)+
  tm_scale_bar(position = c('right', 'bottom'))+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )


tmap_save(emergency_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/emergency_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')



# Medical Resilience -----------


medical_resilience <- resilience_indicators_sf %>% 
  filter(resilience_type == 'Medical')


medical_map <- 
  tm_shape(scotland_border)+
  tm_borders(col = 'black')+
  tm_shape(medical_resilience)+
  tm_fill('dist', 
          palette = '-magma', 
          n = 12, 
          title = 'Median distance (km)')+
  tm_shape(local_authority_border)+
  tm_borders(col = 'black')+
  tm_layout(title = 'Shortest Distance Analysis to Emergency-Medical Resilience Indicators', 
            legend.frame = T)+
  tm_scale_bar(position = c('right', 'bottom'))+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )


tmap_save(medical_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/medical_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')



    ############################
    #### Hot spot locations ####
    ############################


# load resilience data

resilience_indicators_sf <- st_read('~/OneDrive - SRUC/Data/resilience/resilience_indicators_sf.gpkg')

# Load borders data --------

# load Scotland local authority
local_authority_border <- st_read('~/OneDrive - SRUC/Data/geographic/boundaries/local_authority_border.gpkg')

# load Scotland border
scotland_border <- st_read('~/OneDrive - SRUC/Data/geographic/boundaries/scotland_border.gpkg')




# Attach local authority areas ----------

data_zone_council_lu <- read_csv('~/OneDrive - SRUC/Data/lookup/Datazone_council_lookup.csv')

la_codes <- read_csv('~/OneDrive - SRUC/Data/lookup/Local_Authority_Districts_Codes_Names.csv')

scotland_la_codes <- la_codes %>% 
  select(Council = LAD17CD, 
         la = LAD17NM) %>% 
  filter(str_detect(Council, '^S'))

datazone_council <- data_zone_council_lu %>% 
  select(DataZone, Council) %>% 
  left_join(scotland_la_codes) %>% 
  select(DataZone, la)
  

resilience_indicators_council <- 
  resilience_indicators_sf %>% 
  left_join(datazone_council)
  


  ##############
  ## Highland ##
  ##############
 
# Highland border

highland_border <- local_authority_border %>% 
  filter(str_detect(LAName, 'Highland'))



# Everyday Resilience 

highland_every_day <- 
  resilience_indicators_council %>% 
  filter(resilience_type == 'Everyday', 
         str_detect(la, 'Highland'))


highland_everyday_map <- tm_shape(highland_border)+
  tm_borders(col = 'black')+
  tm_shape(highland_every_day)+
  tm_fill('dist', 
          palette = '-magma', 
          n = 12, 
          title = 'Median distance (km)')+
  tm_layout(title = 'Highland - Shortest Distance Analysis to Everyday Resilience Indicators', 
            legend.frame = T, 
            inner.margins = c(.16, .16, .16, .16))+
  tm_scale_bar(position = c('right', 'bottom'))+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )

tmap_save(highland_everyday_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/highland_everyday_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')






  #############
  ## Argyll ##
  #############
 
# Argyll border

argyll_border <- local_authority_border %>% 
  filter(str_detect(LAName, 'Argyll'))



# Everyday Resilience 

argyll_every_day <- 
  resilience_indicators_council %>% 
  filter(resilience_type == 'Everyday', 
         str_detect(la, 'Argyll'))


argyll_everyday_map <- tm_shape(argyll_border)+
  tm_borders(col = 'black')+
  tm_shape(argyll_every_day)+
  tm_fill('dist', 
          palette = '-magma', 
          n = 12, 
          title = 'Median distance (km)')+
  tm_layout(title = 'Argyll and Bute - Shortest Distance Analysis to Everyday Resilience Indicators', 
            legend.frame = T, 
            legend.position = c('left', 'centre'),
            inner.margins = c(.15, .15, .15, .15))+
  tm_scale_bar(position = c('right', 'bottom'))+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .05),
    size = .75
  )

tmap_save(argyll_everyday_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/argyll_everyday_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')


   #####################
   ### Backdrop maps ###
   #####################


# Highland

highland_inlet <- local_authority_border %>% 
  mutate(highland  = case_when(
    str_detect(LAName, 'Highland')~ 'Highland', 
    T~ 'Rest of Scotland'))



highland_inlet_map <- tm_shape(highland_inlet)+
  tm_fill('highland', 
          palette = c('grey', 'black'))+
  tm_layout(legend.show = F, 
            bg.color = 'transparent')


tmap_save(highland_inlet_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/highland_inlet_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')


  # Argyll
argyll_inlet <- local_authority_border %>% 
  mutate(argyll  = case_when(
    str_detect(LAName, 'Argyll')~ 'Argyll & Bute', 
    T~ 'Rest of Scotland'))



argyll_inlet_map <- tm_shape(argyll_inlet)+
  tm_fill('argyll', 
          palette = c('grey', 'black'))+
  tm_layout(legend.show = F, 
            bg.color = 'transparent')


tmap_save(argyll_inlet_map, 
          filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/argyll_inlet_map.pdf', 
          width = 210, 
          height = 297, 
          unit = 'mm')








