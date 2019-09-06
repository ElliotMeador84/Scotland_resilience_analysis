library(tidyverse)
library(tmap)
library(sf)
library(grid)
load('data/poi_march_2019_pross.RData')
load("data/resas_rural.RData")
load('data/Scotland_Resilience_Indicators.RData')
source('~/Downloads/all_functions.R')
load('data/resilience_indicators_data.RData')
# Make datazone count data -------------


pc_to_resilience <- st_read('~/Downloads/pc_to_resilience (2).gpkg')


SIMD_polygon <- st_read(
  '~/Downloads/SG_SIMD_2016/SG_SIMD_2016.shp'
)


rural_index_union <-
  st_read('geo_spatial/scotland_border/scotland_border_simp.shp')

resilience_indicators_data_sf <- 
  resilience_indicators_data %>%
  select(reference_number,
         name, 
         description, 
         feature_easting, 
         feature_northing) %>% 
    st_as_sf(coords = c("feature_easting",
                        "feature_northing")) %>%
    st_sf(crs = st_crs(SIMD_polygon)) 




resilience_pts_join <- 
  st_join(resilience_indicators_data_sf, 
        SIMD_polygon, 
        join = st_intersects) 


resilience_points_join_data <- 
  resilience_pts_join %>% 
  select(name, description, DataZone, everything())




save(resilience_points_join_data, 
     file = 'data/resilience_points_join_data.RData')



resilience_points_n_datazone <- 
  resilience_points_join_data %>% 
  as_tibble() %>% 
  set_names(str_to_lower(names(.))) %>% 
  mutate_if(is.factor, as.character) %>% 
  count(datazone, description) %>% 
  na.omit() %>% 
  arrange(-n)




# link counts df to rural-urban

dz_urban_rural <- read_csv('~/Downloads/datazone_rural_urban_2016.csv')



resilience_points_n_datazone %>% 
  left_join(dz_urban_rural, 
            by = 'datazone') %>% 
  distinct()





# Mike file -------------

# --------------------------------
# --------------------------------
# Example db read and join
# --------------------------------
# --------------------------------

library(RSQLite)
library(tidyverse)
library(rgdal)

db = dbConnect(SQLite(), dbname="~/Downloads/pc_to_resilience (2).gpkg")

# db structure
dbListTables(db)
dbListFields(db, "postcode_to_POI")

postcodes = dbGetQuery(db, "SELECT datazone, median(dist_km) AS dist
                       FROM postcode_to_POI
                       GROUP BY datazone") %>% 
  as_tibble()

datazones = readOGR("~/Downloads/pc_to_resilience (2).gpkg", "DataZone_2011")

datazones@data = datazones@data %>% 
  left_join(postcodes, by = c(DataZone = "datazone"))



indicators_pc_distance <- datazones@data %>% 
  as_tibble()


save(indicators_pc_distance, file = 'data/indicators_pc_distance.RData')

















