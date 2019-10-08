library(tidyverse)
library(ggraph)
library(tidygraph)
library(readxl)
library(janitor)
library(sf)
library(tmap)
library(rayshader)
setwd('C:/Users/emeador/OneDrive - SRUC')
# load resilience data



resilience_indicators_sf <-
    st_read('Data/resilience/resilience_indicators_sf.gpkg')

# Load borders data --------

# load Scotland local authority
local_authority_border <-
    st_read('Data/geographic/boundaries/local_authority_border.gpkg')

# load Scotland border
scotland_border <-
    st_read('Data/geographic/boundaries/scotland_border.gpkg')


# Everyday Resilience -----------


every_day_resilience <- resilience_indicators_sf %>%
    filter(resilience_type == 'Everyday')


ggplot_everyday <-
    ggplot() + 
    geom_sf(data = every_day_resilience, 
            aes(fill = dist), 
            color = NA, 
            show.legend = T) + 
    theme_minimal()+
    scale_fill_viridis_c(direction = -1, 
                         name = 'Distance (km)', 
                         option = 'magma')+
    labs(title = 'Community Resilience Indicators')



plot_gg(
    ggplot_everyday,
    width = 3.5,
    multicore = TRUE,
    windowsize = c(800, 800),
    zoom = 0.85,
    phi = 35,
    theta = 30,
    sunangle = 225,
    soliddepth = -100
)


render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
render_snapshot(
    filename = 'scotland_ncr/figures/community_shader.png',
                clear = TRUE)











