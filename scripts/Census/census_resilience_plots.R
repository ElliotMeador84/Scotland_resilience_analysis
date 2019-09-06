library(tidyverse)
library(sf)
library(broom)
# set to the data/figures folder
setwd('C:/Users/emeador/OneDrive - SRUC/')


resilience_census <- read_csv('scotland_ncr/data/resilience_census.csv')

resilience_indicators_sf <- st_read('Data/resilience/resilience_indicators_sf.gpkg')


medical_resilience <- resilience_indicators_sf %>% 
    as_tibble() %>% 
    filter(resilience_type == 'Medical') %>% 
    select(datazone = DataZone, 
           dist)



medical_resilience_census <- medical_resilience %>% 
    left_join(resilience_census) %>% 
    distinct()

resilience_census$prop_limited_health_no_car+100), medical_resilience_census$dist)

dist_med_car <- medical_resilience_census %>% 
    filter(prop_limited_health_no_car != 1) %>% 
    ggplot(aes(prop_limited_health_no_car*100, dist))+
    geom_point(color = Blues[5])+
    geom_hline(yintercept = 7, linetype = 2)+
    geom_smooth(color = Greys[2])+
    scale_x_continuous(labels = percent_sign)+
    scale_y_log10()+
    theme_minimal()+
    theme(panel.grid = element_blank(), 
          axis.title.y = element_text(angle = 0), 
          plot.margin = margin(1,1,1,1, 'cm'))+
    annotate('label', x = 50, y = 7.75, label = 'Approx. walking distance', size = 3)+
    labs(title = 'Distance to Emergency-Medical Resilience Indicators and Percentage of People\n with Poor Health and no Vehicle',
         x = 'Proportion of resiendents with limited heatlh and no car',
         y = 'Median shortest distance to\nEmergency resilience indicator', 
         caption = 'Data are from the Scottish Census 2011 and Resilience Indicators')




ggsave(dist_med_car, 
       path = 'scotland_ncr/report_outputs/figures/dist_med_car',
       device = 'png',
       width = 11, 
       height = 8)



























































































































