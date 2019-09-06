library(tidyverse)
library(sf)
library(broom)
# set to the data/figures folder
setwd('C:/Users/emeador/OneDrive - SRUC/')

sys <- Sys.info()[1]

if(sys == 'Windows'){
    source('C:/Users/emeador/Downloads/all_functions.R')
}

resilience_census <- read_csv('scotland_ncr/data/resilience_census.csv')

resilience_indicators_sf <- st_read('Data/resilience/resilience_indicators_sf.gpkg')


dz_desc <- read_csv('Data/lookup/SG_Rural_Urban_dz_description.csv') %>% 
    select(ur8 = URCLASS, 
           description = URNAME) 

desc_order <- dz_desc %>% 
    pull(description)


dz_desc <- dz_desc %>% 
    mutate(description = factor(description, desc_order))





dz_ru <- read_csv('Data/lookup/SG_Rural_Urban_dz_lookup.csv') %>% 
    select(datazone = DZ_CODE,
           ur8 = UR8FOLD) %>% 
    left_join(dz_desc)


resas <- read_csv('Data/lookup/resas_rural_lookup.csv')





medical_resilience <- resilience_indicators_sf %>% 
    as_tibble() %>% 
    filter(resilience_type == 'Medical') %>% 
    select(datazone = DataZone, 
           dist)



medical_resilience_census <- medical_resilience %>% 
    left_join(resilience_census) %>% 
    distinct() %>% 
    left_join(dz_ru)






# dist_med_car <- 
    medical_resilience_census %>% 
    arrange(ur8) %>% 
    filter(prop_limited_health_no_car != 1) %>% 
    ggplot(aes(prop_limited_health_no_car*100, dist))+
    geom_point(aes(color = description))+
    geom_hline(yintercept = 2.5, linetype = 2)+
    geom_smooth(color = Greys[2])+
    scale_x_continuous(labels = percent_sign)+
    scale_y_log10()+
    scale_color_brewer(palette = 'YlGnBu', name = 'SG 8-Fold Rural-Urban')+
    theme_minimal()+
    theme(panel.grid = element_blank(), 
          axis.title.y = element_text(angle = 0), 
          plot.margin = margin(1,1,1,1, 'cm'), 
          legend.position = c(.75, .75), 
          legend.background = element_rect(color = 'black'))+ 
        guides(colour = guide_legend(override.aes = list(size = 5)))+
    annotate('label', x = 50, y = 2.75, label = 'Approx. walking distance', size = 3)+
    labs(title = 'Distance to Emergency-Medical Resilience Indicators and Percentage of People\n with Poor Health and no Vehicle',
         x = 'Proportion of resiendents with limited heatlh and no car',
         y = 'Median shortest distance to\nEmergency resilience indicator', 
         caption = 'Data are from the Scottish Census 2011 and Resilience Indicators')




ggsave(dist_med_car, 
       path = 'scotland_ncr/report_outputs/figures/dist_med_car',
       device = 'png',
       width = 11, 
       height = 8)



























































































































