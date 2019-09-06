library(tidyverse)
library(janitor)
library(ggsci)
library(scales)
library(glue)
rural_urban_lookup <- read_csv('~/OneDrive - SRUC/Data/lookup/SG_Rural_Urban_dz_lookup.csv') %>% 
  set_names('data_zone', 'ur2', 'ur3', 'ur6', 'ur8')

resilience_dist <- read_csv('~/OneDrive - SRUC/scotland_ncr/data/resilience_indicators_dist_dz.csv') %>% 
  clean_names() %>% 
  mutate_if(is.factor, as.character)


rural_urban_description <- read_csv('~/OneDrive - SRUC/Data/lookup/SG_Rural_Urban_dz_description.csv') %>% 
  clean_names()

resilience_dist_ru <- resilience_dist %>% 
  left_join(rural_urban_lookup)


# Plots ----------


point_data <- resilience_dist_ru %>% 
  unite(new_vary, c('resilience_type', 'ur8'), 
        remove = F)  

point_data <- point_data %>% 
  separate(new_vary, c('resilience_type','ur8'), 
           remove = F) %>% 
  mutate(index = group_indices(., resilience_type)) %>% 
  unite(new_order, c('ur8', 'index'), 
        sep = '.', 
        remove = F) %>% 
  arrange(new_order) 

 # color palette


show_col(pal_startrek("uniform")(7))
show_col(pal_aaas('default')(7))
show_col(pal_d3('category20c')(7))
show_col(pal_igv('default')(12))

pal_igv('default')(12)

resilience_cols <- c('#5DB1DDFF', '#F0E685FF', '#924822FF')

names(resilience_cols) <- c('Everyday', 'Emergency', 'Medical')

#labels

ru_names <- pull(rural_urban_description, urname) %>% 
  str_wrap(15)

point_labels <- map(ru_names, function(x){
  c(' ', x, ' ')
}) %>% 
  flatten_chr()


resilience_ru_box <- 
  point_data %>% 
  ggplot(aes(new_order, dist))+
  geom_jitter(aes(color = resilience_type))+
  geom_boxplot(aes(fill = resilience_type), 
               alpha = .95, outlier.color = NA)+
  scale_y_log10()+
  scale_x_discrete(labels = point_labels)+
  theme_minimal()+
  scale_fill_manual(values = resilience_cols)+
  scale_color_manual(values = resilience_cols)+
  labs(title = 'Median Distance to Resilience Type Across Rural-Urban', 
         x = '8-Fold Rural-Urban Classification', 
       y  = 'Median Distance (km)\nto Resilience Type', 
       color = 'Resilience Type', 
       fill = 'Resilience Type')+
  theme(axis.title.y = element_text(angle = 0), 
        panel.grid = element_blank(), 
        axis.ticks = element_line(color = 'black'), 
        plot.margin = margin(1, 1, 1, 1, 'cm'), 
        legend.position = c(.15, .85), 
        legend.box.background = element_rect(color = 'black'))
  
ggsave(resilience_ru_box, 
       filename = '~/OneDrive - SRUC/scotland_ncr/report_outputs/figures/resilience_ru_box.png',
       height = 210, 
       width = 297, 
       unit = 'mm')



# Resilience metric -------------

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

resilience_dist_ru %>% 
  filter(str_detect(resilience_type, 'Everyday')) %>% 
  select(data_zone, 
         resilience_type, 
         ur8, 
         dist) 







































