library(tidyverse)
library(sf)
library(tmap)
library(scales)
library(knitr)
library(kableExtra)
load('data/resilience_points_join_data.RData')
source('scripts/theme_min_plus.R')
rural_class <- read_csv('~/Downloads/00544933 (1).csv')
SIMD_polygon <- st_read(
  '~/Downloads/SG_SIMD_2016/SG_SIMD_2016.shp'
)


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



rural_res_agg <- 
  resilience_points_n_datazone %>% 
  left_join(dz_urban_rural, 
            by = 'datazone') %>% 
  distinct()

# General Statistics resilience indicators --------

rural_labels <- rural_class %>% 
  pull(URNAME)


(community_indicators_across_rural_urban <- 
    rural_res_agg %>% 
  filter(str_detect(description, 'community')) %>% 
  count(description, UR8_2013_2014) %>% 
  ggplot(aes(factor(UR8_2013_2014), n))+
  geom_boxplot(aes(fill = UR8_2013_2014), 
               show.legend = F)+
  scale_fill_manual(values = ru_ur_cols)+
    scale_x_discrete(labels = rural_labels)+
  coord_flip()+
  theme_min_plus+
  labs(x = 'Scottish Government\n8-Fold Rural-Urban Classification', 
       y = '# Community Resilience Indicators',
       subtitle = '~urban areas have community resilience indicators than do rural places', 
       title = 'Community Resilience Indicators Accross Rural-Urban'))


ggsave(community_indicators_across_rural_urban, 
filename = 'png/draft_final/community_indicators_across_rural_urban.png', width = 8, 
height = 6)


 # a map of resilience indicators

resilience_points <- 
  resilience_points_join_data %>% 
  filter(str_detect(description, 'community')) %>% 
  mutate(marker = 'marker')

scotland_union <- st_union(SIMD_polygon)



community_resilinece_map <- 
  tm_shape(scotland_union)+
    tm_borders('black', 
               lwd = .125)+
    tm_fill(Greys[2])+
  tm_shape(resilience_points)+
    tm_dots('marker', 
          size = .25,
          legend.show = F,
          col = Set1[2])+
  tm_layout(title = 'Community Centre & Hall Resilience Indicators\n  in Scotland',
            title.color = 'black',
            bg.color = 'white', #Set1[2]
            legend.show = F, 
            frame = 'black', 
            frame.double.line = T)+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .025),
    size = .65, 
    col = 'black', 
    align = 'left'
  )

tmap_save(community_resilinece_map, 
 filename = 'png/draft_final/community_resilinece_map.png', 
          width = 6, 
          height = 9)


# Clusters of community halls -------------

# what are the range min - max of community centres

   # consider islands


rural_res_agg %>% 
  select(datazone, description, n)

# Clusters of emergency centres -------------

 # A map of emergency resilience indicators

# a map of resilience indicators

emergency_cats <- 
  resilience_points_join_data %>% 
  count(description) %>% 
  pull(description) %>% 
  .[c(1,4, 7, 11, 14, 16)] 




emergency_resilience_points <- 
  resilience_points_join_data %>% 
  filter(description %in% emergency_cats) %>% 
  mutate(Category = description)


emergency_resilience_points %>% 
  nrow()

emergency_resilience_indicators_map <- 
  tm_shape(scotland_union)+
  tm_borders('black', 
             lwd = .125)+
  tm_fill(Greys[2])+ #
  tm_shape(emergency_resilience_points)+
  tm_dots('Category', 
          size = .25,
          palette = '-Set3',
          border.col = 'white')+
  tm_layout(title = 'Emergency Resilience Indicators in Scotland',
            title.color = 'black',
            bg.color = 'white', 
            frame = 'black', 
            frame.double.line = T,
            legend.title.color = 'black',
            legend.bg.color = Greys[2], 
            legend.frame = T, 
            legend.text.color = 'black')+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .025),
    size = .65, 
    col = 'black', 
    align = 'left'
  )

tmap_save(emergency_resilience_indicators_map, 
          filename = 'png/draft_final/emergency_resilience_indicators_map.png', 
          width = 6, 
          height = 9)


 ## Rescue services

rural_order_df <- rural_res_agg %>% 
  left_join(rural_class, 
            by = c('UR8_2013_2014' = 
                     'URCLASS')) %>% 
  filter(description %in% emergency_cats) %>% 
  arrange(URNAME) %>% 
  distinct(URNAME) %>% 
  mutate(rural_order = c(6, 3, 1, 2, 7, 4, 8, 5))



alfa <- rural_res_agg %>% 
  left_join(rural_class, 
            by = c('UR8_2013_2014' = 
                   'URCLASS')) %>% 
  filter(description %in% medical_cats) %>% 
  select(description, n, 
         URNAME) %>% 
  group_by(description, URNAME) %>% 
  summarise(mean = mean(n, na.rm = T), 
         min = min(n), 
         max = max(n)) %>% 
  ungroup() %>% 
  left_join(rural_order_df) 

tango <- alfa %>% 
  group_split(description) %>% 
  map(., function(x){
    x %>% 
      arrange(rural_order)
  }) %>% 
  bind_rows()  

foxtrot <- tango  %>% 

  mutate_if(is.numeric, list(~round(., 2))) %>% 
  mutate_if(is.character, str_to_sentence) %>% 
  mutate_if(is.character, list(~str_wrap(.,15))) %>% 
  select(-min, -rural_order) 


foxtrot %>% 
  mutate(description = case_when(
    description == 'Special purpose
machinery and
equipment' ~ 'Special purpose machinery', 
    T~description
  ), description = case_when(
    description == 'Ambulance
and medical
transportation
services' ~ 'Ambulance services', 
    T~description
  )) %>% 
  filter(max > 1) %>% 
  mutate(flag = !duplicated(description), 
         description = case_when(
           flag == T ~ description, 
           T ~ ''
         )) %>% 
  select(-flag) %>% 
  set_names('Rescue Service Category', 
            '8-fold rural-urban classification',
            'Average services', 
            'Maximum services') %>% 
  kable(., 'latex', booktabs = T, caption = 'Emergency rescue indicators across rural and urban areas') %>% 
  kable_styling(full_width = F)




# Clusters of medical centres -------------

 # A map of emergency resilience indicators

# a map of resilience indicators

medical_cats <- 
  resilience_points_join_data %>% 
  count(description) %>% 
  pull(description) %>% 
  .[c(2, 3, 6, 12)] 



medical_resilience_points <- 
  resilience_points_join_data %>% 
  filter(description %in% medical_cats) %>% 
  mutate(Category = description)


medical_resilience_points %>% 
  count(description, sort = T)

medial_resilience_indicators_map <- 
  tm_shape(scotland_union)+
  tm_borders('black', 
             lwd = .125)+
  tm_fill(Greys[2])+
  tm_shape(medical_resilience_points)+
  tm_dots('Category', 
          size = .25,
          palette = '-Set3',
          border.col = 'white')+
  tm_layout(title = 'Emergency Medical Resilience Indicators in Scotland',
            title.color = 'black',
            bg.color = 'white', #Set1[2]
            frame = 'black', 
            frame.double.line = T,
            legend.title.color = 'black',
            legend.bg.color = Greys[2], 
            legend.frame = T, 
            legend.text.color = 'black')+
  tm_compass(type = '4star')+
  tm_scale_bar()+
  tm_credits(
    str_wrap(
      "(C) Crown Copyright and Database Right (2019) Ordnance Survey (Digimap License) This  material includes data licensed from PointX Database/Copyright(2019). Contains NRS data (c) Crown copyright and database right (2019)",
      30
    ),
    position = c(.025, .025),
    size = .65, 
    col = 'black', 
    align = 'left'
  )

tmap_save(medial_resilience_indicators_map, 
          filename = 'png/draft_final/medial_resilience_indicators_map.png', 
          width = 6, 
          height = 9)


 ## Rescue services

rural_order_df <- rural_res_agg %>% 
  left_join(rural_class, 
            by = c('UR8_2013_2014' = 
                     'URCLASS')) %>% 
  filter(description %in% emergency_cats) %>% 
  distinct(URNAME) %>% 
  mutate(rural_order = c(1, 2, 5, 6, 3, 4, 8, 7))




alfa <- rural_res_agg %>% 
  left_join(rural_class, 
            by = c('UR8_2013_2014' = 
                   'URCLASS')) %>% 
  filter(description %in% emergency_cats) %>% 
  select(description, n, 
         URNAME) %>% 
  group_by(description, URNAME) %>% 
  summarise(mean = mean(n, na.rm = T), 
         min = min(n), 
         max = max(n)) %>% 
  ungroup() %>% 
  left_join(rural_order_df) 

tango <- alfa %>% 
  group_split(description) %>% 
  map(., function(x){
    x %>% 
      arrange(rural_order)
  }) %>% 
  bind_rows()  

foxtrot <- tango  %>% 

  mutate_if(is.numeric, list(~round(., 2))) %>% 
  mutate_if(is.character, str_to_sentence) %>% 
  mutate_if(is.character, list(~str_wrap(.,15))) %>% 
  select(-min, -rural_order) 


foxtrot %>% 
  filter(max > 1) %>% 
  mutate(flag = !duplicated(description), 
         description = case_when(
           flag == T ~ description, 
           T ~ ''
         )) %>% 
  select(-flag) %>% 
  set_names('Rescue Service Category', 
            '8-fold rural-urban classification',
            'Average services', 
            'Maximum services') %>% 
  kable(., 'latex', booktabs = T, caption = 'Emergency rescue indicators across rural and urban areas') %>% 
  kable_styling(full_width = F)


# Resilience metric ------------

alfa <- resilience_points_join_data %>% 
  select(datazone = DataZone, description) %>% 
  mutate(datazone = as.character(datazone)) %>% 
  as_tibble()


datazone_8_fold <- dz_urban_rural %>% 
  select(datazone, 
         local_authority,
         rural = UR8_2013_2014)

metric_database <- alfa %>% 
  left_join(datazone_8_fold) %>% 
  select(-geometry)



# Resilience metric -------------


 # create a metric that counts the total number of places per rural datazone -- 
 # maybe create eight metrics, one for each rural classification
 # then compare rural areas to rural areas (apples to apples) when identifying hot spots and not spots

rural_datazone_n <- dz_urban_rural %>% 
  filter(UR8_2013_2014 == 8) %>% 
  count(local_authority, name = 'tot_dz')


# Community Centres
metric_rural_count <- metric_database %>% 
  add_count(local_authority,description, rural) %>% 
  select(-datazone) %>% 
  distinct() 

 # the most rural places in each local authority
very_remote_ru_community <-
  metric_rural_count %>%
  filter(str_detect(description, 
                    'halls and community')) %>% 
  filter(rural == 8)  %>% 
  left_join(rural_datazone_n) %>% 
  select(-description) %>% 
  mutate(ratio = n/tot_dz, 
         local_authority = 
           fct_reorder(local_authority, ratio))


community_ratio <- very_remote_ru_community %>% 
  ggplot(aes(local_authority, ratio))+
  geom_col(fill = Set2[2])+
  geom_text(aes(label = str_c('n=',n)),
            color = 'white', 
            nudge_y = -.075)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1, linetype = 2)+
  scale_y_continuous(breaks = seq(0,3,.25),
                     limits = c(0, 1.5))+
  theme_min_plus +
  theme(text = element_text(color = 'black'), 
        panel.grid = element_blank())+
  coord_flip()+
  labs(x = NULL, 
       y = 'Ratio of community halls and centres\nto number of total datazones', 
       subtitle = '*Community Centres and Halls', 
       caption = 'Number of community resilience indicators are shown in bar labels.\n*Only includes local authorities that have datazones categorised as very remote rural.')

ggsave(community_ratio, 
       filename = 'png/draft_final/community_ratio.png', 
       width = 7, 
       height = 5)

# Doctor's Surgeries
metric_rural_count <- metric_database %>% 
  add_count(local_authority,description, rural) %>% 
  select(-datazone) %>% 
  distinct() 

 # the most rural places in each local authority
very_remote_ru_doctor <-
  metric_rural_count %>%
  filter(str_detect(description, 
                    'doctor')) %>% 
  filter(rural == 8)  %>% 
  left_join(rural_datazone_n) %>% 
  select(-description) %>% 
  mutate(ratio = n/tot_dz, 
         local_authority = 
           fct_reorder(local_authority, ratio))


doctor_ratio <- very_remote_ru_doctor %>% 
  ggplot(aes(local_authority, ratio))+
  geom_col(fill = Set2[1])+
  geom_text(aes(label = str_c('n=',n)),
            color = 'white', 
            nudge_y = -.095)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1, linetype = 2)+
  scale_y_continuous(breaks = seq(0,3,.25))+
  theme_min_plus +
  theme(text = element_text(color = 'black'), 
        panel.grid = element_blank())+
  coord_flip()+
  labs(x = NULL, 
       y = 'Ratio of community halls and centres\nto number of total datazones', 
       subtitle = '*Doctor\'s Surgeries', 
       caption = 'Number of doctor\'s surgeries indicators are shown in bar labels.\n*Only includes local authorities that have datazones categorised as very remote rural.')


ggsave(doctor_ratio, 
    filename = 'png/draft_final/doctor_ratio.png', 
       width = 7, 
       height = 5)

# Doctor's Surgeries
metric_rural_count <- metric_database %>% 
  add_count(local_authority,description, rural) %>% 
  select(-datazone) %>% 
  distinct() 

 # the most rural places in each local authority
very_remote_ru_police <-
  metric_rural_count %>%
  filter(str_detect(description, 
                    'police')) %>% 
  filter(rural == 8)  %>% 
  left_join(rural_datazone_n) %>% 
  select(-description) %>% 
  mutate(ratio = n/tot_dz, 
         local_authority = 
           fct_reorder(local_authority, ratio))


police_ratio <- very_remote_ru_police %>% 
  ggplot(aes(local_authority, ratio))+
  geom_col(fill = Set2[3])+
  geom_text(aes(label = str_c('n=',n)),
            color = 'white', 
            nudge_y = -.05)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1, linetype = 2)+
  scale_y_continuous(breaks = seq(0,3,.25))+
  theme_min_plus +
  theme(text = element_text(color = 'black'), 
        panel.grid = element_blank())+
  coord_flip()+
  labs(x = NULL, 
       y = 'Ratio of police stations\nto number of total datazones', 
       subtitle = '*Police Stations', 
       caption = 'Number of police station indicators are shown in bar labels.\n*Only includes local authorities that have datazones categorised as very remote rural.')


ggsave(police_ratio, 
    filename = 'png/draft_final/police_ratio.png', 
       width = 7, 
       height = 5)







# Resilience metric urban vs rural ---------


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
           fct_reorder(local_authority, ratio)) %>% 
    select(1, 3, 4, 5) %>% 
    mutate_if(is.numeric, list(~round(., 2))) %>% 
    arrange(-ratio) %>% 
    kable('latex', booktabs = T, caption = 'Resilience metric in large urban areas')


la.s <- paste(c('Argyll', 'Edinburgh'))
SIMD_polygon %>% 
  as_tibble() %>%
  filter(str_detect(LAName, la.s)) %>% 
  group_by(LAName) %>% 
  summarise(median.size = median(Shape_Area))

































