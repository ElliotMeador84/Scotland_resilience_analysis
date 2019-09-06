library(tidyverse)
library(broom)
library(scales)
library(tidytext)
load('data/poi_2959457/poi_march_2019_pross.RData')
source('~/all_functions.R')
load('data/scotland_la_fortify.RData')

source('scripts/import/POI_March_2019_import_merge.R')
# Temp --------------------


# Datazones are classified as resilient if they contain a 'resilience indicator.' Resilience indicators can be a variety of things and establishments. They will be identified through ground-level 'truthing.' Some datazones will have many different indicators. Therefore, we will count the number of total resilience indicators in datazones.

# As we cannot identify all resilience indicators accross Scotland, a typology classification model will be created to identify potential resilience indicators from a database of all locations. 

# What we want to know is: Can we identify patterns that differentiate places with resilient indicators from other places that do not have resilience indators ... so called 'hot' spots and 'not' spots. 



poi_march_2019_pross_lc <- poi_march_2019_pross %>% 
  mutate_if(is.character, list(~str_to_lower(.))) 

indicator <- poi_march_2019_pross_lc %>%  
  mutate(community_indicator = case_when(
    str_detect(name, 'community') ~ 1, 
    T ~ 0
  ), 
        fire_indicator = case_when(
          str_detect(name, 'fire station') ~ 1, 
          T ~ 0
        ), 
        police_indicator = case_when(
    str_detect(name, 'police station') ~ 1, 
    T ~ 0
  )) %>% 
  select(contains('indicator'), 
         datazone, 
         contains('overall'))

indicator_df <- indicator %>% 
  gather(key, value, -datazone, -overall_simd16_rank) %>% 
  na.omit() %>%  
  group_by(datazone) %>% 
  mutate(tot = sum(value)) %>% 
  ungroup() %>% 
  select(overall_simd16_rank, tot) %>% 
  distinct() %>% 
  arrange(desc(tot))

indicator_df %>% 
  ggplot(aes(tot, overall_simd16_rank, group = tot))+
  geom_boxplot()+
  scale_x_continuous(breaks = 0:10, 
                     labels = 0:10)+
  labs(title = 'Overall SIMD Rank & Total # Resilience Indicators', 
       subtitle = 'Counts of \'community\', \'fire stations\' & \'police stations\'', 
       x = 'Total # of Indicators',
       y = 'SIMD\nRank')+
  theme_classic()+
  theme(axis.title.y = element_text(angle = 0), 
        plot.margin = margin(1,1,1,1, 'cm'))

glm(tot ~ ., data = indicator_df, 
    family = poisson(link = "log")) %>% 
  tidy() 












