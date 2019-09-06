library(tidyverse)
library(scales)
library(viridis)
library(ggforce)
library(rgdal)
source('~/all_functions.R')
source('~/R/Scotland_Resilience_NCR/scripts/Census/census_import.R')
load('data/poi_march_2019_pross.RData')

# Read distance to community centre -------------

resilience_data <- resilience_poi@data %>%
  as_tibble() %>%
  mutate_if(is.factor, as.character)

# Merge with Scottish Census -----------------

## what about people who don't have a car
car <- census_read('car', grab = 1, read = T)

car_df <- car[[1]] %>%
  select(datazone,
         all_households,
         no_cars = 3) %>%
  transmute(datazone,
            prop_no_car = no_cars / all_households)

community_car <- car_df %>%
  right_join(resilience_data %>%
               select(datazone, dist_km), by = "datazone") %>%
  group_by(datazone) %>%
  mutate(dist_km_m =
           median(dist_km, na.rm = T)) %>%
  distinct(datazone, prop_no_car, dist_km_m) %>%
  ungroup()


# Merge with Scottish Household Survey 2016 ----------------

source('~/R/Scotland_Resilience_NCR/scripts/SHS_2016/shs2016_social_public.R')

load('~/R/SIMD/data/SIMD_2016/SIMD_2016_process.RData')

council_dz_lookup <- SIMD_2016_process %>%
  select(datazone, council_area) %>%
  clean_council(council_area)

shs2016_dz_neigh_emergency <- shs2016_resilience %>%
  clean_council(local_authority) %>%
  left_join(council_dz_lookup,
            by = c('local_authority' = 'council_area')) %>%
  select(datazone, neighborhood_emergency) %>%
  drop_na(datazone) %>%
  mutate(
    neighborhood_emergency_n = case_when(
      neighborhood_emergency == 'Strongly agree' ~ 5,
      neighborhood_emergency == 'Tend to agree' ~ 4,
      neighborhood_emergency == 'Neither agree nor disagree' ~ 3,
      neighborhood_emergency == 'Tend to disagree' ~ 2,
      neighborhood_emergency == 'Strongly disagree' ~ 1
    )
  ) %>%
  group_by(datazone) %>%
  summarise(ave_emergency =
              mean(neighborhood_emergency_n, na.rm = T)) %>%
  ungroup()

community_car_emergency <- community_car %>%
  left_join(shs2016_dz_neigh_emergency)




# Create a single plot that pulls it together ----------

community_car_df <- community_car_emergency %>%
  drop_na() %>%
  mutate(
    ave_emer_facet = cut_number(
      ave_emergency,
      4,
      labels = c('Less likely',
                 'Somewhat likely',
                 'Likely',
                 'Most likely')
    ),
    ave_emer_facet = str_c(ave_emer_facet, ' to help in an emergency'),
    ave_emer_facet = fct_relevel(ave_emer_facet, c(
      str_c('Less likely', ' to help in an emergency'),
      str_c('Somewhat likely', ' to help in an emergency'),
      str_c('Likely', ' to help in an emergency'),
      str_c('Most likely', ' to help in an emergency')
    ))
  )


df_grey <- community_car_df %>%
  transmute(x = dist_km_m,
            y = prop_no_car)


(
  comm_car_black_grey <-  community_car_df %>%
    ggplot(aes(dist_km_m, prop_no_car)) +
    geom_jitter(
      data = df_grey,
      aes(x, y),
      color = 'grey',
      size = 1,
      show.legend = F
    ) +
    geom_jitter(
      color = 'black',
      size = 1,
      alpha = .85,
      show.legend = F
    ) +
    geom_smooth(color = Greys[2]) +
    scale_x_log10() +
    scale_y_continuous(label = percent) +
    facet_wrap( ~ ave_emer_facet) +
    theme_classic() +
    theme(
      legend.position = 'bottom',
      legend.background = element_rect(fill = 'transparent'),
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      plot.title = element_text(size = 20),
      axis.title.y = element_text(angle = 0,
                                  hjust = 1),
      strip.background = element_blank()
    ) +
    labs(
      x = 'Median Distance (km) to Community Development Centre\nAs identified in the Points of Interest database\n(POI Database)',
      y = 'Percentage of\nhouseholds with NO\naccess to a car or \nvan (Census 2010)\n\n\n\nBlack points\nshow points\nwithin each\n category\n\n\n\nGrey points\nshow all\ndata points',
      title = '\nDatazones that are further from Community Development Centres\nare more likely to have access to a car or van\n',
      caption = 'Data are from the POI database (2019), Scottish Census (2010) and Scottish Household Survey (2016)'
    )
)



ggsave(
  comm_car_black_grey,
  filename = 'mini_report/comm_car_black_grey.png',
  width = 12,
  height = 8
)






# Same plot with viris coloring ----------

(
  comm_car <- sample(community_car_emergency) %>%
    ggplot(aes(dist_km_m, prop_no_car)) +
    geom_jitter(aes(color = ave_emergency),
                size = 2.25,
                alpha = .65) +
    geom_smooth(color = Greys[2]) +
    scale_x_log10() +
    scale_y_continuous(label = percent) +
    scale_color_viridis(
      option = 'magma',
      name = 'Would you help your\nneighbour in an emergency?\n(SHS 2016)',
      breaks = c(4.3, 4.8),
      labels = c('Less likely\nto help',
                 'More likely\nto help')
    ) +
    theme_classic() +
    theme(
      legend.position = c(.75, .75),
      legend.background = element_rect(fill = 'transparent'),
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      plot.title = element_text(size = 20),
      axis.title.y = element_text(angle = 0, hjust = 1)
    ) +
    labs(
      x = 'Median Distance (km) to Community Development Centre\nAs identified in the Points of Interest database\n(POI Database)',
      y = 'Percentage of\nhouseholds with NO\naccess to a car or \nvan (Census 2010)',
      title = '\nDatazones that are further from Community Development Centres\nare more likely to have access to a car or van\n',
      caption = 'Data are from the POI database (2019), Scottish Census (2010) and Scottish Household Survey (2016)'
    )
)



ggsave(comm_car,
       filename = 'mini_report/comm_car.png',
       width = 12,
       height = 8)






df <- community_car_emergency %>% 
  select(-datazone) %>% 
  mutate(dist_km_m = log(dist_km_m), 
         ave_emergency = cut_number(ave_emergency, 4))


lm(dist_km_m ~ prop_no_car, data = df) %>% 
  summary() %>% 
  broom::tidy() %>% 
  mutate_if(is.numeric, list(~round(., 2))) %>% 
  View()

lm(dist_km_m ~ ., data = df) %>% 
  summary() %>% 
  broom::tidy() %>% 
  mutate_if(is.numeric, list(~round(., 2))) %>% 
  View()













