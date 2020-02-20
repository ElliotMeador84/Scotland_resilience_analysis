library(tidyverse)
library(janitor)
library(sf)
library(sp)
library(scales)
library(glue)
## working directories to save time

source('/Users/emeador/OneDrive - SRUC/all_functions.R')

project_wd <- getwd()
data_repo_wd <- '/Users/emeador/OneDrive - SRUC/Data'



## Read in geo-data

# Settlements
# A settlement is defined to be a group of high density postcodes whose combined population rounds to 500 people or more. They are separated by low density postcodes.

# Localities
# Localities correspond to the more recognisable towns and cities of Scotland which can be found within settlements. They also have a minimum rounded population of 500 people or more.

setwd(data_repo_wd)

localities <-
  st_read('geographic/boundaries/Scotland_localities_2016/Localities2016_MHW.shp')

settlements <-
  st_read(
    'geographic/boundaries/settlements/Settlements_2016_boundaries/Settlements2016_MHW.shp'
  )

settlement_pop <-
  read_csv('population/scotland_settlement_population_2016.csv')

la_border <-
  st_read('geographic/boundaries/local_authority_border.gpkg')

rural_urban_sf <-
  st_read('geographic/rural-urban/SG_UrbanRural_2016.gpkg')

## resilience points
resilience_points_df <-
  read_csv("resilience/resilience_points_df.csv")





res_east_north <- resilience_points_df %>%
  select(feature_easting, feature_northing)



east_north <- SpatialPoints(res_east_north,
                            proj4string = CRS("+init=epsg:27700"))

east_north_sf <- st_as_sf(east_north)

resilience_points_sf <-
  bind_cols(resilience_points_df, east_north_sf) %>%
  st_as_sf()


# long_lat <- spTransform(east_north, CRS("+init=epsg:4326"))

setwd(project_wd)



### Combine settlements and localities

settlements$type <- 'Settlements (n = 519)'
localities$type <- 'Localities (n = 655)'

settlement_locality <- rbind(settlements,
                             localities) %>%
  distinct(name, .keep_all = T)

### plot/maps ###

localities_blue <-
  ggplot() +
  geom_sf(
    data = la_border,
    fill = 'white',
    color = 'grey80',
    size = .1
  ) +
  geom_sf(
    data = settlement_locality,
    aes(fill  = type),
    color = 'black',
    size = 0.01,
    show.legend = F
  ) +
  scale_fill_brewer(palette = 'Spectral',
                    direction = -1) +
  facet_grid( ~ type) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  labs(title = 'Scottish Localities')


if (!exists('pdf/localities.pdf')) {
  ggsave(
    localities_blue,
    filename = 'pdf/localities.pdf',
    width = 8,
    height = 11
  )
}


# system('open /Users/emeador/Documents/R/Scotland_resilience_analysis/pdf/localities.pdf')



### Join settlemens/localities and resilience points

# create a uniform CRS
resilience_points_sf <-
  st_transform(resilience_points_sf,
               st_crs(settlements))


resilience_settlements <-
  st_join(resilience_points_sf,
          settlements,
          join = st_intersects)



resilience_settlements_df <- resilience_settlements %>%
  as_tibble() %>%
  rename(
    indicator = name.x,
    indicator_reference = reference_number,
    settlement = name.y,
    settlement_length = Shape_Leng,
    settlement_area = Shape_Area,
    settlement_code = code,
    settlement_id = OBJECTID
  ) %>%
  select(-type,-geometry) %>%
  left_join(settlement_pop, by = 'settlement')



resilience_score <- resilience_settlements_df %>%
  mutate_if(is.factor, as.character) %>%
  add_count(settlement, description) %>%
  drop_na(settlement) %>%
  arrange(settlement) %>%
  distinct(settlement,
           description,
           n,
           settlement_area,
           population_2016) %>%
  add_count(settlement,
            name = 'total_resilience_n') %>%
  mutate(resilience_ratio = total_resilience_n / (settlement_area * population_2016))




resilience_score_df <- resilience_score %>%
  arrange(desc(resilience_ratio)) %>%
  distinct(settlement, resilience_ratio)


settlement_centroids <- st_centroid(settlements)

settlement_resilience_sf <- settlement_centroids %>%
  left_join(resilience_score_df,
            by = c('name' = 'settlement')) %>%
  mutate(resilience_ratio_c = cut_number(resilience_ratio,
                                         4,
                                         labels = c('Lowest',
                                                    'Low',
                                                    'High',
                                                    'Higest'))) %>%
  drop_na(resilience_ratio_c)

settlement_resilience_sf %>%
  as_tibble() %>%
  ggplot(aes(log(resilience_ratio))) +
  geom_histogram()



res_median <- median(resilience_score_df$resilience_ratio,
                     na.rm = T) + .000000017225

resilience_score_plot <- ggplot() +
  geom_sf(
    data = la_border,
    fill = 'white',
    color = 'grey80',
    size = .1
  ) +
  geom_sf(data = settlement_resilience_sf,
          aes(color = resilience_ratio_c),
          show.legend = F) +
  scale_color_brewer(palette = 'Spectral') +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'skyblue',
                                    color = 'black'),
    panel.grid = element_blank()
  ) +
  facet_wrap( ~ resilience_ratio_c) +
  labs(
    title = 'Resilience scores for each settlement in Scotland',
    subtitle = str_wrap(
      'A settlement is defined to be a group of high density postcodes whose combined population rounds to 500 people or more. They are separated by low density postcodes.',
      65
    ),
    size = 'Resilience score',
    color = 'Resilience score'
  )



ggsave(
  resilience_score_plot,
  filename = 'pdf/resilience_ratio.pdf',
  width = 11,
  height = 8
)



system(
  'open /Users/emeador/Documents/R/Scotland_resilience_analysis/pdf/resilience_ratio.pdf'
)



### Add rural urban


rural_urban_sf <- st_transform(rural_urban_sf,
                               crs = st_crs(settlement_resilience_sf))

settlement_resilience_r_ur_sf <-
  st_join(settlement_resilience_sf,
          rural_urban_sf,
          join = st_intersects)


settlement_rural_urban <-
  settlement_resilience_r_ur_sf %>%
  as_tibble() %>%
  select(name, ru_ur_8 = UR8FOLD)



settlement_resilience_sf <- settlement_resilience_sf %>%
  left_join(settlement_rural_urban, by = 'name')



(
  resilience_boxplot <- settlement_resilience_sf %>%
    as_tibble() %>%
    drop_na() %>%
    ggplot(aes(as.factor(ru_ur_8), resilience_ratio)) +
    geom_boxplot(aes(fill = as.factor(ru_ur_8)),
                 show.legend = F) +
    scale_y_log10() +
    scale_fill_brewer(palette = 'Spectral') +
    theme_minimal() +
    theme(plot.margin = margin(2, 2, 2, 2, 'cm')) +
    labs(
      title  = str_wrap(
        'Overall, remote places tend to have higher resilince scores that offsets their isolation',
        75
      ),
      x = '8-Fold Rural-Urban\n(Scottish Government)',
      y = 'Resilience score\nNOTE: Log scale'
    )
)


ggsave(
  resilience_boxplot,
  filename = 'png/resilience_boxplot.png',
  width = 8,
  height = 5
)



system(
  'open /Users/emeador/Documents/R/Scotland_resilience_analysis/png/resilience_boxplot.png'
)



settlement_rural_urban <- settlement_resilience_sf %>%
  as_tibble() %>%
  select(name, ru_ur_8)


res_sett_trunc <- resilience_settlements_df %>%
  mutate_if(is.factor, as.character) %>%
  add_count(settlement, description) %>%
  drop_na(settlement) %>%
  arrange(settlement) %>%
  left_join(settlement_rural_urban,
            by =  c('settlement' = 'name')) %>%
  select(ru_ur_8,
         settlement,
         description,
         population_2016)

## show model results
lm_eqn <- function(df) {
  m <- lm(n ~ population_2016 + ru_ur_8,
          data = res_trunc_lm) 
  
  eq <-
    substitute(
      italic(y) == a + b %.% italic(x) * "," ~  ~ italic(r) ^ 2 ~ "=" ~ r2,
      list(
        a = format(unname(coef(m)[1]), digits = 2),
        b = format(unname(coef(m)[2]), digits = 2),
        r2 = format(summary(m)$r.squared, digits = 3)
      )
    )
  as.character(as.expression(eq))
  
}


res_trunc_lm <- res_sett_trunc %>%
  add_count(settlement) %>%
  distinct(ru_ur_8, n, population_2016)


equation_lm <- lm_eqn(res_trunc_lm)

  
res_sett_trunc %>%
  add_count(settlement) %>%
  distinct(ru_ur_8, n, population_2016) %>%
  drop_na() %>% 
  ggplot(aes(population_2016, n)) +
  geom_jitter(aes(color = as.factor(ru_ur_8)), 
              size = 2.5) +
  geom_smooth(color = 'grey60', 
              linetype = 4,
              method = 'lm',
              alpha = .125) +
  scale_color_brewer(palette = 'Spectral')+
  scale_x_log10(label = comma) +
  scale_y_log10() +
  theme_minimal() +
  theme(plot.margin = margin(2,2,2,2, 'cm'))+
  labs(title = 'Linear association between number of indicators and population size', 
       x = 'Settlement population', 
       y = 'Number of resilience indicators', 
       color = 'Scottish Gov.\nRural-Urban class.', 
       caption = 'We can use this to argue that some places are much more resilint ~~ or the break a clear trend.')+
  annotate(geom = 'text', 
           x = 9500, 
           y = 500, 
           label = equation_lm, parse = T)+
  annotate('label', 
           x = 650, 
           y = 1.25, 
           label = 'Less resilient')+
  annotate('label', 
           x = 650, 
           y = 15, 
           label = 'More resilient')


# 
resilience_median <- res_sett_trunc %>%
  add_count(settlement, 
            description, 
            name = 'desc_n') %>% 
  mutate(
    description_ratio = 
              population_2016/desc_n) %>%
  distinct(ru_ur_8, description,
           description_ratio) %>% 
  group_by(ru_ur_8, description) %>% 
  summarise(mean_ratio = 
              mean(description_ratio, 
                     na.rm = T)) %>% 
  ungroup()

legend_label <- glue('{comma(round(exp(c(7:12))))}:1')

resilience_median %>% 
  mutate(description = fct_reorder(description, 
                                   mean_ratio, 
                                   .desc = T)) %>% 
  ggplot(aes(ru_ur_8, description))+
  geom_tile(aes(fill = log(mean_ratio)))+
  scale_fill_gradientn(colors = rev(Spectral), 
    name = 'Resilience Indicators\nRatio', 
    breaks = seq(7, 12, 1), 
    labels = legend_label)+
  scale_x_continuous(breaks = 1:8, 
                     expand = c(0,0))+
  scale_y_discrete(expand = c(0, 0), 
                   labels = str_to_sentence)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = 'grey', 
                                        color = 'grey'), 
        panel.grid = element_blank(), 
        legend.position = 'bottom', 
        legend.text = element_text(angle  = 45, 
                                   hjust = 1), 
        plot.margin = margin(1,1,1,1, 'cm'), 
        axis.text.y = element_text(size = 12.5, 
                                   color = 'black'))+ 
  guides(fill = guide_colorbar(reverse=T))+
  labs(x = 'Rural/Urban Class.',
       y = NULL)


































