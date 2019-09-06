library(tidyverse)
library(scales)
library(sp)
library(tidytext)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(viridis)
load('data/poi_march_2019_pross.RData')
load('data/scotland_la_fortify.RData')
source('~/all_functions.R')

# Quick maps -------------

# pull the potential community development centres
community <- poi_march_2019_pross %>%
  mutate_if(is.character, list(~ str_to_lower(.))) %>%
  filter(
    str_detect(name, 'community'),
    str_detect(sub_activity, 'other'),
    sub_activity == 'other'
  ) %>%
  select(long,
         lat,
         name,
         sub_activity)


# prep the Scotland LA for longitude and latitude
dat <- select(scotland_la_fortify,
              long,
              lat)

east_north <- SpatialPoints(dat,
                            proj4string = CRS("+init=epsg:27700"))

long_lat <- spTransform(east_north,
                        CRS("+init=epsg:4326")) %>%
  as_tibble() %>%
  purrr::set_names('long', 'lat')

# merge files abck together
scotland_la_fortify <- long_lat %>%
  bind_cols(scotland_la_fortify %>%
              select(-long, -lat))


# make the community development plots
(
  community_plot <- ggplot() +
    geom_polygon(
      data = scotland_la_fortify,
      aes(long,
          lat,
          group = group),
      fill = Greys[1],
      color = Greys[3], 
      size = .25
    ) +
    geom_point(data = community,
               aes(long,
                   lat),
               size = .5) +
    coord_map() +
    labs(title = 'Name has \'Community\' in the title', 
         x = NULL, 
         y = NULL) +
    theme_light() +
    theme(
      panel.background =
        element_rect(fill = Blues[3]),
      panel.grid.major =
        element_line(color = Blues[1])
    )
)

ggsave(community_plot, 
       filename = 'pdf/community_map.pdf', 
       height = 11, 
       width = 7)


# Word gram to accompany map ------------------
# from
# file.edit('scripts/n_grams/poi_name_networks.R')

poi_word_graph <- function(df, word_search = 'Development') {
  df %>%
    select(name) %>%
    filter(str_detect(name, word_search)) %>%
    unnest_tokens(word, name) %>%
    anti_join(stop_words) %>%
    mutate(lead.word = lead(word)) %>%
    unite(pair, word, lead.word, remove = F) %>%
    add_count(pair) %>%
    mutate(cumulative = cume_dist(n)) %>%
    distinct(word, lead.word, n, cumulative)
}


# Table_graph -----------
community_tble_g <- community %>%
  poi_word_graph('community') %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(id = 1:max(row_number()),
         degree = centrality_degree()) %>%
  top_n(9)


# GGraph plot -------------


(community_g <- community_tble_g  %>%
  ggraph(., layout = 'stress') +
  geom_edge_arc(
    aes(color = log(n),
        width = log(n)),
    arrow = arrow(type = 'closed',
                  length = unit(0.25, 'cm')),
    start_cap = circle(7.5, 'mm'),
    end_cap = circle(7.5, 'mm'),
    curvature = .15,
    show.legend = T
  ) +
  geom_node_text(aes(label = str_to_title(name)),
                 size = 3) +
  scale_edge_color_gradient2(
    low = Blues[2],
    high = Blues[8],
    midpoint = 1,
    mid = Greys[3],
    name = 
      'Number of times\nword pairs appear',
    labels = seq(200, 800, 200)
  ) +
  scale_edge_width(range = c(1, 2), guide = F) +
  theme_min_plus +
  labs(x = NULL,
       y = NULL)+
  coord_equal())


ggsave(community_g, 
       file = 'pdf/community_g.pdf', 
       width = 8, 
       height = 8)




emergency <- paste(c('police station', 
                     'fire station'), 
                   collapse = '|')

fire_police <- poi_march_2019_pross %>% 
  mutate(name = str_to_lower(name)) %>% 
  filter(str_detect(name, emergency), 
         !str_detect(name, 'bus stop')) %>%
  select(long,
         lat,
         name,
         sub_activity) %>% 
  mutate(type = case_when(
    str_detect(name, 'fire') ~ 'Fire', 
    T ~ 'Police'
  ))



# prep the Scotland LA for longitude and latitude
dat <- select(scotland_la_fortify,
              long,
              lat)

east_north <- SpatialPoints(dat,
                            proj4string = CRS("+init=epsg:27700"))

long_lat <- spTransform(east_north,
                        CRS("+init=epsg:4326")) %>%
  as_tibble() %>%
  purrr::set_names('long', 'lat')

# merge files abck together
scotland_la_fortify <- long_lat %>%
  bind_cols(scotland_la_fortify %>%
              select(-long, -lat))


# make the fire_police plots
(
  fire_police_plot <- ggplot() +
    geom_polygon(
      data = scotland_la_fortify,
      aes(long,
          lat,
          group = group),
      fill = Greys[1],
      color = Greys[3], 
      size = .25
    ) +
    geom_point(data = fire_police,
               aes(long,
                   lat, 
                   color = type),
               size = 1.15) +
    coord_map() +
    scale_color_manual(values = c(Oranges[7], Blues[7]), 
                       name = 'Station Type', 
                       labels = c('Fire', 'Police'))+
    labs(title = 
           'Name has \'Fire\' or \'Police\' in the title', 
         x = NULL, 
         y = NULL) +
    theme_light() +
    theme(
      panel.background =
        element_rect(fill = Blues[3]),
      panel.grid.major =
        element_line(color = Blues[1])
    )
)





