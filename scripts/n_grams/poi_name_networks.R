library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)
source('~/all_functions.R')
load('~/R/Scotland_Resilience_NCR/data/poi_2959457/poi_march_2019_pross.RData')


source('~/R/Scotland_Resilience_NCR/scripts/theme_min_plus.R')



poi <- poi_march_2019_pross
# Alfa ---------------



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



actual <- function(x) {
  paste0('\\b', x, '\\b')
}



(Community_village_halls <- poi  %>%
  filter(!str_detect(name, actual('Bus'))) %>% 
  poi_word_graph(word_search = actual('Community')) %>%
  top_n(7, n) %>%
  bind_rows(poi  %>%
              filter(!str_detect(name, actual('Bus'))) %>% 
              poi_word_graph(word_search = actual('Hall')) %>%
              top_n(7, n)) %>% 
  bind_rows(poi  %>%
              filter(!str_detect(name, actual('Bus'))) %>% 
              poi_word_graph(word_search = actual('Village')) %>%
              top_n(7, n)) %>% 
  unite('key', c('word', 'lead.word')) %>% 
  group_by(key) %>% 
  summarise(n = sum(n)) %>% 
  separate(key, c('word', 'lead.word')) %>% 
  as_tbl_graph() %>%
  ggraph(., layout = 'stress') +
  geom_edge_fan(
    aes(color = n),
    width = 2.25,
    arrow = arrow(type = 'closed',
                  length = unit(0.25, 'cm')),
    start_cap = circle(13.5, 'mm'),
    end_cap = circle(12.5, 'mm'),
    show.legend = T
  ) +
  geom_node_text(aes(label = str_to_title(name)),
                 size = 5) +
  scale_edge_color_gradient(
      low = Spectral_n(1),
      high = Spectral_n(11),
      name = 'Number of times word pairs\nappear in place names', 
      labels = comma) +
  theme_min_plus +
  theme(axis.text = element_text(face = 'bold', 
                                 color = 'black'),
        legend.title = element_text(size = 14), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 17.5))+  
  labs(x = NULL,
       y = NULL, 
       title = 'N-grams to help identify search terms in the POI database', 
       subtitle = 'Using \'Hall\' as the starting search term'))

ggsave(Community_village_halls, 
       filename = 'mini_report/Community_village_halls.png', 
       width = 12, 
       height = 11)



