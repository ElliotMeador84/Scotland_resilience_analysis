library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(kable)
library(kableExtra)
library(scales)
source('~/Downloads/all_functions.R')



df <- tribble(~source, ~target, 
              'law', 'statesmanship', 
              'statesmanship', 'law', 
              'literature', 'statesmanship', 
              'literature', 'scientific method', 
              'statesmanship', 'scientific method', 
              'scientific method', 'mechanics', 
              'scientific method', 'child psychology',
              'child psychology', 'social research',
              'social research', 'disaster risk reduction',
              'disaster risk reduction', 'climate change adaption',
              'mechanics', 'manufacturing', 
              'mechanics', 'ecology', 
              'ecology', 'anthropology', 
              'ecology', 'disaster risk reduction', 
              'ecology', 'management (adaptive)', 
              'ecology', 'sustainability science', 
              'anthropology', 'social research', 
              'anthropology', 'child psychology')










g <-  df %>% 
  graph_from_data_frame()




name_year <- tibble(name = V(g)$name) %>% 
  mutate(year = c(
    35,
    1529, 
    500, 
    1625, 
    1865, 
    1965, 
    2000, 
    1859, 
    1973, 
    1940, 
    2010, 
    1945, 
    2000, 
    2005
  ))



theme_lab <- function(x, y = 10){
  x <- str_to_title(x)
  x <- str_wrap(x, y)
}

layout_res <- layout_with_stress(g) %>% 
    layout_rotate(90) %>% 
  as_tibble() %>% 
  set_names('x', 'y')





g %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  left_join(name_year) %>% 
  ggraph(., layout = 'manual', 
         node.position = layout_res)+
  geom_edge_arc(curvature = .15)+
  geom_node_point(aes(color = year), 
                  size = 25)+
  geom_node_text(aes(label = theme_lab(name)))+
  scale_color_gradient2(low = 'tomato3', 
                        high = 'skyblue', 
                        mid = 'tomato', 
                        midpoint = 1050)+
  theme_graph()















