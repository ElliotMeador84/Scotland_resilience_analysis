library(tidyverse)
source('~/Downloads/all_functions.R')

theme_min_plus <- theme_minimal()+
  theme(text = element_text(color = 'black'), 
        panel.grid.minor = element_blank(), 
        plot.margin = margin(.75, .75, .75, .75, 'cm'))


ru_ur_cols <- Set3_n(8)


names(ru_ur_cols) <- 1:8


