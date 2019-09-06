library(tidyverse)
library(knitr)
library(kableExtra)
source('~/Downloads/all_functions.R')
source('scripts/Census/census_import.R')

file.edit('scripts/Census/census_import.R')

car_van <- census_read('car', grab = 5, read = T)[[1]]




dt <- mtcars %>% 
  slice(1:5)

kable(dt, "latex")


kable(dt, "latex", booktabs = T)