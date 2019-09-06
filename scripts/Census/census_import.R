library(tidyverse)
library(janitor)
load('data/poi_march_2019_pross.RData')
source('~/Downloads/all_functions.R')
options(warn = -1)

#######################
#######################
# Format Census Files #
#######################
#######################


# files <- path.finder('data/census_2011_so/raw')
#
# names_ls <- map(files, function(x){
#  a <-  clean_names(read_csv(x, skip=1, na="-"))[1]
#  str_remove(names(a), 'table_')
# })
#
# files_ls <- map(files, function(x){
#   read_csv(x, skip=4, na="-") %>%
#     mutate_all(list(~replace(., is.na(.), 0))) %>%
#     clean_names() %>%
#     rename(datazone = 1) %>%
#     slice(-1)
#   }) %>%
#   set_names(names_ls)
#
# map2(files_ls, names(files_ls),function(x, y){
#   write_csv(x, path  = paste0('data/census_2011_so/process/', y,'.csv'))
# })


# Function to find & read data ------------

census_read <- function(x, grab = NULL, read = F) {
  library(tidyverse, quietly = T)
  
  all_file_paths <- list.files('data/census_2011_so/process/')
  
  value <- paste0('_', x, '_')
  results <- grep(value,
                  all_file_paths,
                  value = T,
                  ignore.case = T)
  results_df <- tibble(files = results)
  
  ## print grab
  
  if (!is.null(grab)) {
    df <- results_df %>%
      slice(grab)
    print(df)
  } else {
    df_i <- results_df
    print(df_i)
  }
  
  if (read == T) {
    files <-  results_df %>%
      slice(grab) %>%
      pull(files)
    map(files, function(x) {
      read_csv(paste0('data/census_2011_so/process/', x))
    })
  }
  
}

# example
print('New function \'census_read\' will help with calling relevant census data')

x <- census_read('car', grab = 1, read = T)

x[[1]] %>%
  select(datazone,
         all_households,
         no_cars = 3) %>%
  transmute(datazone,
            prop_no_car = no_cars / all_households)




options(warn = 0)
