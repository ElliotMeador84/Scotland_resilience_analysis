library(tidyverse)
library(tmap)
library(mapview)
library(sf)
library(kable)
library(kableExtra)
library(scales)
source('~/Downloads/all_functions.R')
load('data/poi_march_2019_pross.RData')

# Make base map ------------

# Read simple features or layers from file or database
rural_index <-  st_read('data/SG_UrbanRural_2016.shp')

# create a rural-urban base map
# rural_index_simp <- st_simplify(rural_index)
# scotland_border_simp <- st_union(rural_index_simp)

rural_index_union <-
  st_read('geo_spatial/scotland_border/scotland_border_simp.shp')

tmap_style("col_blind")

quick_view <- function(.df) {
  .df %>%
    st_as_sf(coords = c("feature_easting",
                        "feature_northing")) %>%
    st_sf(crs = st_crs(rural_index_union)) %>%
    mapview()
}

word_sep <- function(x) {
  paste0('\\b(', x, ')\\b', collapse = '|')
}

indicator_clean <- function(.df) {
  .df %>%
    select(name,
           feature_easting,
           feature_northing,
           geographic_county,
           main_industry) %>%
    mutate(name = str_to_lower(name))
}


# Pull indicator df from POI database ---------

# emergency

emergency_terms <- word_sep(c('police',
                              'fire',
                              'rescue'))
emergency_sic <- word_sep(c('Government'))

emergency <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, emergency_terms),
         str_detect(main_industry, emergency_sic)) %>%
  mutate(category = 'emergency')



# community

community_terms <- word_sep(c('community centre',
                              'village hall',
                              'village centre'))
community_stop <- word_sep(c('nurse',
                             'nurses',
                             'school',
                             'bus',
                             'pitch',
                             'sports hall'))
community_sic <- word_sep(c(''))

community <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, community_terms),!str_detect(name, community_stop)) %>%
  mutate(category = 'community')



# YMCA

ymca_terms <- word_sep(c('ymca'))

ymca <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, ymca_terms)) %>%
  mutate(category = 'other')



# sports hall

sport_hall_terms <- word_sep(c('sports hall'))

sports_hall <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, sport_hall_terms)) %>%
  mutate(category = 'sports hall')


# primary school

school_terms <- word_sep(c('primary school',
                           'high school',
                           'secondary school'))
school_stop <- word_sep(c('bus stop',
                          'pitch',
                          'sports hall'))
school_stop_regx <- c('\\(')

schools <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(
    str_detect(name, school_terms),!str_detect(name, school_stop),!str_detect(name, school_stop_regx)
  ) %>%
  mutate(category = 'schools')




# Religious centres

# don't include these just yet
religous_term <- word_sep(c('church',
                            'mosque',
                            'hebrew'))

religous_stop <- word_sep(c('cemetery',
                            'bus stop',
                            'remains of'))

religous <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, religous_term),!str_detect(name, religous_stop)) %>%
  mutate(category = 'religous')



# medical


med_terms <- word_sep(c(
  'nhs',
  'medical clinic',
  'hospital',
  'emergency room',
  'infirmary'
))
med_stop <- word_sep(c('bus stop',
                       'herb'))

medical_care <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, med_terms),!str_detect(name, med_stop)) %>%
  mutate(category = 'medical')

# post office
# not including this as of now

post_terms <- word_sep(c('post office'))
post_stop <- word_sep(c('bus stop',
                        'herb'))

post_office <- poi_march_2019_pross %>%
  indicator_clean() %>%
  filter(str_detect(name, post_terms)) %>%
  mutate(category = 'post office')

# Merge databases ---------


# merge
indicator_df <- bind_rows(sports_hall,
                          schools,
                          # religous,
                          community,
                          emergency,
                          medical_care,
                          ymca)


extract_new <- function(.df, .new_col, .search_word) {
  .df %>%
    extract('name',
            .new_col,
            word_sep(.search_word),
            remove = F)
}



indicator_final <- indicator_df  %>%
  extract_new('community', paste0(
    c('community centre',
      'village hall',
      'village centre'),
    collapse = '|'
  )) %>%
  extract_new('sports_hall', c('sports hall')) %>%
  extract_new('medical', paste0(
    c(
      'nhs',
      'medical clinic',
      'hospital',
      'emergency room',
      'infirmary'
    ),
    collapse = '|'
  )) %>%
  # extract_new('religous', paste0(c('church',
  #                                  'mosque',
  #                                  'infirmary',
  #                                  'nhs'), collapse = '|')) %>%
  extract_new('emergency', paste0(c('police',
                                    'fire',
                                    'rescue'), collapse = '|')) %>%
  extract_new('school', paste0(
    c('primary school',
      'high school',
      'secondary school'),
    collapse = '|'
  )) %>%
  mutate_if(is.character, list(~ str_replace_na(., ''))) %>%
  unite('label',
        c('community',
          'medical',
          'sports_hall',
          # 'religous',
          'emergency',
          'school'),
        sep = ';') %>%
  mutate(
    label = str_replace_all(label, ";+", "; "),
    label = str_replace_all(label, "; $", ""),
    label = str_replace_all(label, '^;', ''),
    label = str_squish(label)
  ) %>%
  rename(local_authority = geographic_county) %>%
  clean_council(local_authority)


save(indicator_final,
     file = 'data/indicator_final.RData')


(indic_n <- indicator_final %>%
    nrow() %>%
    comma())

indicator_final %>%
  count(category, sort = T) %>%
  mutate(
    category = str_to_title(category),
    Percent = percent(n / sum(n)),
    n = comma(n)
  ) %>%
  set_names(str_to_title(names(.))) %>%
  select(1, 3, 2) %>%
  kable(caption = 'Resilience Categories',
        align = c('r', 'c', 'c')) %>%
  kable_styling('striped',
                full_width = F) %>%
  column_spec(column = c(1, 2),
              border_right = T) 




indicator_final %>%
  mutate(label = fct_lump(label, 19)) %>%
  count(label, sort = T) %>%
  mutate(label = str_to_title(label),
         Percent = percent(n / sum(n)),
         n = comma(n)) %>%
  set_names(str_to_title(names(.))) %>%
  select(1, 3, 2) %>%
  kable(caption = 'Resilience Sub-Categories',
        align = c('r', 'c', 'c')) %>%
  kable_styling('striped',
                full_width = F) %>%
  column_spec(column = c(1, 2),
              border_right = T) %>%
  footnote(paste('There are', indic_n, 'places in the database.'))




























