library(tidyverse)
library(ggpubr)
library(scales)
source('scripts/theme_min_plus.R')
source('~/Downloads/all_functions.R')
load('data/resas_rural.RData')

shs2016_social_public <-
 suppressWarnings(suppressMessages( read_delim(
    "data/UKDA-8333-tab/tab/shs2016_social_public.tab",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE, 
  )))

# Create SHS tribbles -------------

council <- tribble(
  ~council, ~local_authority, 
'4', 'West Dumbartonshire',
'B', 'Aberdeenshire',
'S', 'Midlothian',
'T', 'Moray',
'O', 'Fife',
'1', 'South Ayrshire',
'K', 'East Lothian',
'J', 'East Dumbartonshire',
'2', 'South Lanarkshire',
'6', 'Eilean Siar',
'M', 'Edinburgh City',
'Y', 'Renfrewshire',
'Q', 'Highland',
'3', 'Stirling',
'W', 'Orkney',
'U', 'North Ayrshire',
'V', 'North Lanarkshire',
'N', 'Falkirk',
'G', 'Dumfries and Galloway',
'P', 'Glasgow City',
'X', 'Perth and Kinross',
'C', 'Angus',
'E', 'Scottish Borders',
'Z', 'Shetland',
'R', 'Inverclyde',
'F', 'Clackmannanshire',
'A', 'Aberdeen City',
'D', 'Argyll and Bute', 
'H', 'Dundee City',
'L', 'East Renfrewshire',
'I', 'East Ayrshire',
'5', 'West Lothian') %>% 
  mutate_if(is.character,list(~str_trim(.)))


rb1 <- tribble(~rb1, ~neighborhood_rate, 
  1, 'Very good',
  2, 'Fairly good',
  3, 'Fairly poor',
  4, 'Very poor',
  5, 'No opinion'      
        )

commbel <- tribble(
  ~commbel, ~neighborhood_belong, 
  1, 'Very strongly',
  2, 'Fairly strongly',
  3, 'Not very strongly',
  4, 'Not at all strongly',
  5, 'Don\'t know'
)

area1 <- tribble(~area1, ~neighborhood_better,
        1, 'Got much better',
        2, 'Got a little better',
        3, 'Stayed the same',
        4, 'Got a little worse',
        5, 'Got much worse',
        6, 'Don\'t know' 
        )


asb1d <- tribble(~asb1d, ~neighborhood_disputes,
        1, 'Very common',
        2, 'Fairly common',
        3, 'Not very common',
        4, 'Not at all common',
        5, 'Don\'t know'
)


rb4da <- tribble(~rb4da, ~neighborhood_rely, 
        1, 'Strongly agree',
        2, 'Tend to agree',
        3, 'Neither agree nor disagree',
        4, 'Tend to disagree',
        5, 'Strongly disagree'
        )

rb4db <- tribble(~rb4db, ~neighborhood_home,
        1, 'Strongly agree',
        2, 'Tend to agree',
        3, 'Neither agree nor disagree',
        4, 'Tend to disagree',
        5, 'Strongly disagree'
        )

rb4dc <- tribble(~rb4dc, ~neighborhood_advice,
        1, 'Strongly agree',
        2, 'Tend to agree',
        3, 'Neither agree nor disagree',
        4, 'Tend to disagree',
        5, 'Strongly disagree'
)

rb4dd <- tribble(~rb4dd, ~neighborhood_emergency,
        1, 'Strongly agree',
        2, 'Tend to agree',
        3, 'Neither agree nor disagree',
        4, 'Tend to disagree',
        5, 'Strongly disagree'
)




# Merge database ---------------


RESAS_def <- resas_rural %>% 
  select(local_authority = la, 
         RESAS = definition) %>% 
  clean_council(local_authority) %>% 
  mutate(local_authority = case_when(
    str_detect(local_authority, 'Orkney')~'Orkney', 
    str_detect(local_authority, 'Shetland')~'Shetland',
    T ~ local_authority
  ))


shs2016_resilience <- shs2016_social_public %>% 
  select(1408, 
         rb1, 
         commbel, 
         area1, 
         asb1d, 
         rb4da, 
         rb4db, 
         rb4dc, 
         rb4dd) %>% 
  left_join(council, by = "council") %>% 
  clean_council(local_authority) %>% 
  mutate(local_authority = case_when(
    str_detect(local_authority, 'West Dumbartonshire')~'West Dunbartonshire',
    str_detect(local_authority, 'East Dumbartonshire')~'East Dunbartonshire',
    T ~ local_authority
  )) %>% 
  left_join(RESAS_def, by = "local_authority")


res_ls <- list(rb1, 
commbel, 
area1, 
asb1d, 
rb4da, 
rb4db, 
rb4dc, 
rb4dd)

by_vect <- c('rb1', 
       'commbel', 
       'area1', 
       'asb1d', 
       'rb4da', 
       'rb4db', 
       'rb4dc', 
       'rb4dd')

for(i in seq_along(res_ls)){
  shs2016_resilience <- 
    shs2016_resilience %>% 
    left_join(res_ls[[i]], by = by_vect[[i]])
}

shs2016_resilience <- shs2016_resilience %>% 
  select(local_authority, 
         RESAS,
         contains('neighborhood'))

# save(shs2016_resilience, 
#      file = 'data/shs2016_resilience.RData')


# Functions to make plots ----------------

resas_order <- shs2016_resilience %>% 
  distinct(RESAS) %>% 
  pull(RESAS) %>% 
  .[c(2, 1, 3, 4)]


shs_neighs <- function(.var){
  shs2016_resilience %>% 
    count(RESAS, .dots = lazyeval::lazy(.var)) %>% 
    na.omit() %>% 
    group_by(RESAS) %>% 
    mutate(freq = n/sum(n)) %>% 
    ungroup() %>% 
    mutate(RESAS = fct_relevel(RESAS, resas_order))
}


col_vars <- c(Blues[c(8, 4)], 
  Greens[c(4, 8)])


names(col_vars) <- shs2016_resilience %>% 
  distinct(RESAS) %>% 
  pull(RESAS) %>% 
  .[c(2, 1, 3, 4)]


# RESAS - Rate neighborhood ----------------

neigh_rate <- c('Very good', 
  'Fairly good', 
  'Fairly poor', 
  'Very poor', 
  'No opinion')

shs_neighs(neighborhood_rate) %>% 
  mutate(neighborhood_rate = fct_relevel(
    neighborhood_rate, neigh_rate
  )) %>% 
  ggplot(aes(RESAS, freq, 
             color = RESAS, 
             fill = RESAS))+
  geom_col() +
  facet_grid(neighborhood_rate~.)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = col_vars)+
  scale_fill_manual(values = col_vars)+
  theme_min_plus+
  theme(strip.text.y = element_text(angle = 0, 
                                    hjust = 0))+
  labs(title = 'Scottish Household Survey 2016',
       subtitle = 'Thinking now about the neighbourhood you live in,\nhow would you rate it as a place to live?', 
       x =NULL, 
       y = 'Percent', 
       caption = 'X-squared = 281.67, df = 12, p-value < 2.2e-16')


# chisq.test(shs2016_resilience$RESAS, 
#            shs2016_resilience$neighborhood_rate)


# RESAS - Gotten Better neighborhood --------------

neigh_better <- unique(shs2016_resilience$neighborhood_better)[
  c(5, 1, 3, 2, 6, 4)
]

shs_neighs(neighborhood_better) %>%
  mutate(neighborhood_better = fct_relevel(
    neighborhood_better, neigh_better
  )) %>% 
  ggplot(aes(RESAS, freq, 
             color = RESAS, 
             fill = RESAS))+
  geom_col() +
  facet_grid(neighborhood_better~.)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = col_vars)+
  scale_fill_manual(values = col_vars)+
  theme_min_plus+
  theme(strip.text.y = element_text(angle = 0, 
                                    hjust = 0))+
  labs(title = 'Scottish Household Survey 2016',
       subtitle = 'Thinking about your local neighbourhood,\ndo you think it has got better, stayed the same or got worse?', 
       x =NULL, 
       y = 'Percent', 
       caption = expression(paste(chi^{2}==64.186,' df = 15, p-value = 4.743e-08')))


# chisq.test(shs2016_resilience$RESAS, 
#            shs2016_resilience$neighborhood_better)











# RESAS - Emergency rely --------------

neigh_emerg <- unique(shs2016_resilience$neighborhood_emergency)[
  c(1, 2, 3, 6, 4)
  ]

shs_neighs(neighborhood_emergency) %>%
  mutate(neighborhood_emergency = fct_relevel(
    neighborhood_emergency, neigh_emerg
  )) %>% 
  ggplot(aes(RESAS, freq, 
             color = RESAS, 
             fill = RESAS))+
  geom_col() +
  facet_grid(neighborhood_emergency~.)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = col_vars)+
  scale_fill_manual(values = col_vars)+
  theme_min_plus+
  theme(strip.text.y = element_text(angle = 0, 
                                    hjust = 0))+
  labs(title = 'Scottish Household Survey 2016',
       subtitle = 'Would offer help to neighbours in an emergency?', 
       x =NULL, 
       y = 'Percent', 
       caption = expression(paste(chi^{2}==266.37,' df = 12, p-value = < 2.2e-16')))


# chisq.test(shs2016_resilience$RESAS, 
#            shs2016_resilience$neighborhood_rely)


message('Database is called shs2016_resilience')
















# RESAS - Gotten Better neighborhood --------------

neigh_rely <- unique(shs2016_resilience$neighborhood_rely)[
  c(1, 2, 4, 5, 3, 6)
  ]

shs_neighs(neighborhood_rely) %>%
  mutate(neighborhood_rely = fct_relevel(
    neighborhood_rely, neigh_rely
  )) %>% 
  ggplot(aes(RESAS, freq, 
             color = RESAS, 
             fill = RESAS))+
  geom_col(show.legend = F) +
  facet_grid(neighborhood_rely~.)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_color_manual(values = col_vars)+
  scale_fill_manual(values = col_vars)+
  theme_min_plus+
  theme(strip.text.y = element_text(angle = 0, 
                                    hjust = 0), 
        plot.margin = margin(1,1,1,1, 'cm'))+
  labs(title = 'Scottish Household Survey 2016',
       subtitle = 'Would offer help to neighbours in an emergency?', 
       x =NULL, 
       y = 'Percent', 
       caption = expression(paste(chi^{2}==266.37,' df = 12, p-value = < 2.2e-16')))


chisq.test(shs2016_resilience$RESAS, 
           shs2016_resilience$neighborhood_rely)




