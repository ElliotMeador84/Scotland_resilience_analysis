library(tidyverse)
library(ggraph)
library(tidygraph)
library(readxl)
library(janitor)
library(sf)
library(tmap)

# to the bitbucket
# browseURL('https://id.atlassian.com/login?continue=https%3A%2F%2Fid.atlassian.com%2Fopenid%2Fv2%2Fop%3Fopenid.return_to%3Dhttps%3A%2F%2Fbitbucket.org%2Fsocialauth%2Fcomplete%2Fatlassianid%2F%3Fjanrain_nonce%253D2019-09-04T12%25253A06%25253A14ZtlzhMv%26openid.sreg.optional%3Dfullname%2Cnickname%2Cemail%26openid.ns%3Dhttp%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%26openid.ns.sreg%3Dhttp%3A%2F%2Fopenid.net%2Fextensions%2Fsreg%2F1.1%26openid.crowdid.application%3Dbitbucket%26openid.assoc_handle%3D21862042%26openid.ns.crowdid%3Dhttps%3A%2F%2Fdeveloper.atlassian.com%2Fdisplay%2FCROWDDEV%2FCrowdID%252BOpenID%252Bextensions%2523CrowdIDOpenIDextensions-login-page-parameters%26openid.identity%3Dhttp%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select%26openid.realm%3Dhttps%3A%2F%2Fbitbucket.org%26openid.claimed_id%3Dhttp%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select%26openid.mode%3Dcheckid_setup&prompt=&application=bitbucket&tenant=&email=&errorCode=')





# --------------------------------
# --------------------------------
# Example db read and join
# --------------------------------
# --------------------------------


library(RSQLite)
library(tidyverse)
library(rgdal)

# connects to database

db = dbConnect(SQLite(), dbname="~/Downloads/pc_to_resilience (1).gpkg")


# db structure
dbListTables(db)
dbListFields(db, "postcode_to_POI")

# Group by variables of interest -- think in dplyr

postcodes = dbGetQuery(db, "SELECT datazone, 
resilience_type,  median(dist_km) AS dist
                       FROM postcode_to_POI
                       GROUP BY datazone, resilience_type") %>% 
  as_tibble()


datazones <-  st_read("~/Downloads/pc_to_resilience (1).gpkg", "DataZone_2011")


resilience_indicators_sf <- datazones %>% 
  left_join(postcodes, by = c('DataZone' = 'datazone'))

st_write(resilience_indicators_sf, dsn = '~/OneDrive - SRUC/Data/resilience/resilience_indicators_sf.gpkg')

dbDisconnect(db)
rm(db)


# Linking resilience with rural --------

resilience_types <- 
  list('Everyday', 'Emergency', 'Medical')


resilience_indicators_dist_dz <- 
  map_df(resilience_types, function(x){
    resilience_indicators_sf %>% 
      filter(resilience_type == x) %>% 
      as_tibble() %>% 
      select(DataZone, resilience_type, dist) %>% 
      distinct()
  })


path.get()
resilience_indicators_dist_dz %>% 
  write_csv('~/OneDrive - SRUC/scotland_ncr/data/resilience_indicators_dist_dz.csv')




