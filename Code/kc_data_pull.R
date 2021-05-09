# Kansas City Data collection

library(tidyverse)  # the usual
library(lubridate)  # dates
library(tidycensus) # ACS
library(nycgeo)     # nyc geo
library(sf)
library(tigris)     # shp
library(raster)
library(RSocrata)   # API 
library(jsonlite)   
library(glue)  
library(crunch)


# Citizen Satisfaction Survey (CSS)
# 2014 - 2017
# Source: https://data.kcmo.org/Government/Citizen-Satisfaction-Survey-Results-Previous-Years/gphs-q877

# df_css <- readxl::read_xlsx(path = "../311_protected_data/KCMODF_ConsolidatedData_FY13_FY20.xlsx")

# 311 Survey Requests
# 2013 - present
# Source: https://data.kcmo.org/311/311-Call-Center-Service-Requests-2007-March-2021/7at3-sxhp

url_311 <- "https://data.kcmo.org/resource/7at3-sxhp.json"
sql_311 <- ""
df_311 <- RSocrata::read.socrata(paste0(url_311, sql_311)) # jsonlite::fromJSON()

rm(url_311, sql_311)
crunch::write.csv.gz(df_311, file = "Data/kcmo_311_full.csv.gz")

# Property Violations
# 2015 - present
# Source: https://data.kcmo.org/Housing/Property-Violations/nhtf-e75a

url_prop_viol <-  "https://data.kcmo.org/resource/nhtf-e75a.json"
sql_prop_viol <- ""
df_prop_viol <- RSocrata::read.socrata(paste0(url_prop_viol, sql_prop_viol)) # jsonlite::fromJSON()

rm(url_prop_viol, sql_prop_viol)
crunch::write.csv.gz(df_prop_viol, file = "Data/kcmo_prop_viol_full.csv.gz")

# Census Population (ACS)
# 2017

# view var
v17 <- load_variables(2017, "acs5", cache = F)
View(v17)
rm(v17)

# variables to download from acs
acs_var <- c(
  "B01001_001",  # total population
  "B03002_003",  # non hispanic white
  "B03002_004",  # non hispanic black
  "B03002_006",  # non hispanic asian
  "B03002_012",  # hispanic
  "B01002_001",  # median age
  "B19013_001",  # median hh income
  "B17021_002",  # below 100% poverty level
  "B17021_001",  # poverty level denom
  "B15003_001",  # pop 25 over
  "B15003_022",  # bachelors degree
  "B15003_023",  # masters degree
  "B15003_024",  # professional degree
  "B15003_025"   # doctorate
)

mo_counties <- c("jackson", "clay", "platte", "clay")

df_acs <- get_acs(
  state = "MO",
  county = mo_counties,
  geography = "tract",
  variables = acs_var,
  survey = "acs5",
  geometry = F,
  output = "wide"
)

# mo shape file
mo_shp <- tigris::tracts("MO", county = mo_counties, year = 2017)

tract_data_mo <- df_acs %>%
  filter(GEOID %in% mo_shp$GEOID)

# calculate new vars, pcts, moes, etc
tract_acs_data <- 
  tract_data_mo %>% 
    mutate(
      pop_white_pct_est = B03002_003E / B01001_001E,
      # pop_white_pct_moe = moe_prop(B03002_003E, B01001_001E,
      #                              B03002_003M, B01001_001M),
      pop_black_pct_est = B03002_004E / B01001_001E,
      # pop_black_pct_moe = moe_prop(B03002_004E, B01001_001E,
      #                              B03002_004M, B01001_001M),
      pop_hisp_pct_est = B03002_012E / B01001_001E,
      # pop_hisp_pct_moe = moe_prop(B03002_012E, B01001_001E,
      #                             B03002_012M, B01001_001M),
      pop_asian_pct_est = B03002_006E / B01001_001E,
      # pop_asian_pct_moe = moe_prop(B03002_006E, B01001_001E,
      #                              B03002_006M, B01001_001M),
      pop_ba_above_est = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
      # pop_ba_above_moe = pmap_dbl(
      #   list(B15003_022M, B15003_023M, B15003_024M, B15003_025M,
      #        B15003_022E, B15003_023E, B15003_024E, B15003_025E),
      #   ~ moe_sum(
      #     moe = c(..1, ..2, ..3, ..4),
      #     estimate = c(..5, ..6, ..7, ..8),
      #     na.rm = TRUE
      #   )
      # ),
      pop_ba_above_pct_est = pop_ba_above_est / B15003_001E,
      # pop_ba_above_pct_moe = moe_prop(pop_ba_above_est, B15003_001E,
      #                                 pop_ba_above_moe, B15003_001M),
      pop_inpov_pct_est = B17021_002E / B17021_001E,
      # pop_inpov_pct_moe = moe_prop(B17021_002E, B17021_001E,
      #                              B17021_002M, B17021_001M)
    ) %>%
      select(
        geoid = GEOID,
        pop_total_est = B01001_001E,
        #pop_total_moe = B01001_001M,
        med_age_est = B01002_001E,
        #med_age_moe = B01002_001M,
        med_hhinc_est = B19013_001E,
        #med_hhinc_moe = B19013_001M,
        pop_white_est = B03002_003E,
        #pop_white_moe = B03002_003M,
        pop_black_est = B03002_004E,
        #pop_black_moe = B03002_004M,
        pop_hisp_est = B03002_012E,
        #pop_hisp_moe = B03002_012M,
        pop_asian_est = B03002_006E,
        #pop_asian_moe = B03002_006M,
        pop_ba_above_est,
        #pop_ba_above_moe,
        pop_educ_denom_est = B15003_001E,
        #pop_educ_denom_moe = B15003_001M,
        pop_inpov_est = B17021_002E,
        #pop_inpov_moe = B17021_002M,
        pop_inpov_denom_est = B17021_001E,
        #pop_inpov_denom_moe = B17021_001M,
        #pop_white_pct_est:pop_inpov_pct_moe
      ) %>%
      #mutate(pop_ba_above_moe = as.numeric(round(pop_ba_above_moe))) %>%
      mutate_at(vars(contains("pct")),
                ~ as.numeric(round(.x * 100, digits = 1))) %>%
      mutate_all(~ replace(.x, is.nan(.x), NA))

# remove unnecessary var
rm(acs_var, df_acs, mo_counties, tract_data_mo)
sf::st_write(mo_shp, paste0("Data/shapefiles/", "kcmo.shp"))
crunch::write.csv.gz(tract_acs_data, file = "Data/kcmo_tract_acs_data.csv.gz")



