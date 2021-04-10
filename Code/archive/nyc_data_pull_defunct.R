# Data collection


library(tidyverse)  # the usual
library(lubridate)  # dates
library(tidycensus) # ACS
library(nycgeo)     # nyc geo
library(sf)
library(tigris)     # shp
library(RSocrata)   # API 
library(jsonlite)   
library(glue)       



# 311 Data (2014) ----
# Source: https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9

url_311 <- "https://data.cityofnewyork.us/resource/erm2-nwe9.json"
sql_311 <- "?$where=created_date between '2014-01-01T12:00:00' and '2014-12-31T23:00:00'&$limit=1000000"

df_311 <- RSocrata::read.socrata(paste0(url_311, sql_311)) # jsonlite::fromJSON()

# HPD HMC Housing Violations (Violations + Complaints) ----

# Source: https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5
url_hpd_viol <- "https://data.cityofnewyork.us/resource/wvxf-dwi5.json"
# limit to 2014
sql_hpd_viol <- "?$where=inspectiondate between '2014-01-01T12:00:00' and '2014-12-31T23:00:00'&$limit=800000"

df_hpd_viol <- RSocrata::read.socrata(paste0(url_hpd_viol, sql_hpd_viol)) # jsonlite::fromJSON()

# # Source: https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Complaints/uwyv-629c
url_hpd_compl <- "https://data.cityofnewyork.us/resource/uwyv-629c.json"
# limit to 2014
sql_hpd_compl <- "?$where=receiveddate between '2014-01-01T12:00:00' and '2014-12-31T23:00:00'&$limit=800000"

df_hpd_compl <- RSocrata::read.socrata(paste0(url_hpd_compl, sql_hpd_compl))


# Multiple Dwelling Registration ------------------------------------------

# Source: https://data.cityofnewyork.us/Housing-Development/Multiple-Dwelling-Registrations/tesw-yqqr


# Primary Land Use Tax Lot Output (MapPLUTO) -----------------------------------------------------

pluto <- read_csv(file = "Data/nyc_pluto_21v1_csv/pluto_21v1.csv")

df_311_pluto <- 
  df_311 %>% 
    select(!borough) %>%
    rename(xcoord = x_coordinate_state_plane,
           ycoord = y_coordinate_state_plane) %>%
    mutate(latitude = as.double(latitude),
           longitude = as.double(longitude),
           bbl = as.double(bbl)) %>%
  
  
    left_join(pluto, by = c("bbl"))


# Oil Boiler Data // Local Law 87 -----------------------------------------

# Source: https://data.cityofnewyork.us/Housing-Development/DOB-NOW-Safety-Boiler/52dp-yji6


# American Community Survey (ACS) -----------------------------------------

# If using kontokosta (2017) data, use ACS 5-year 2015

# view var
v19 <- load_variables(2019, "acs5", cache = F)
View(v19)

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

# nyc_counties <- c("new york", "bronx", "kings", "queens", "richmond")

df_acs <- get_acs(
    state = "NY",
    county = nyc_counties,
    geography = "tract",
    variables = acs_var,
    survey = "acs5",
    geometry = F,
    output = "wide"
  )

# nyc shape file
# nyc_shp <- tigris::tracts(state = "NY", county = nyc_counties)

tract_data_nyc <- df_acs %>%
  filter(GEOID %in% nycgeo::tract_sf$geoid)

# calculate new vars, pcts, moes, etc
tract_acs_data <- 
  tract_data_nyc %>% {
  mutate(
    pop_white_pct_est = B03002_003E / B01001_001E,
    pop_white_pct_moe = moe_prop(B03002_003E, B01001_001E,
                                 B03002_003M, B01001_001M),
    pop_black_pct_est = B03002_004E / B01001_001E,
    pop_black_pct_moe = moe_prop(B03002_004E, B01001_001E,
                                 B03002_004M, B01001_001M),
    pop_hisp_pct_est = B03002_012E / B01001_001E,
    pop_hisp_pct_moe = moe_prop(B03002_012E, B01001_001E,
                                B03002_012M, B01001_001M),
    pop_asian_pct_est = B03002_006E / B01001_001E,
    pop_asian_pct_moe = moe_prop(B03002_006E, B01001_001E,
                                 B03002_006M, B01001_001M),
    pop_ba_above_est = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
    pop_ba_above_moe = pmap_dbl(
      list(B15003_022M, B15003_023M, B15003_024M, B15003_025M,
           B15003_022E, B15003_023E, B15003_024E, B15003_025E),
      ~ moe_sum(
        moe = c(..1, ..2, ..3, ..4),
        estimate = c(..5, ..6, ..7, ..8),
        na.rm = TRUE
      )
    ),
    pop_ba_above_pct_est = pop_ba_above_est / B15003_001E,
    pop_ba_above_pct_moe = moe_prop(pop_ba_above_est, B15003_001E,
                                    pop_ba_above_moe, B15003_001M),
    pop_inpov_pct_est = B17021_002E / B17021_001E,
    pop_inpov_pct_moe = moe_prop(B17021_002E, B17021_001E,
                                 B17021_002M, B17021_001M)
  ) %>%
  select(
    geoid = GEOID,
    pop_total_est = B01001_001E,
    pop_total_moe = B01001_001M,
    med_age_est = B01002_001E,
    med_age_moe = B01002_001M,
    med_hhinc_est = B19013_001E,
    med_hhinc_moe = B19013_001M,
    pop_white_est = B03002_003E,
    pop_white_moe = B03002_003M,
    pop_black_est = B03002_004E,
    pop_black_moe = B03002_004M,
    pop_hisp_est = B03002_012E,
    pop_hisp_moe = B03002_012M,
    pop_asian_est = B03002_006E,
    pop_asian_moe = B03002_006M,
    pop_ba_above_est,
    pop_ba_above_moe,
    pop_educ_denom_est = B15003_001E,
    pop_educ_denom_moe = B15003_001M,
    pop_inpov_est = B17021_002E,
    pop_inpov_moe = B17021_002M,
    pop_inpov_denom_est = B17021_001E,
    pop_inpov_denom_moe = B17021_001M,
    pop_white_pct_est:pop_inpov_pct_moe
  ) %>%
  mutate(pop_ba_above_moe = as.numeric(round(pop_ba_above_moe))) %>%
  mutate_at(vars(contains("pct")),
            ~ as.numeric(round(.x * 100, digits = 1))) %>%
  mutate_all(~ replace(.x, is.nan(.x), NA))
}

# Session Info ----
sessionInfo()