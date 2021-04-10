# Kansas City Data Merge and Tidy

library(tidyverse)  # the usual
library(lubridate)  # dates


# Load data ---------------------------------------------------------------

# 311
df_311 <- read_csv("Data/kcmo_311_full.csv.gz")

# Housing Violations
df_prop_viol <- read_csv("Data/kcmo_prop_viol_full.csv.gz")

# Citizen Satisfaction Survey
print("<<< Survey TBD >>>")

# Census
df_acs17 <- read_csv("Data/kcmo_tract_acs_data.csv.gz")


# Merge 311 ---------------------------------------------------------------

df_311_viol_full <-
  df_311 %>%
  left_join(df_prop_viol, by = c("case_id", "zip_code", "neighborhood", "county", "council_district", "police_district"))

# How many total 311 requests? Opened 2016
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  nrow() # 162913

# Of req opened in 2016, how many are housing (NHS)? 
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department == "NHS") %>% 
  nrow() # 114688 

# Of req opened in 2016, how many are not housing (!= NHS)?
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department != "NHS") %>% 
  nrow() # 48225

# If housing (NHS) and 2016, how many resulted in prop violations?
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department == "NHS") %>%
  filter(!is.na(violation_code)) %>%
  nrow() # 71356 ( + 43332 = 114688 total NHS)
  



