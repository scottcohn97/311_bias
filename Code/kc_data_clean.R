# Kansas City Data Merge and Tidy

library(tidyverse)  # the usual
library(lubridate)  # dates
library(janitor)    # clean col names


# Load data ---------------------------------------------------------------

# 311
df_311 <- read_csv("Data/kcmo_311_full.csv.gz")

# Housing Violations
df_prop_viol <- read_csv("Data/kcmo_prop_viol_full.csv.gz")

# Citizen Satisfaction Survey
df_css <- readxl::read_xlsx(path = "../311_protected_data/KCMODF_ConsolidatedData_FY13_FY20.xlsx")

# Census
df_acs17 <- read_csv("Data/kcmo_tract_acs_data.csv.gz")


# Merge 311 with violations --------------------------------------------------------------

df_311_viol_full <-
  df_311 %>%
  left_join(df_prop_viol, by = c("case_id", "zip_code", "neighborhood", "county", "council_district", "police_district"))

# Clean CSS ---------------------------------------------------------------

# filter to 2014-2017
df_css <- 
  df_css %>% 
  # filter(str_detect(ID, pattern = c("2014", "2015", "2016", "2017"))) %>% 
  janitor::clean_names() %>%
  mutate(
    year = case_when(
      str_detect(id, pattern = "2014") ~ 2014,
      str_detect(id, pattern = "2015") ~ 2015,
      str_detect(id, pattern = "2016") ~ 2016,
      str_detect(id, pattern = "2017") ~ 2017
    )
  ) %>%
  drop_na(year)

# extract 311 and demographic info
df_css_311 <-
  df_css %>% 
  select(1:3, 7, 24, 130:134, 168, 200:217)


  
df_css_311 %>%
  group_by(quality_of_311_services) %>%
  summarise(cnt = n()) %>% 
  ggplot(aes(x = quality_of_311_services, y = cnt)) + 
  geom_bar(position = "dodge", stat = "identity")

  

