# sum_stat.R
# run cleaning script first and keep var in namespace

# Respondents who have used 311 at least once (CSS)
# 1 = yes, 2 = no, 9 = don't know
df_css_311 %>% 
  count(contacted_311_call_center) %>%
  mutate(total = sum(n)) %>%
  mutate(prop = n / total)

# Total 311 complaints (2016)

# How many total 311 requests? Opened 2016
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  distinct(case_id) %>%
  nrow() # 103955

# Of req opened in 2016, how many are housing (NHS)? 
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department == "NHS") %>% 
  distinct(case_id) %>%
  nrow() # 55743

# Of req opened in 2016, how many are not housing (!= NHS)?
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department != "NHS") %>% 
  distinct(case_id) %>%
  nrow() # 48212

# If housing (NHS) and 2016, how many reports resulted in prop violations?
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department == "NHS") %>%
  filter(!is.na(violation_code)) %>%
  distinct(case_id) %>%
  nrow() # 12411 

# ...  and how many prop violations came from those reports?
df_311_viol_full %>%
  filter(creation_year == 2016) %>%
  filter(department == "NHS") %>%
  filter(!is.na(violation_code)) %>%
  nrow() # 71356
