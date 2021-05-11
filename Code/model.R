# model.R

# Dependencies: load parallel folder with \311_protected_data

# libs and funcs ---

library(tidyverse)
library(tidymodels)
library(ranger)
library(vip)
library(ggthemes)
library(modelsummary)
library(janitor)
library(knitr)
library(kableExtra)

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

# Load data ----
#' NOTE: This is protected data and is only available on request from KCMO City Gov't
#' Please note the file path is external to the repository
kcmo_survey <- 
  # load 
  read_csv("../311_protected_data/KCMODF_ConsolidatedData_FY13_FY20.csv") %>%
  # select years
  filter(YEAR %in% c("FY2014_15", "FY2015_16", "FY2016_17")) %>%
  # clean var names
  janitor::clean_names() %>%
  # remove "uncertain" for contacting 311 
  filter(contacted_311_call_center != 9) %>%
  # shorten var name
  rename(contacted_311 = contacted_311_call_center) %>%
  # outcome as factor
  mutate(contacted_311 = as.factor(contacted_311)) %>%
  filter(total_annual_household_income != 5) %>%
  filter(have_access_to_internet_at_home != 3) 

# Feature engineering
kcmo_survey <- 
  kcmo_survey %>%
    # select var of interest
    select(
      # / outcome /
      contacted_311, 
      # / predictors /
      # demographics + socioeconomic
      gender_identity,
      what_is_your_age, 
      total_annual_household_income,
      describe_you_race_ethnicity, 
      are_you_hispanic_latino_spanish, 
      own_or_rent_current_residence, 
      how_many_years_lived_in_kcmo, 
      have_access_to_internet_at_home, 
      how_often_use_curbside_recycling,
      # civic engagement
      visited_city_s_website, 
      visited_kcmo_community_center, 
      visited_any_parks_in_kcmo, 
      # political participation
      vote_in_election_2012_13_to_2014_15, 
      # citizen satisfaction
      quality_of_neighborhood_services, 
      quality_of_life_in_the_city, 
      feeling_of_safety_in_the_city,
    )

kcmo_survey <-
  kcmo_survey %>%
  mutate(
    # 311
    contacted_311 = if_else(contacted_311 == 1, 1, 0),
    contacted_311 = factor(contacted_311), 
    # gender
    female = if_else(gender_identity == 2, 1, 0),
    # age
    age_18_24 = if_else(what_is_your_age == 1, 1, 0),
    age_25_34 = if_else(what_is_your_age == 2, 1, 0),
    age_35_44 = if_else(what_is_your_age == 3, 1, 0),
    age_45_54 = if_else(what_is_your_age == 4, 1, 0),
    age_55_65 = if_else(what_is_your_age == 5, 1, 0),
    age_over65 = if_else(what_is_your_age == 6, 1, 0),
    # income
    income_under30 = if_else(total_annual_household_income == 1, 1, 0),
    income_30_59 = if_else(total_annual_household_income == 2, 1, 0),
    income_60_99 = if_else(total_annual_household_income == 3, 1, 0),
    income_over100 = if_else(total_annual_household_income == 4, 1, 0),
    # race/ethnicity
    asian_pac_isl = if_else(describe_you_race_ethnicity == 1, 1, 0),
    white = if_else(describe_you_race_ethnicity == 2, 1, 0),
    amer_ind_esk = if_else(describe_you_race_ethnicity == 3, 1, 0),
    black = if_else(describe_you_race_ethnicity == 4, 1, 0),
    race_other = if_else(describe_you_race_ethnicity == 5, 1, 0),
    hispanic = if_else(are_you_hispanic_latino_spanish  == 1, 1, 0),
    # own/rent
    own_prop = if_else(own_or_rent_current_residence == 1, 1, 0),
    rent_prop = if_else(own_or_rent_current_residence == 2, 1, 0),
    # internet
    internet_access = if_else(have_access_to_internet_at_home == 1, 1, 0),
    # recycle
    recycle_freq_weekly = if_else(how_often_use_curbside_recycling == 1, 1, 0),
    recycle_freq_biweekly = if_else(how_often_use_curbside_recycling == 2, 1, 0),
    recycle_freq_monthly = if_else(how_often_use_curbside_recycling == 3, 1, 0),
    recycle_freq_never = if_else(how_often_use_curbside_recycling == 4, 1, 0),
    # city website
    visit_city_website = if_else(visited_city_s_website == 1, 1, 0),
    # community center
    visit_comm_center = if_else(visited_kcmo_community_center == 1, 1, 0),
    # parks
    visit_park = if_else(visited_any_parks_in_kcmo == 1, 1, 0),
    # voted
    #voted = if_else(vote_in_election_2012_13_to_2014_15 == 1, 1, 0)#,
    # years
    #years_kcmo = (how_many_years_lived_in_kcmo - mean(how_many_years_lived_in_kcmo)) / sd(how_many_years_lived_in_kcmo)
  ) %>%
  filter(
    quality_of_neighborhood_services != 9,
    quality_of_life_in_the_city != 9,
    feeling_of_safety_in_the_city != 9
  ) %>%
  # remove unneeded var
  select(!c(
    gender_identity, 
    what_is_your_age,
    total_annual_household_income,
    describe_you_race_ethnicity,
    are_you_hispanic_latino_spanish,
    own_or_rent_current_residence,
    have_access_to_internet_at_home,
    how_often_use_curbside_recycling,
    visited_city_s_website,
    visited_kcmo_community_center,
    visited_any_parks_in_kcmo,
    vote_in_election_2012_13_to_2014_15,
    quality_of_neighborhood_services,
    quality_of_life_in_the_city,
    feeling_of_safety_in_the_city,
    how_many_years_lived_in_kcmo
    ))


# survey split on 311
kcmo_survey %>% 
  count(contacted_311) %>% 
  mutate(prop = n/sum(n)) 
  # seems split pretty evenly... 
  # 5474 yes (47%)

# train/test and validation sets ----
set.seed(123)
splits <- initial_split(kcmo_survey, strata = contacted_311)

survey_other <- training(splits)
survey_test  <- testing(splits)

# training set proportions by contacted_311
survey_other %>% 
  count(contacted_311) %>% 
  mutate(prop = n/sum(n))

# test set proportions by contacted_311
survey_test  %>% 
  count(contacted_311) %>% 
  mutate(prop = n/sum(n))

# create validation set from survey_other
set.seed(234)
val_set <- validation_split(survey_other, 
                            strata = contacted_311, 
                            prop = 0.80)
val_set

# Penalized logistic regression ----

# define model engine
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  #logistic_reg(mixture = 1) %>% 
  set_engine("glmnet")# %>% 
  #set_mode("classification")

# feature engineering / create recipe
lr_recipe <- 
  recipe(contacted_311 ~ .,
         data = survey_other) 
  # ft eng done above

# Workflow 
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

# Hyperparameter tuning 
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

# Train/Tune 
lr_res <- 
  lr_workflow %>% 
  #fit(survey_other)
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE, extract = get_model),
            metrics = metric_set(roc_auc))

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number()) + 
  theme_clean()

ggsave("Figures/lr_auc_pen.png", lr_plot, "png")

# Select best model
top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(contacted_311, .pred_0) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

# finalize wkflw 
lr_workflow <- 
  lr_workflow %>%
  finalize_workflow(lr_best)

# Random Forest Model ----

cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

# Create recipe and workflow

rf_recipe <- 
  recipe(contacted_311 ~ ., data = survey_other)
  # ft eng done above

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

# Train/Tune
rf_mod

rf_mod %>%    
  parameters()  

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(contacted_311, .pred_0) %>% 
  mutate(model = "Random Forest")

# compare rf and lr
bind_rows(rf_auc, lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_brewer(palette = "Set1") +
  theme_bw()

# The last fit ----

# lr model
last_lr_fit <- 
  lr_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(splits)

last_lr_fit %>% 
  collect_metrics()

lr_vip <-
  last_lr_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20) + 
  theme_clean()

ggsave("Figures/lr_vip.png", lr_vip, "png")

last_lr_auc <-
  last_lr_fit %>% 
  collect_predictions() %>% 
  roc_curve(contacted_311, .pred_0)  %>% 
  mutate(model = "Logistic Regression")

# rf model
# the last model
last_rf_mod <-
  rand_forest(mtry = 1, min_n = 38, trees = 1000) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>%
  collect_metrics()

rf_vip <-
  last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20) + 
  theme_clean()

ggsave("Figures/rf_vip.png", rf_vip, "png")

last_rf_auc <-
  last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(contacted_311, .pred_0)  %>% 
  mutate(model = "Random Forest")


# compare rf and lr
final_auc_plot <-
  bind_rows(last_rf_auc, last_lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_brewer(palette = "Set1") +
  theme_bw()

ggsave("Figures/final_auc_plot.png", final_auc_plot)


last_lr_fit %>% 
  conf_mat_resampled()

last_rf_fit %>%
  conf_mat_resampled()


# pull coefs from lr model -- save to lr_coef.tex
map_dfr(lr_res$.extracts, ~ .x[[1]][[1]]) %>%
select(1, 2) %>%
kbl(digits = 3, 
    booktabs = T, 
    caption = "Results of the logistic regression model for 311 use",
    format = "latex") 



  