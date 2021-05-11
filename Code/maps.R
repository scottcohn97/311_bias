# maps.R

# Dependencies: run kc_data_clean 
# File too big to upload to git (>100mb)

# NOTE: The ggsave calls take ~forever~ so I just exported to png because I was impatient

library(tidyverse)
library(ggmap)
library(ggthemes)
library(ggspatial)
library(viridis)
library(sf)

# 311 calls
df_311 <- read_csv("Data/kcmo_311_full.csv.gz") %>%
  filter( between(creation_year, 2013, 2016) )

# property violations
df_prop_viol <- read_csv("Data/kcmo_prop_viol_full.csv.gz") %>%
  filter(violation_entry_date > "2013-01-01" & violation_entry_date < "2018-01-01")

# county data
kcmo_cty <- map_data("county", "Missouri") %>% 
  filter(subregion %in% c("jackson", "clay", "platte", "clay"))

# shapefile for neighborhoods
kcmo_nbhd <- sf::read_sf("Data/shapefiles/kcmo_nbhd.shp")

# population by nbhd (2010)
kcmo_nbhd_pop <- RSocrata::read.socrata("https://data.kcmo.org/resource/7nq4-imiw.json")

df_311_geo <- 
  df_311 %>% 
  inner_join(kcmo_nbhd, by = c("neighborhood" = "nbhname")) %>%
  inner_join(kcmo_nbhd_pop, by = c("neighborhood" = "neighborhood_name")) %>%
  group_by(neighborhood) %>% 
  mutate(freq = n())

df_prop_viol_geo <- 
  df_prop_viol %>% 
  inner_join(kcmo_nbhd, by = c("neighborhood" = "nbhname")) %>%
  inner_join(kcmo_nbhd_pop, by = c("neighborhood" = "neighborhood_name")) %>%
  group_by(neighborhood) %>% 
  mutate(freq = n())

# 311 reports at nbhd level -----------------------------------------------

p1_311_nbhd <-
  df_311_geo %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = freq))

p2_311_nbhd <-
  p1_311_nbhd + 
  scale_fill_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  theme_map()

ggsave(p2_311_nbhd, "Figures/311_at_nbhd_level.png", device = "cairo")

# 311 reports per capita at nbhd level ------------------------------------

p1_311_nbhd_per_cap <-
  df_311_geo %>% 
  mutate(freq_per_cap = freq / as.numeric(X_2010_census_population)) %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = freq_per_cap))

p2_311_nbhd_per_cap <-
  p1_311_nbhd_per_cap + 
  scale_fill_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  theme_map()

ggsave(p2_311_nbhd_per_cap, "Figures/311_nbhd_per_cap.png", device = "cairo")

# Property violations at nbhd level ---------------------------------------

p1_prop_viol_nbhd <-
  df_prop_viol_geo %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = freq))

p2_prop_viol_nbhd <-
  p1_prop_viol_nbhd + 
  scale_fill_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  theme_map()

ggsave(p2_prop_viol_nbhd, "Figures/prop_viol_at_nbhd_level.png", device = "cairo")

# Property violations per capita at nbhd level ----------------------------

p1_prop_viol_nbhd_per_cap <-
  df_prop_viol_geo %>% 
  mutate(freq_per_cap = freq / as.numeric(X_2010_census_population)) %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = freq_per_cap))

p2_prop_viol_nbhd_per_cap <-
  p1_prop_viol_nbhd_per_cap + 
  scale_fill_viridis(direction = -1) +
  theme(legend.title = element_blank()) +
  theme_map()

ggsave(p2_prop_viol_nbhd, "Figures/prop_viol_nbhd_per_cap.png", device = "cairo")


