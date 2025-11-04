

library(sf)
library(spData) # has nz map/data
library(ggplot2)
library(janitor)
library(scales)

nz <- nz %>%
  select(Name, geom)


ggplot(data = nz) +
  geom_sf(aes(fill = Name), color = "black") + # Fill by region name and add black borders
  labs(title = "Map of New Zealand Regions", fill = "Region Name") +
  theme_bw() # Use a clean theme

# Stats NZ subnational population estimates (RC, constituency), by age and sex, at 30 June 2023-24 (2023 boundaries)
pop24 <- read_csv("maps/2025-11-04_polygon/statsNZ_pop_region.csv") %>%
  clean_names() %>%
  filter(year_at_30_june == 2024) %>%
  filter(area != "Total New Zealand by regional councils") %>%
  select(Name = area, Population24 = obs_value) %>%
  mutate(Name = str_remove(Name, "region")) %>%
  mutate(Name = str_trim(Name)) %>%
  mutate(Name =  str_replace_all(Name, "ū", "u")) %>%
 mutate(Name =  str_replace_all(Name, "Wh", "W"))



nzpop24 <- left_join(nz, pop24, by = "Name")

  
ggplot(data = nzpop24) +
  geom_sf(aes(fill = Population24), color = "black") + # Fill by region name and add black borders
  labs(title = "New Zealand Population by Region", fill = "Population") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = label_number(scale = 1e-6, suffix = "M")) +
  theme_bw() 
 
