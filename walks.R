library(googlesheets4)
library(tidyverse)
library(janitor)

bookings <- read_sheet("https://docs.google.com/spreadsheets/d/1v3ZIwjE3RgQ3y0seRQ1EAg1xj1VN0oVUHjGKm_-6bRY/edit?gid=0#gid=0", sheet = "gw_bookings") %>%
select(-"Total all Great Walks") %>%
  pivot_longer(names_to = "track", values_to = "count", "Abel Tasman Coast Track":"Whanganui Journey") 


tracks <- read_sheet("https://docs.google.com/spreadsheets/d/1v3ZIwjE3RgQ3y0seRQ1EAg1xj1VN0oVUHjGKm_-6bRY/edit?gid=0#gid=0", sheet = "gw_lat_long") %>%
  clean_names() %>%
 separate(start_lat_long, into = c("start_lat", "start_long"), ",") %>%
  separate(end_lat_long, into = c("end_lat", "end_long"), ",") %>%
mutate(track_format = stringr::str_extract(location, "\\([^)]+\\)$"), # extract string within last bracket
                track_format = stringr::str_remove_all(track_format, "[\\(\\)]"),  # remove brackets
                location = stringr::str_remove(location, "\\s*\\([^)]+\\)$")) %>% # make everything but the bracket content location
  mutate(start_location = str_extract(location, "^[^:]+"), # pull up to first : into start location
         end_location = str_extract(location, "(?<=;\\s)[^:]+")) %>% # pull text after ; space up to :
mutate(start_lat = as.numeric(start_lat), end_lat = as.numeric(end_lat), 
       start_long = as.numeric(start_long), end_long = as.numeric(end_long))
  
  
glimpse(tracks)

joined <- left_join(bookings, tracks, by = "track")

total_year <- joined %>%
  group_by(year, track, start_lat, start_long, track_format) %>%
  summarise(total_bookings = sum(count))

total23_24 <- total_year %>%
  filter(year == "2023-2024")


library(rnaturalearth)
library(plotly)

nz <- ne_countries(country = "new zealand", returnclass = "sf", scale = "large")


# first plot from tracks data to check locations, all 10 walking

ggplot() +
  geom_sf(data = nz) +
  geom_point(data = tracks, aes(x = start_long, y = start_lat, colour = track_format)) +
  coord_sf(xlim = c(166, 179), ylim = c(-47, -34)) +
  theme_minimal() 



# then plot from bookings joined data
map2 <-  ggplot() +
  geom_sf(data = nz) +
  geom_point(data = total23_24, aes(x = start_long, y = start_lat, 
                                    colour = track_format, size = total_bookings)) +
  coord_sf(xlim = c(166, 179), ylim = c(-47, -34)) +
  scale_size_continuous(range = c(2, 6)) + 
  theme_minimal() 


ggplotly(map2)




