library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggtext)

clr_ocean <- "#d9f0ff"
clr_land <- "#facba6"

world <- ne_countries(scale = 110) |>
  filter(admin != "Antarctica")

places <- tribble(
  ~city                    , ~lat       , ~long        , ~nudge_x , ~nudge_y , ~period,
  "Provo, Utah"            , 40.248674  , -111.649261  , -1400000 ,        0 , "1984; 1993–2003;<br>2006–2008;<br>2010–2012;<br>2017–2019",
  "Rome, Italy"            , 41.970477  ,   12.544698  ,        0 ,  -700000 , "2003–2005",
  "Provo, Utah"            , 40.248674  , -111.649261  , NA       , NA       , NA,
  "Amman, Jordan"          , 32.015175  ,   35.873671  ,  1400000 ,   250000 , "2006",
  "Provo, Utah"            , 40.248674  , -111.649261  , NA       , NA       , NA,
  "Cairo, Egypt"           , 29.9545395 ,   31.2657007 ,        0 ,  -700000 , "2008–2010",
  "Provo, Utah"            , 40.248674  , -111.649261  , NA       , NA       , NA,
  "Durham, North Carolina" , 35.999115  ,  -78.943701  ,  2000000 ,   250000 , "1985–1993; 2012–2017",
  "Provo, Utah"            , 40.248674  , -111.649261  , NA       , NA       , NA,
  "Atlanta, Georgia"       , 33.748955  ,  -84.388099  ,        0 ,  -700000 , "2019–now",
) |>
  mutate(order = 1:n()) |>
  mutate(
    label = glue::glue(
      "**{city}**<br><span style='font-size: 9pt; color: #666666;'>{period}</span>"
    )
  ) |>
  st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

# Plot great circle routes!
# Adapted from Jesse Sadler: https://www.jessesadler.com/post/great-circles-sp-sf/#great-circles-with-sf
# Extract just the coordinates for each point
coords <- st_coordinates(places) |>
  as_tibble() |>
  rename(long = X, lat = Y)

# Make a dataframe of all the paried cities
# (i.e. City A → City B, City B → City C )
pairs <- places |>
  arrange(order) |>
  st_drop_geometry() |>
  mutate(
    long = coords$long,
    lat = coords$lat,
    next_long = lead(long),
    next_lat = lead(lat),
    next_city = lead(city),
    next_order = lead(order)
  ) |>
  filter(!is.na(next_long))

# Create a linestring between each pair of points
pairs_paths <- pairs |>
  mutate(
    direction = if_else(city == "Provo, Utah", "out", "in"),
    geometry = pmap(
      list(long, lat, next_long, next_lat),
      \(long, lat, next_long, next_lat) {
        line <- matrix(c(long, next_long, lat, next_lat), ncol = 2) |>
          st_linestring() |>
          st_sfc(crs = st_crs("EPSG:4326")) |>
          st_segmentize(units::set_units(50, km))
        
        # Trim the line a little at both ends to create a buffer
        line <- st_cast(line, "POINT")
        # Make sure there are enough points to trim
        if (length(line) > 10) {
          # Remove 5 segments from the start and end
          # Each segment is 50 km, so this makes a buffer of ≈250 km
          line <- line[6:(length(line) - 5)] |>
            st_combine() |>
            st_cast("LINESTRING")
        }
        
        line
      }
    )
  ) |>
  mutate(geometry = list_c(geometry)) |>
  st_sf()

# Make a little bounding box with these specific coordinates to to zoom in
bbox <- st_bbox(
  c(xmin = -125, xmax = 57, ymin = 20, ymax = 70),
  crs = st_crs("EPSG:4326")
) |>
  st_as_sfc() |>
  st_transform("+proj=eqearth") |>
  st_bbox()

# Plot this puppy
ggplot() +
  geom_sf(data = world, fill = clr_land, linewidth = 0) +
  geom_sf(
    data = pairs_paths,
    # I tried coloring by direction but didn't like it
    # aes(color = direction),
    color = "#d43511",
    linewidth = 0.8,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  geom_sf(data = places) +
  geom_richtext(
    data = drop_na(places, period),
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    nudge_x = drop_na(places, period)$nudge_x,
    nudge_y = drop_na(places, period)$nudge_y,
    family = "Inter"
  ) +
  coord_sf(
    crs = "+proj=eqearth",
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  labs(
    title = "Everywhere I’ve lived, 1984–today",
    subtitle = "Utah is a gosh dang magnet that keeps pulling me back"
  ) +
  theme_void(base_family = "Inter") +
  theme(
    panel.background = element_rect(fill = clr_ocean),
    plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.5)),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = rel(1.1),
      margin = margin(t = 5, b = 5)
    )
  ) +
  ggview::canvas(width = 10, height = 3.63)