#### Creating Minimalist Map ####

# Load packages
library(tidyverse)
library(sf)
library(osmdata)
library(extrafont)

font_import(pattern = "EBGaramond-Regular.ttf", prompt = FALSE)


#### Getting OSM Data ####

# Input the city/place to base the map on
place <- "Cordoba, Argentina"

# Get OSM data for different types of road, street and paths (values selected from list of OSM highways https://wiki.openstreetmap.org/wiki/Key:highway)
streets_osm <- opq(place, timeout = 50) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "unclassified",
      "residential",
      "service",
      "footway"
    )
  ) %>%
  osmdata_sf()

# Select the lines from OSM data only
streets <- streets_osm$osm_lines

#### Create Circle to Define the Borders of the Map ####

# Find and define the local coordinate reference system (crs) of the area being mapped
crs_local <- 22177 # crs for the UK (https://epsg.io/27700)

# Define centre of map (I start from the postcode I want to use and then find the lat and long via https://postcodes.io/)
centre <- c(long = -64.1905403,
            lat = -31.4127688)

# Define radius of circle in metres
radius <- 8000

# Create circle given centre and radius
circle <- tibble(lat = centre["lat"], long = centre["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs_local) %>%
  st_buffer(dist = radius) %>%
  st_transform(crs = 4326)

# Crop the streets to the circle
streets_cropped <- streets %>% st_intersection(circle)

#### Create and Save the Map ####

# Create map with OSM attribution
minimalist_map <- ggplot() +
  geom_sf(data = streets_cropped,
          size = .2,
          color = "grey40") +
  labs(
    caption = paste0(
      "Map data ©OpenStreetMap contributors, ODbL.\n",
      "©2023 Carlos Marcos (https://github.com/marcoscarloseduardo)"
    )
  ) +
  ggtitle("Ciudad de Córdoba, Argentina") +
  theme_void() +
  theme(
    plot.caption = element_text(color = "grey20",
                                face = "italic",
                                family = "Garamond"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Garamond", face = "bold.italic")
  )

# Print the map
minimalist_map

# Save the map as a png
ggsave(
  "minimalist_map_cordoba.png",
  plot = minimalist_map,
  width = 400,
  height = 400,
  units = "mm",
  dpi = "retina"
)
