library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(jsonlite)
library(tibble)
library(tidyverse)
library(ggthemes)

# Load Australian State and Territories shapefile data
aus_poc <- readOGR(dsn = "./Data/POA", layer = "POA_2011_AUST")
# # Convert to GeoJSON
# aus_poc_json <- geojson_json(aus_poc)
# # Simplify the polygons to reduce the size
# aus_poc_sim <- ms_simplify(aus_poc_json)
# # Write GeoJSON file out to a file system
# geojson_write(aus_poc_sim, file = "./Data/POA/aus_poc.geojson")
# 
# aus_poc_lines <- ms_lines(aus_poc)
# aus_poc_lines_lines <- aus_poc_lines@lines

points_data <- data.frame(index = numeric(), order = numeric(),
                          long = numeric(), lat = numeric())

for(i in 1:length(aus_poc@polygons)){
  tempdata <- data.frame(aus_poc@polygons[[i]]@Polygons[[1]]@coords) %>% 
    mutate(index = i,
           order = row_number()) %>% 
    select(index, order, long = X1, lat = X2)
  points_data <- rbind(points_data, tempdata)
}

aus_poc_data <- aus_poc@data %>% 
  mutate(index = row_number()) %>% 
  inner_join(points_data) %>% 
  rename(id = index)

aus_poc_summ <- aus_poc@data %>% 
  mutate(index = row_number()) %>% 
  rename(id = index)

ggplot(aes(map_id=id), data=aus_poc_summ) +
  geom_map(aes(), map=aus_poc_data, col = "grey50") +
  expand_limits(x=aus_poc_data$long, y=aus_poc_data$lat) + 
  theme_map() + coord_equal()

syd_poc_data <- aus_poc_data %>% 
  filter(POA_CODE >= 2000 & POA_CODE < 2300)

syd_poc_summ <- aus_poc_summ %>% 
  filter(POA_CODE >= 2000 & POA_CODE < 2300)

ggplot(aes(map_id=id), data=syd_poc_summ) +
  geom_map(aes(), map=syd_poc_data, col = "grey50") +
  expand_limits(x=syd_poc_data$long, y=syd_poc_data$lat) + 
  theme_map() + coord_equal()


library(ozmaps)
library(sf)
#> Linking to GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1

oz_states <- ozmaps::ozmap_states
oz_states

ste <- ozmaps::abs_ste
