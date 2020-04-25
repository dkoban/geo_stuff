library(dplyr)
library(readr)
library(geohashTools)
library(leaflet)

# Read in example data and province shape files
df <- read_csv("ACLED.csv") %>% select(event_date, latitude, longitude)

# Geohash the data
df$hash <- gh_encode(df$latitude, df$longitude, precision = 4)
density <- df %>% group_by(hash) %>% count()
density[,c("hash_lat","hash_lon","lat_error","lon_error")] <- geohashTools::gh_decode(density$hash, include_delta = TRUE)
density <- density %>% mutate(
  lon_low = hash_lon - lon_error, lon_high = hash_lon + lon_error,
  lat_low = hash_lat - lat_error, lat_high = hash_lat + lat_error)

# Create a leaflet map
map_center <- tibble(lat = as.numeric(names(sort(table(round(density$hash_lat,1)),
                                                 decreasing = TRUE))[1]),
                     lon = as.numeric(names(sort(table(round(density$hash_lon,1)),
                                                 decreasing = TRUE))[1]))
log_n <- log(density$n)
pal <- colorNumeric(palette = "Reds", domain = range(log_n))
map_colors <- pal(log_n)
map <- leaflet() %>%
  setView(lng = map_center$lon-0.2, lat = map_center$lat+0.05, zoom = 6) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addRectangles(
    lng1 = density$lon_low, lat1 = density$lat_low,
    lng2 = density$lon_high, lat2 = density$lat_high,
    fillColor = map_colors, fillOpacity = 0.7, stroke = FALSE,
    popup = as.character(paste("Geohash:", density$hash, "<br>",
                               "Total Hits:", density$n)))
map
