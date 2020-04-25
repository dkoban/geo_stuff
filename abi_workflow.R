library(dplyr)
library(readr)
library(geohashTools)
library(leaflet)
library(rgdal)
library(lubridate)

# Collect
df <- read_csv("ACLED2.csv") %>% select(data_id, event_date, event_type, sub_event_type, notes,
                                        latitude, longitude, geo_precision, source)
df$latitude = df$latitude + runif(nrow(df), 0, 0.002)
df$longitude = df$longitude + runif(nrow(df), 0, 0.002)
armed_clashes <- df %>% filter(sub_event_type == "Armed clash")
suicide_attacks <- df %>% filter(sub_event_type == "Suicide bomb")
idf_attacks <- df %>% filter(sub_event_type == "Shelling/artillery/missile attack")
ied_attacks <- df %>% filter(sub_event_type == "Remote explosive/landmine/IED")
rm(df)

# Condition
armed_clashes <- armed_clashes %>% mutate(date = dmy(event_date)) 
suicide_attacks <- suicide_attacks %>% mutate(date = dmy(event_date))
idf_attacks <- idf_attacks %>% mutate(date = dmy(event_date))
ied_attacks <- ied_attacks %>% mutate(date = dmy(event_date))

# Filter
geocode <- function(df){
  province_shapes <- readOGR("provicinial-boundaries")
  provinces <- province_shapes$PRV_NAME %>% as.character()
  province_shapes$PRV_NAME <- province_shapes$PRV_NAME %>% as.character()
  df$province <- NA
  for(i in 1:length(provinces)){
    spdf <- df
    coordinates(spdf) <- c("longitude", "latitude")
    proj4string(spdf) <- proj4string(province_shapes[i,])
    idx <- !is.na(over(as(spdf, "SpatialPoints"),
                       as(province_shapes[i,], "SpatialPolygons")))
    df$province[idx] <- province_shapes$PRV_NAME[i]}
  return(df)}
armed_clashes <- geocode(armed_clashes) %>% filter(geo_precision >= 2, province %in% c("NANGARHAR"))
suicide_attacks <- geocode(suicide_attacks) %>% filter(geo_precision >= 2, province %in% c("NANGARHAR"))
idf_attacks <- geocode(idf_attacks) %>% filter(geo_precision >= 2, province %in% c("NANGARHAR"))
ied_attacks <- geocode(ied_attacks) %>% filter(geo_precision >= 2, province %in% c("NANGARHAR"))

# Enrich
fatality_events <- read_csv("ACLED2.csv") %>% select(data_id, fatalities) %>% filter(fatalities >= 1) 
armed_clashes <- armed_clashes %>% left_join(fatality_events, by = "data_id") %>% filter(fatalities >= 1) %>%
  mutate(popup_txt = paste0("<b>Event: </b>", sub_event_type, "<br>",
                            "<b>Date: </b>", date, "<br>",
                            "<b>Description: </b>", notes, "<br>",
                            "<b>Fatalities: </b>", fatalities, "<br>",
                            "<b>Report Source: </b>", source))
suicide_attacks <- suicide_attacks %>% left_join(fatality_events, by = "data_id") %>% filter(fatalities >= 1) %>%
  mutate(popup_txt = paste0("<b>Event: </b>", sub_event_type, "<br>",
                            "<b>Date: </b>", date, "<br>",
                            "<b>Description: </b>", notes, "<br>",
                            "<b>Fatalities: </b>", fatalities, "<br>",
                            "<b>Report Source: </b>", source))
idf_attacks <- idf_attacks %>% left_join(fatality_events, by = "data_id") %>% filter(fatalities >= 1) %>%
  mutate(popup_txt = paste0("<b>Event: </b>", sub_event_type, "<br>",
                            "<b>Date: </b>", date, "<br>",
                            "<b>Description: </b>", notes, "<br>",
                            "<b>Fatalities: </b>", fatalities, "<br>",
                            "<b>Report Source: </b>", source))
ied_attacks <- ied_attacks %>% left_join(fatality_events, by = "data_id") %>% filter(fatalities >= 1) %>%
  mutate(popup_txt = paste0("<b>Event: </b>", sub_event_type, "<br>",
                            "<b>Date: </b>", date, "<br>",
                            "<b>Description: </b>", notes, "<br>",
                            "<b>Fatalities: </b>", fatalities, "<br>",
                            "<b>Report Source: </b>", source))

# Merge
df <- bind_rows(armed_clashes %>% mutate(data_source = "armed_clash"), 
                suicide_attacks %>% mutate(data_source = "suicide_attacks"), 
                idf_attacks %>% mutate(data_source = "idf_attacks"), 
                ied_attacks %>% mutate(data_source = "ied_attacks"))

# Summarize
df$hash <- gh_encode(df$latitude, df$longitude, precision = 6)
density <- df %>% group_by(hash) %>% summarize(data_source_count = data_source %>% unique() %>% length())
density[,c("hash_lat","hash_lon","lat_error","lon_error")] <- geohashTools::gh_decode(density$hash, include_delta = TRUE)
density <- density %>% mutate(
  lon_low = hash_lon - lon_error, lon_high = hash_lon + lon_error,
  lat_low = hash_lat - lat_error, lat_high = hash_lat + lat_error)

# Create a leaflet map
map_center <- tibble(lat = as.numeric(names(sort(table(round(density$hash_lat,1)),
                                                 decreasing = TRUE))[1]),
                     lon = as.numeric(names(sort(table(round(density$hash_lon,1)),
                                                 decreasing = TRUE))[1]))
pal <- colorNumeric(palette = "Reds", domain = range(density$data_source_count))
map_colors <- pal(density$data_source_count)
province_shapes <- readOGR("provicinial-boundaries")
map <- leaflet() %>%
  setView(lng = map_center$lon-0.2, lat = map_center$lat+0.05, zoom = 12) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Matter") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addPolygons(data = province_shapes, color = "gray", weight = 2, fillOpacity = 0) %>%
  addRectangles(
    lng1 = density$lon_low, lat1 = density$lat_low,
    lng2 = density$lon_high, lat2 = density$lat_high,
    fillColor = map_colors, fillOpacity = 0.7, stroke = FALSE, group = "Density",
    popup = as.character(paste("Geohash:", density$hash, "<br>",
                               "Data Source Count:", density$data_source_count))) %>%
  addCircleMarkers(lng = armed_clashes$longitude, lat = armed_clashes$latitude, 
                   popup = armed_clashes$popup_txt,
                   color = "blue", fillOpacity = 0.7, radius = 2, group = "Attacks") %>%
  addCircleMarkers(lng = suicide_attacks$longitude, lat = suicide_attacks$latitude,
                   popup = armed_clashes$popup_txt,
                   color = "red", fillOpacity = 0.7, radius = 2, group = "Suicide Attacks") %>%
  addCircleMarkers(lng = ied_attacks$longitude, lat = ied_attacks$latitude,
                   popup = armed_clashes$popup_txt,
                   color = "yellow", fillOpacity = 0.7, radius = 2, group = "IED Attacks") %>%
  addCircleMarkers(lng = idf_attacks$longitude, lat = idf_attacks$latitude,
                   popup = armed_clashes$popup_txt,
                   color = "orange", fillOpacity = 0.7, radius = 2, group = "IED Attacks") %>%
  addLayersControl(
    baseGroups = c("Dark Matter", "World Imagery"),
    overlayGroups = c("Density", "Attacks", "Suicide Attacks", "IED Attacks", "IDF Attacks"))
map
 
sum("cat", "dog")
