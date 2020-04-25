library(dplyr)
library(readr)
library(rgdal)

# Read in example data and province shape files
df <- read_csv("ACLED.csv") %>% select(event_date, latitude, longitude)
province_shapes <- readOGR("provicinial-boundaries")
provinces <- province_shapes$PRV_NAME %>% as.character()
province_shapes$PRV_NAME <- province_shapes$PRV_NAME %>% as.character()

# Geocode observations by province.
df$province <- NA
for(i in 1:length(provinces)){
  spdf <- df
  coordinates(spdf) <- c("longitude", "latitude")
  proj4string(spdf) <- proj4string(province_shapes[i,])
  idx <- !is.na(over(as(spdf, "SpatialPoints"),
                     as(province_shapes[i,], "SpatialPolygons")))
  df$province[idx] <- province_shapes$PRV_NAME[i]
}
df$province %>% table()

# Filter observations to a specific province
df <- df %>% filter(province %in% c("KUNAR", "NANGARHAR"))