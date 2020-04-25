import pandas as pd
import geopandas as gpd
import pygeohash as gh
import folium
from shapely import geometry
import math

# Read in example data and province shape files
df = pd.read_csv("ACLED.csv")
df = df[['event_date', 'latitude', 'longitude']]

# Geohash the data
df['hash'] = df.apply(lambda x: gh.encode(x.latitude, x.longitude, precision = 4), axis = 1)
density = df['hash'].value_counts().reset_index()
density.columns = ['hash', 'count']

def geohash_to_polygon(geo):
    lat_centroid, lng_centroid, lat_offset, lng_offset = gh.decode_exactly(geo)
    corner_1 = (lat_centroid - lat_offset, lng_centroid - lng_offset)[::-1]
    corner_2 = (lat_centroid - lat_offset, lng_centroid + lng_offset)[::-1]
    corner_3 = (lat_centroid + lat_offset, lng_centroid + lng_offset)[::-1]
    corner_4 = (lat_centroid + lat_offset, lng_centroid - lng_offset)[::-1]
    return geometry.Polygon([corner_1, corner_2, corner_3, corner_4, corner_1])
density['geometry'] = density['hash'].apply(geohash_to_polygon)

# Create a leaflet map
density['value'] = density['count'].apply(math.log) 
densityjson = gpd.GeoDataFrame(density, crs="EPSG:4326")
densityjson = densityjson.to_json()

map_center = [34.4,68.7]
map = folium.Map(location=(map_center[0], map_center[1]), 
                 tiles = "Stamen Toner", zoom_start=6)
choropleth = folium.Choropleth(geo_data = densityjson,
                               name = 'choropleth',
                               data = density,
                               columns = ['hash', 'value'],
                               key_on = 'feature.properties.hash',
                               fill_color = 'Reds',
                               fill_opacity = 0.7,
                               line_opacity = 0.2,
                               legend_name = 'Density').add_to(map)
choropleth.geojson.add_child(folium.features.GeoJsonTooltip(['hash', 'count'],labels=False))
choropleth.save("mymap.html")