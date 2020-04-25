import pandas as pd
import geopandas as gpd

# Read in example data and province shape files
df = pd.read_csv("ACLED.csv")
df = df[['event_date', 'latitude', 'longitude']]
province_shapes = gpd.read_file("provicinial-boundaries")
provinces = province_shapes['PRV_NAME'].tolist()

# Geocode observations by province
gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.longitude, df.latitude))
df = []
for value in range(0,len(province_shapes)):    
    temp = gdf[gdf.geometry.intersects(province_shapes.iloc[value]['geometry'])]
    temp = temp.assign(province=province_shapes.iloc[value]['PRV_NAME'])
    df.append(temp)
df = pd.concat(df)
df['province'].value_counts()

# Filter obseravations to a specific province
df = df[df['province'].isin(['KUNAR', 'NANGARHAR'])]
