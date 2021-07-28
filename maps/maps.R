####################### New York Map ##########################

### Libraries ###
library(sf)
library(ggmap)
library(ggplot2)
library(dplyr)


### Data and Polygons ###
feb_20 = read.csv("../data/yellow_tripdata_2020-02.csv")
nyc_boundary = st_read("../shapefiles/taxi_zones.shp")
str(nyc_boundary)

head(feb_20$PULocationID)
head(feb_20$DOLocationID)
head(nyc_boundary$LocationID)

counts_data = feb_20 %>% group_by(PULocationID) %>% mutate(count = n()) %>% select(PULocationID, count) %>% unique()
nyc_boundary = nyc_boundary %>% left_join(counts_data, by = c('LocationID' = "PULocationID"))

ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray", aes(fill = count)) +
  ggtitle("NYC boundaries") +
  coord_sf() +
  geom_point(data = feb_20)
