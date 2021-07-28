####################### New York Map ##########################

### Libraries ###
library(sf)
library(ggmap)
library(ggplot2)
library(dplyr)

### Data and Polygons ###
feb_20 = read.csv("../data/yellow_tripdata_2020-02.csv")
nyc_boundary = st_read("../shapefiles/taxi_zones.shp")

counts_data = feb_20 %>% group_by(PULocationID) %>% mutate(count = n()) %>% select(PULocationID, count) %>% unique()
quantiles = quantile(counts_data$count)
counts_data$level = vector("integer", length = dim(counts_data)[1])

for (i in 1:dim(counts_data)[1]) {
  if (counts_data$count[i] <= quantiles[2]) {
    counts_data$level[i] = 1
  
  } else if (counts_data$count[i] > quantiles[2] && counts_data$count[i] <= quantiles[3]) {
    counts_data$level[i] = 2  
  
  } else if (counts_data$count[i] > quantiles[3] && counts_data$count[i] <= quantiles[4]) {
    counts_data$level[i] = 3
  
  } else if (counts_data$count[i] > quantiles[4] && counts_data$count[i] <= quantiles[5]) {
    counts_data$level[i] = 4  
  }
}

nyc_boundary = nyc_boundary %>% left_join(counts_data, by = c('LocationID' = "PULocationID"))

ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = level)) +
  ggtitle("NYC Taxi Rides February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50"
  )

counts_data
