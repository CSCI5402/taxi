####################### New York Map ##########################

### Libraries ###
library(sf)
library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(lattice)
library(gridExtra)

### Data and Polygons ###
feb_20 = read.csv("../data/yellow_tripdata_2020-02.csv")
mar_20 = read.csv("../data/yellow_tripdata_2020-03.csv")
apr_20 = read.csv("../data/yellow_tripdata_2020-04.csv")
nyc_boundary = st_read("../shapefiles/taxi_zones.shp")

# add counts
### February ###
feb_counts_data = feb_20 %>%
  group_by(PULocationID) %>%
  mutate(pickup_count = n()) %>%
  select(PULocationID, pickup_count) %>%
  unique() 

feb_counts_data = feb_counts_data %>% left_join(
  feb_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('PULocationID' = 'DOLocationID')
)

# get bounds bounds for map legend and groups (use the same for all 6 maps)
quantiles = quantile(feb_counts_data$pickup_count)

feb_counts_data$pickup_level = vector("integer", length = dim(feb_counts_data)[1])
feb_counts_data$dropoff_level = vector("integer", length = dim(feb_counts_data)[1])

for (i in 1:dim(feb_counts_data)[1]) {
  if (feb_counts_data$pickup_count[i] <= quantiles[2]) {
    feb_counts_data$pickup_level[i] = 1
    
  } else if (feb_counts_data$pickup_count[i] > quantiles[2] && feb_counts_data$pickup_count[i] <= quantiles[3]) {
    feb_counts_data$pickup_level[i] = 2  
    
  } else if (feb_counts_data$pickup_count[i] > quantiles[3] && feb_counts_data$pickup_count[i] <= quantiles[4]) {
    feb_counts_data$pickup_level[i] = 3
    
  } else if (feb_counts_data$pickup_count[i] > quantiles[4] && feb_counts_data$pickup_count[i] <= quantiles[5]) {
    feb_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(feb_counts_data)[1]) {
  if (feb_counts_data$dropoff_count[i] <= quantiles[2]) {
    feb_counts_data$dropoff_level[i] = 1
    
  } else if (feb_counts_data$dropoff_count[i] > quantiles[2] && feb_counts_data$dropoff_count[i] <= quantiles[3]) {
    feb_counts_data$dropoff_level[i] = 2  
    
  } else if (feb_counts_data$dropoff_count[i] > quantiles[3] && feb_counts_data$dropoff_count[i] <= quantiles[4]) {
    feb_counts_data$dropoff_level[i] = 3
    
  } else if (feb_counts_data$dropoff_count[i] > quantiles[4] && feb_counts_data$dropoff_count[i] <= quantiles[5]) {
    feb_counts_data$dropoff_level[i] = 4  
  }
}

nyc_boundary_feb = nyc_boundary %>% left_join(feb_counts_data, by = c('LocationID' = "PULocationID"))

### March ###

mar_counts_data = mar_20 %>%
  group_by(PULocationID) %>%
  mutate(pickup_count = n()) %>%
  select(PULocationID, pickup_count) %>%
  unique()

mar_counts_data = mar_counts_data %>% left_join(
  mar_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('PULocationID' = 'DOLocationID')
)

mar_counts_data$pickup_level = vector("integer", length = dim(mar_counts_data)[1])
mar_counts_data$dropoff_level = vector("integer", length = dim(mar_counts_data)[1])

for (i in 1:dim(mar_counts_data)[1]) {
  if (mar_counts_data$pickup_count[i] <= quantiles[2]) {
    mar_counts_data$pickup_level[i] = 1
    
  } else if (mar_counts_data$pickup_count[i] > quantiles[2] && mar_counts_data$pickup_count[i] <= quantiles[3]) {
    mar_counts_data$pickup_level[i] = 2  
    
  } else if (mar_counts_data$pickup_count[i] > quantiles[3] && mar_counts_data$pickup_count[i] <= quantiles[4]) {
    mar_counts_data$pickup_level[i] = 3
    
  } else if (mar_counts_data$pickup_count[i] > quantiles[4] && mar_counts_data$pickup_count[i] <= quantiles[5]) {
    mar_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(mar_counts_data)[1]) {
  if (mar_counts_data$dropoff_count[i] <= quantiles[2]) {
    mar_counts_data$dropoff_level[i] = 1
    
  } else if (mar_counts_data$dropoff_count[i] > quantiles[2] && mar_counts_data$dropoff_count[i] <= quantiles[3]) {
    mar_counts_data$dropoff_level[i] = 2  
    
  } else if (mar_counts_data$dropoff_count[i] > quantiles[3] && mar_counts_data$dropoff_count[i] <= quantiles[4]) {
    mar_counts_data$dropoff_level[i] = 3
    
  } else if (mar_counts_data$dropoff_count[i] > quantiles[4] && mar_counts_data$dropoff_count[i] <= quantiles[5]) {
    mar_counts_data$dropoff_level[i] = 4  
  }
}

nyc_boundary_mar = nyc_boundary %>% left_join(mar_counts_data, by = c('LocationID' = "PULocationID"))

### April ###

apr_counts_data = apr_20 %>%
  group_by(PULocationID) %>%
  mutate(pickup_count = n()) %>%
  select(PULocationID, pickup_count) %>%
  unique

apr_counts_data = apr_counts_data %>% left_join(
  apr_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('PULocationID' = 'DOLocationID')
)

apr_counts_data$pickup_level = vector("integer", length = dim(apr_counts_data)[1])
apr_counts_data$dropoff_level = vector("integer", length = dim(apr_counts_data)[1])

for (i in 1:dim(apr_counts_data)[1]) {
  if (apr_counts_data$pickup_count[i] <= quantiles[2]) {
    apr_counts_data$pickup_level[i] = 1
    
  } else if (apr_counts_data$pickup_count[i] > quantiles[2] && apr_counts_data$pickup_count[i] <= quantiles[3]) {
    apr_counts_data$pickup_level[i] = 2  
    
  } else if (apr_counts_data$pickup_count[i] > quantiles[3] && apr_counts_data$pickup_count[i] <= quantiles[4]) {
    apr_counts_data$pickup_level[i] = 3
    
  } else if (apr_counts_data$pickup_count[i] > quantiles[4] && apr_counts_data$pickup_count[i] <= quantiles[5]) {
    apr_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(apr_counts_data)[1]) {
  if (apr_counts_data$dropoff_count[i] <= quantiles[2]) {
    apr_counts_data$dropoff_level[i] = 1
    
  } else if (apr_counts_data$dropoff_count[i] > quantiles[2] && apr_counts_data$dropoff_count[i] <= quantiles[3]) {
    apr_counts_data$dropoff_level[i] = 2  
    
  } else if (apr_counts_data$dropoff_count[i] > quantiles[3] && apr_counts_data$dropoff_count[i] <= quantiles[4]) {
    apr_counts_data$dropoff_level[i] = 3
    
  } else if (apr_counts_data$dropoff_count[i] > quantiles[4] && apr_counts_data$dropoff_count[i] <= quantiles[5]) {
    apr_counts_data$dropoff_level[i] = 4  
  }
}

nyc_boundary_apr = nyc_boundary %>% left_join(apr_counts_data, by = c('LocationID' = "PULocationID"))

### Maps! ###

map1 = ggplot() + 
  geom_sf(data = nyc_boundary_feb, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_feb, aes(fill = pickup_level)) +
  ggtitle("Pickup Locations February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

map2 = ggplot() + 
  geom_sf(data = nyc_boundary_feb, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_feb, aes(fill = dropoff_level)) +
  ggtitle("Dropoff Locations February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

map3 = ggplot() + 
  geom_sf(data = nyc_boundary_mar, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_mar, aes(fill = pickup_level)) +
  ggtitle("Pickup Locations March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

map4 = ggplot() + 
  geom_sf(data = nyc_boundary_mar, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_mar, aes(fill = dropoff_level)) +
  ggtitle("Dropoff Locations March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

map5 = ggplot() + 
  geom_sf(data = nyc_boundary_apr, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_apr, aes(fill = pickup_level)) +
  ggtitle("Pickup Locations April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

map6 = ggplot() + 
  geom_sf(data = nyc_boundary_apr, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_apr, aes(fill = dropoff_level)) +
  ggtitle("Dropoff Locations April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Number of Rides",
    low = "yellow",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c(quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  ) + 
  theme(legend.position = "none")

## GG arrage it or something
grid.arrange(map1, map3, map5, map2, map4, map6, ncol = 3)

