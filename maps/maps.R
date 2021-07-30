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
library(hms)
library(gganimate)

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

feb_counts_data$pickup_level = vector("integer", length = dim(feb_counts_data)[1])
feb_counts_data$dropoff_level = vector("integer", length = dim(feb_counts_data)[1])

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

# get bounds bounds for map legend and groups (use the same for all 6 maps)
pickup_quantiles_feb = quantile(feb_counts_data$pickup_count) 
pickup_quantiles_mar = quantile(mar_counts_data$pickup_count)
pickup_quantiles_apr = quantile(apr_counts_data$pickup_count)
mean_pickup_quantiles = (pickup_quantiles_apr + pickup_quantiles_mar + pickup_quantiles_feb) / 3

dropoff_quantiles_feb = quantile(feb_counts_data$dropoff_count)
mar_counts_data[is.na(mar_counts_data)] = 0
dropoff_quantiles_mar = quantile(mar_counts_data$dropoff_count)
dropoff_quantiles_apr = quantile(apr_counts_data$dropoff_count)
mean_dropoff_quantiles = (dropoff_quantiles_apr + dropoff_quantiles_mar + dropoff_quantiles_feb) / 3


for (i in 1:dim(feb_counts_data)[1]) {
  if (feb_counts_data$pickup_count[i] <= mean_pickup_quantiles[2]) {
    feb_counts_data$pickup_level[i] = 1
    
  } else if (feb_counts_data$pickup_count[i] > mean_pickup_quantiles[2] && feb_counts_data$pickup_count[i] <= mean_pickup_quantiles[3]) {
    feb_counts_data$pickup_level[i] = 2  
    
  } else if (feb_counts_data$pickup_count[i] > mean_pickup_quantiles[3] && feb_counts_data$pickup_count[i] <= mean_pickup_quantiles[4]) {
    feb_counts_data$pickup_level[i] = 3
    
  } else if (feb_counts_data$pickup_count[i] > mean_pickup_quantiles[4] && feb_counts_data$pickup_count[i] <= mean_pickup_quantiles[5]) {
    feb_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(feb_counts_data)[1]) {
  if (feb_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[2]) {
    feb_counts_data$dropoff_level[i] = 1
    
  } else if (feb_counts_data$dropoff_count[i] > mean_dropoff_quantiles[2] && feb_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[3]) {
    feb_counts_data$dropoff_level[i] = 2  
    
  } else if (feb_counts_data$dropoff_count[i] > mean_dropoff_quantiles[3] && feb_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[4]) {
    feb_counts_data$dropoff_level[i] = 3
    
  } else if (feb_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4] && feb_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[5]) {
    feb_counts_data$dropoff_level[i] = 4  
  }
}

feb_counts_data[feb_counts_data$pickup_level == 0,] = 1
feb_counts_data[feb_counts_data$dropoff_level == 0,] = 1
feb_counts_data[order(feb_counts_data$PULocationID),]

nyc_boundary_feb = nyc_boundary %>% left_join(feb_counts_data, by = c('LocationID' = "PULocationID"))

### March ###


for (i in 1:dim(mar_counts_data)[1]) {
  if (mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[2]) {
    mar_counts_data$pickup_level[i] = 1
    
  } else if (mar_counts_data$pickup_count[i] > mean_pickup_quantiles[2] && mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[3]) {
    mar_counts_data$pickup_level[i] = 2  
    
  } else if (mar_counts_data$pickup_count[i] > mean_pickup_quantiles[3] && mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[4]) {
    mar_counts_data$pickup_level[i] = 3
    
  } else if (mar_counts_data$pickup_count[i] > mean_pickup_quantiles[4] && mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[5]) {
    mar_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(mar_counts_data)[1]) {
  if (mar_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[2]) {
    mar_counts_data$dropoff_level[i] = 1
    
  } else if (mar_counts_data$dropoff_count[i] > mean_dropoff_quantiles[2] && mar_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[3]) {
    mar_counts_data$dropoff_level[i] = 2  
    
  } else if (mar_counts_data$dropoff_count[i] > mean_dropoff_quantiles[3] && mar_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[4]) {
    mar_counts_data$dropoff_level[i] = 3
    
  } else if (mar_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4] && mar_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[5]) {
    mar_counts_data$dropoff_level[i] = 4  
  }
}

mar_counts_data[mar_counts_data$pickup_level == 0,] = 1
mar_counts_data[mar_counts_data$dropoff_level == 0,] = 1
mar_counts_data[order(mar_counts_data$PULocationID),]

nyc_boundary_mar = nyc_boundary %>% left_join(mar_counts_data, by = c('LocationID' = "PULocationID"))

### April ###
for (i in 1:dim(apr_counts_data)[1]) {
  if (apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[2]) {
    apr_counts_data$pickup_level[i] = 1
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[2] && apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[3]) {
    apr_counts_data$pickup_level[i] = 2  
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[3] && apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[4]) {
    apr_counts_data$pickup_level[i] = 3
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[4] && apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[5]) {
    apr_counts_data$pickup_level[i] = 4  
  }
}

for (i in 1:dim(apr_counts_data)[1]) {
  if (apr_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[2]) {
    apr_counts_data$dropoff_level[i] = 1
    
  } else if (apr_counts_data$dropoff_count[i] > mean_dropoff_quantiles[2] && apr_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[3]) {
    apr_counts_data$dropoff_level[i] = 2  
    
  } else if (apr_counts_data$dropoff_count[i] > mean_dropoff_quantiles[3] && apr_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[4]) {
    apr_counts_data$dropoff_level[i] = 3
    
  } else if (apr_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4] && apr_counts_data$dropoff_count[i] <= mean_dropoff_quantiles[5]) {
    apr_counts_data$dropoff_level[i] = 4  
  }
}

apr_counts_data[apr_counts_data$pickup_level == 0,] = 1
apr_counts_data[apr_counts_data$dropoff_level == 0,] = 1
apr_counts_data[order(apr_counts_data$PULocationID),]

nyc_boundary_apr = nyc_boundary %>% left_join(apr_counts_data, by = c('LocationID' = "PULocationID"))

### Maps! ###

map1 = ggplot() + 
  geom_sf(data = nyc_boundary_feb, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_feb, aes(fill = pickup_level)) +
  ggtitle("Pickup February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map2 = ggplot() + 
  geom_sf(data = nyc_boundary_feb, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_feb, aes(fill = dropoff_level)) +
  ggtitle("Dropoff February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map3 = ggplot() + 
  geom_sf(data = nyc_boundary_mar, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_mar, aes(fill = pickup_level)) +
  ggtitle("Pickup March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map4 = ggplot() + 
  geom_sf(data = nyc_boundary_mar, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_mar, aes(fill = dropoff_level)) +
  ggtitle("Dropoff March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map5 = ggplot() + 
  geom_sf(data = nyc_boundary_apr, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_apr, aes(fill = pickup_level)) +
  ggtitle("Pickup April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "right")

map6 = ggplot() + 
  geom_sf(data = nyc_boundary_apr, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary_apr, aes(fill = dropoff_level)) +
  ggtitle("Dropoff April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "green",
    high = "red",
    limits = c(0, 4),
    na.value = "grey50",
    labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "right")

## GG arrage it or something
grid.arrange(map1, map3, map5, map2, map4, map6, ncol = 3)

# animated map
ggplot(data = apr_20[1:50000,], mapping = aes(tpep_pickup_datetime, fare_amount)) +
  geom_line()

################################### 
# Fix!!

p <- ggplot(apr_20, aes(x = fare_amount, y = PULocationID, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p
