####################### New York Map ##########################
# Libraries
library(gapminder)
library(sf)
library(ggmap)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(grid)
library(lattice)
library(lubridate)
library(gridExtra)
library(hms)
library(gganimate)

### Data and Polygons ###
feb_20 = read.csv("../data/yellow_tripdata_2020-02.csv")
mar_20 = read.csv("../data/yellow_tripdata_2020-03.csv")
apr_20 = read.csv("../data/yellow_tripdata_2020-04.csv?")
nyc_boundary = st_read("../shapefiles/taxi_zones.shp")

# add counts
### February ###
feb_counts_data = data.frame('LocationID' = 1:263)
feb_counts_data = feb_counts_data %>% left_join(
  feb_20 %>%
    group_by(PULocationID) %>%
    mutate(pickup_count = n()) %>%
    select(PULocationID, pickup_count) %>%
    unique(),
  by = c('LocationID' = 'PULocationID')
)

feb_counts_data = feb_counts_data %>% left_join(
  feb_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('LocationID' = 'DOLocationID')
)

feb_counts_data$pickup_level = vector("integer", length = dim(nyc_boundary)[1])
feb_counts_data$dropoff_level = vector("integer", length = dim(nyc_boundary)[1])

mar_counts_data = data.frame('LocationID' = 1:263)
mar_counts_data = mar_counts_data %>% left_join(
  mar_20 %>%
    group_by(PULocationID) %>%
    mutate(pickup_count = n()) %>%
    select(PULocationID, pickup_count) %>%
    unique(),
  by = c('LocationID' = 'PULocationID')
)

mar_counts_data = mar_counts_data %>% left_join(
  mar_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('LocationID' = 'DOLocationID')
)

mar_counts_data$pickup_level = vector("integer", length = dim(nyc_boundary)[1])
mar_counts_data$dropoff_level = vector("integer", length = dim(nyc_boundary)[1])

apr_counts_data = data.frame('LocationID' = 1:263)
apr_counts_data = apr_counts_data %>% left_join(
  apr_20 %>%
    group_by(PULocationID) %>%
    mutate(pickup_count = n()) %>%
    select(PULocationID, pickup_count) %>%
    unique(),
  by = c('LocationID' = 'PULocationID')
)

apr_counts_data = apr_counts_data %>% left_join(
  apr_20 %>%
    group_by(DOLocationID) %>%
    mutate(dropoff_count = n()) %>%
    select(DOLocationID, dropoff_count) %>%
    unique(),
  by = c('LocationID' = 'DOLocationID')
)

apr_counts_data$pickup_level = vector("integer", length = dim(nyc_boundary)[1])
apr_counts_data$dropoff_level = vector("integer", length = dim(nyc_boundary)[1])

# get bounds bounds for map legend and groups (use the same for all 6 maps)
feb_counts_data[is.na(feb_counts_data)] = 1
mar_counts_data[is.na(mar_counts_data)] = 1
apr_counts_data[is.na(apr_counts_data)] = 1

pickup_quantiles_feb = quantile(feb_counts_data$pickup_count) 
pickup_quantiles_mar = quantile(mar_counts_data$pickup_count)
pickup_quantiles_apr = quantile(apr_counts_data$pickup_count)
mean_pickup_quantiles = (pickup_quantiles_apr + pickup_quantiles_mar + pickup_quantiles_feb) / 3

dropoff_quantiles_feb = quantile(feb_counts_data$dropoff_count)
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
    
  } else if (feb_counts_data$pickup_count[i] > mean_pickup_quantiles[4]) {
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
    
  } else if (feb_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4]) {
    feb_counts_data$dropoff_level[i] = 4  
  }
}

# feb_counts_data[feb_counts_data$pickup_level == 0,] = 1
# feb_counts_data[feb_counts_data$dropoff_level == 0,] = 1
feb_counts_data = feb_counts_data[order(feb_counts_data$LocationID),]

nyc_boundary_feb = nyc_boundary %>% left_join(feb_counts_data)

### March ###


for (i in 1:dim(mar_counts_data)[1]) {
  if (mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[2]) {
    mar_counts_data$pickup_level[i] = 1
    
  } else if (mar_counts_data$pickup_count[i] > mean_pickup_quantiles[2] && mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[3]) {
    mar_counts_data$pickup_level[i] = 2  
    
  } else if (mar_counts_data$pickup_count[i] > mean_pickup_quantiles[3] && mar_counts_data$pickup_count[i] <= mean_pickup_quantiles[4]) {
    mar_counts_data$pickup_level[i] = 3
    
  } else if (mar_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4]) {
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
    
  } else if (mar_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4]) {
    mar_counts_data$dropoff_level[i] = 4
    
  }
}

# mar_counts_data[mar_counts_data$pickup_level == 0,] = 1
# mar_counts_data[mar_counts_data$dropoff_level == 0,] = 1
mar_counts_data = mar_counts_data[order(mar_counts_data$LocationID),]

nyc_boundary_mar = nyc_boundary %>% left_join(mar_counts_data)

### April ###
for (i in 1:dim(apr_counts_data)[1]) {
  if (apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[2]) {
    apr_counts_data$pickup_level[i] = 1
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[2] && apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[3]) {
    apr_counts_data$pickup_level[i] = 2  
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[3] && apr_counts_data$pickup_count[i] <= mean_pickup_quantiles[4]) {
    apr_counts_data$pickup_level[i] = 3
    
  } else if (apr_counts_data$pickup_count[i] > mean_pickup_quantiles[4]) {
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
    
  } else if (apr_counts_data$dropoff_count[i] > mean_dropoff_quantiles[4]) {
    apr_counts_data$dropoff_level[i] = 4
  }
}

# apr_counts_data[apr_counts_data$pickup_level == 0,] = 1
# apr_counts_data[apr_counts_data$dropoff_level == 0,] = 1
apr_counts_data = apr_counts_data[order(apr_counts_data$LocationID),]

nyc_boundary_apr = nyc_boundary %>% left_join(apr_counts_data)

### Maps! ###
limits = c(0, max(feb_counts_data$pickup_count))

map1 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = feb_counts_data$pickup_count)) +
  ggtitle("Pickup February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map2 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = feb_counts_data$dropoff_count)) +
  ggtitle("Dropoff February 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map3 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = mar_counts_data$pickup_count)) +
  ggtitle("Pickup March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map4 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = mar_counts_data$dropoff_count)) +
  ggtitle("Dropoff March 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "bottom")

map5 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = apr_counts_data$pickup_count)) +
  ggtitle("Pickup April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

map6 = ggplot() + 
  geom_sf(data = nyc_boundary, size = 1, color = "gray") +
  geom_sf(data = nyc_boundary, aes(fill = apr_counts_data$dropoff_count)) +
  ggtitle("Dropoff April 2020") +
  coord_sf() +
  scale_fill_continuous(
    name = "Counts",
    low = "yellow",
    high = "red",
    limits = limits,
    # breaks = breaks,
    na.value = "grey50",
    # labels = c("Min", "25%", "Median", "75%", "Max")
  ) + 
  theme(legend.position = "none")

grid.arrange(map1, map3, map5, map2, map4, map6, ncol = 3)

#binned data
feb_20_bins = feb_20 %>%
  mutate(time = ymd_hms(tpep_pickup_datetime)) %>%
  group_by(PULocationID, time = floor_date(time, "1 day")) %>%
  summarise(counts = n()) %>%
  filter(time >= as.Date("2020-02-01")) %>%
  filter(time <= as.Date("2020-02-29"))

mar_20_bins = mar_20 %>%
  mutate(time = ymd_hms(tpep_pickup_datetime)) %>%
  group_by(PULocationID, time = floor_date(time, "1 day")) %>%
  summarise(counts = n()) %>%
  filter(time >= as.Date("2020-03-01")) %>%
  filter(time <= as.Date("2020-03-31"))

apr_20_bins = apr_20 %>%
  mutate(time = ymd_hms(tpep_pickup_datetime)) %>%
  group_by(PULocationID, time = floor_date(time, "1 day")) %>%
  summarise(counts = n()) %>%
  filter(time >= as.Date("2020-04-01")) %>%
  filter(time <= as.Date("2020-04-30"))

# Animation
data = rbind(feb_20_bins, mar_20_bins, apr_20_bins) %>% 
  filter(PULocationID %in% c(161, 237, 236, 162, 230, 186, 234, 170, 48, 142))

for (i in 1:dim(data)[1]) {
  if (data$PULocationID[i] == 161) {
    data$location[i] = "Midtown Center"
    
  } else if (data$PULocationID[i] == 237) {
    data$location[i] = "Upper East Side South"
    
  } else if (data$PULocationID[i] == 236) {
    data$location[i] = "Upper East Side North"
    
  } else if (data$PULocationID[i] == 162) {
    data$location[i] = "Midtown East"
    
  } else if (data$PULocationID[i] == 230) {
    data$location[i] = "Time Square"
    
  } else if (data$PULocationID[i] == 186) {
    data$location[i] = "Penn Station"
    
  } else if (data$PULocationID[i] == 234) {
    data$location[i] = "Union Square"
    
  } else if (data$PULocationID[i] == 170) {
    data$location[i] = "Murray Hill"
    
  } else if (data$PULocationID[i] == 48) {
    data$location[i] = "Clinton East"
    
  } else if (data$PULocationID[i] == 142) {
    data$location[i] = "Lincoln Square East"
    
  }
}

# Plot
data %>%
  ggplot( aes(x=time, y=counts, group=location, color=location)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of Taxi Rides in the top 10 districts in NYC (Feb 2020-Apr 2020)") +
  ylab("Daily Number of Taxi Rides") +
  xlab("Time (days)") +
  scale_colour_brewer(name = "Pick-up Taxi District", palette = "Spectral")

data %>%
  ggplot( aes(x=time, y=counts, group=location, color=location)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of Taxi Rides in the top 10 districts in NYC (Feb 2020-Apr 2020)") +
  ylab("Daily Number of Taxi Rides") +
  xlab("Time (days)") +
  scale_colour_brewer(name = "Pick-up Taxi District", palette = "Spectral") +
  transition_reveal(time)

anim_save("rides.gif")
