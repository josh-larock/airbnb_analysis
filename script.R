# Import Libraries
library(dplyr)
library(ggplot2)
library(ggmap)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(tmaptools)
library(viridis)

# Import AirBnB file ### raw_data = read.csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/visualisations/listings.csv", header = TRUE)
raw_data <- read.csv(file="c:/Users/joshl/Desktop/rdata/nyc_airbnb_june1.csv", header = TRUE)

# View data
glimpse(raw_data)
head(raw_data)
str(raw_data)

# Change neighbourhood_group column name to borough
colnames(raw_data)[names(raw_data) == "neighbourhood_group"] = "borough"

# The reviews_per_month column has NAs when there hasn't been any reviews, we'll replace these with 0
raw_data$reviews_per_month[is.na(raw_data$reviews_per_month)] = 0

# Confirm there are any NAs left in the data
anyNA(raw_data)

# View distributions 
ggplot(data = raw_data) +
  facet_grid(room_type ~ borough) +
  geom_histogram(mapping = aes(price), bins = 30)

# Subset Entire home/apt in Manhattan that are between 100-400$ a night
data <- subset(raw_data, price > 100 & price < 400 & borough == "Manhattan" & room_type == "Entire home/apt")

# plot the rentals geoghraphically
ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("Manhattan")$bbox))), zoom = 12)) +
  geom_point(data = data, aes(x = longitude, y =latitude, colour = factor(data$neighbourhood)),
             alpha = .1, size = 1)

# Density map
ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("Manhattan")$bbox))), zoom = 12)) +
  stat_density2d(mapping = aes(x = longitude, y =latitude, fill = ..level.., 
                               alpha = ..level..), 
                 size = 0.01, bins = 30,geom = "polygon", data = data) + 
  scale_fill_gradient(low = "dark blue", high = "orange") +
  scale_alpha(range = c(0.05, 0.3), guide = FALSE)

# Let's take a closer look
ggplot(data = data) +
  geom_histogram(mapping = aes(price), bins = 12)

# Create a new data set with just numerical factors
num_data <- data[, c(-2, -4, -5, -6, -9, -13)]

# Check correlations (remove factors to do this)
corrplot(cor(num_data), method = "square")

# Check VIF
simple_lm <- lm(price ~ ., data = num_data)
vif(simple_lm)