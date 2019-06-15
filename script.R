# Import

### Import Libraries
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

### Import AirBnB file ### raw_data = read.csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/visualisations/listings.csv", header = TRUE)
raw.data <- read.csv(file="c:/Users/joshl/Desktop/rdata/nyc_airbnb_june1.csv", header = TRUE)

# Data Cleaning

### View data
glimpse(raw.data)
head(raw.data)
str(raw.data)

### Change neighbourhood_group column name to borough
colnames(raw.data)[names(raw.data) == "neighbourhood_group"] = "borough"

### The reviews_per_month column has NAs when there hasn't been any reviews, we'll replace these with 0
raw.data$reviews_per_month[is.na(raw.data$reviews_per_month)] = 0

### Confirm there are any NAs left in the data
anyNA(raw.data)

# Subset Data

### Subset Entire home/apt in Manhattan that are between 100-400$ a night
data <- subset(raw.data, price < 300 & borough == "Manhattan" & room_type == "Entire home/apt")

### View distribution of response variable
ggplot(data = data) +
  geom_histogram(mapping = aes(price), bins = 16)

# Spatial Visualization

### Get a map of Manhattan for visualizations
map <- ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("Manhattan")$bbox))), zoom = 12))

### plot the data
map + geom_point(data = data, aes(x = longitude, y =latitude, colour = factor(neighbourhood)),
             alpha = 0.1) + guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))

### Density map
map + stat_density2d(mapping = aes(x = longitude, y =latitude, fill = ..level.., 
                               alpha = ..level..), 
                 size = 0.01, bins = 30,geom = "polygon", data = data) + 
  scale_fill_gradient(low = "dark blue", high = "orange") +
  scale_alpha(range = c(0.05, 0.3), guide = FALSE)

# Analyse distrubutions and correlations 

### Create a new data set with just numerical factors
num.data <- data[, c(-2, -4, -5, -6, -9, -13)]

### Check correlations
corrplot(cor(num.data), method = "square")

### Check VIF
simple.lm <- lm(price ~ ., data = num.data)
vif(simple.lm)