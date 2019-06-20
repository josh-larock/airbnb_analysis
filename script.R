# I went to NYC earlier this year and was blown away. I want to go back and show my girlfriend
# Jordan the city. I want to find a one bedroom airbnb in either Manhattan or Brooklyn. I 
# will build a model to tell us which neighbourhoods we should be looking into given critiria
# such as price, reviews etc.

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
library(data.table)

### Import AirBnB file
raw.data <- fread("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/data/listings.csv.gz", header = TRUE)

### Check Dimension of Data
dim(raw.data)
glimpse(raw.data)
head(raw.data)
str(raw.data)

# Data Cleaning

### Check how many NAs are in each column
na.count <- sapply(raw.data, function(y) sum(length(which(is.na(y)))))
na.count[na.count > 0]

### Check how many empty strings are in each column
blank.count <- sapply(raw.data, function(y) sum(length(which(y == ""))))
blank.count[blank.count > 0]

### Remove useless and columns
data <- subset(raw.data, select = c(price, neighbourhood_group_cleansed,
                                    neighbourhood_cleansed, latitude, longitude, property_type,
                                    room_type, accommodates, bathrooms, beds, bed_type, host_is_superhost,
                                    cleaning_fee, guests_included, extra_people, minimum_nights,
                                    number_of_reviews, review_scores_rating, review_scores_accuracy,
                                    review_scores_cleanliness, review_scores_checkin,
                                    review_scores_communication, review_scores_location,
                                    review_scores_value, calculated_host_listings_count))

### Rename variables
names(data) <-  c("price", "borough", "neighbourhood", "latitude", "longitude", "property_type",
                 "room_type", "accommodates", "bathrooms", "beds", "bed_type", "superhost",
                 "cleaning_fee", "guests", "extra_people", "minimum_nights","number_of_reviews",
                 "rev_overall", "rev_accuracy", "rev_cleanliness", "rev_checkin","rev_communication",
                 "rev_location", "rev_value", "host_listings")

### Subset the data for our problem

### Convert empty strings in cleaning_fee to $0
data$cleaning_fee[data$cleaning_fee == ""] <- "$0.00"

### Convert price, cleaning_fee and extra_people from chr to numeric 
data$price <- as.numeric(gsub("\\$|,", "", data$price))
data$cleaning_fee <- as.numeric(gsub("\\$|,", "", data$cleaning_fee))
data$extra_people <- as.numeric(gsub("\\$|,", "", data$extra_people))

# Convert from chr to factor
data$neighbourhood <- as.factor(data$neighbourhood)
data$borough <- as.factor(data$borough)
data$property_type <- as.factor(data$property_type)
data$room_type <- as.factor(data$room_type)
data$bed_type <- as.factor(data$bed_type)

# Convert from chr to logical
data$superhost[data$superhost == "t"] <- TRUE
data$superhost[data$superhost == "f"] <- FALSE
data$superhost <- as.logical(data$superhost)

### Clear any entries with NAs left
data <- data[complete.cases(data)]
dim(data)

# Spatial Visualization

### Get a map of Manhattan for visualizations
map <- ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("New York City")$bbox))), zoom = 13))

### Density map
map + stat_density2d(mapping = aes(x = longitude, y =latitude, fill = ..level.., 
                                   alpha = ..level..), 
                     size = 0.01, bins = 30,geom = "polygon", data = data) + 
  scale_fill_gradient(low = "dark blue", high = "orange") +
  scale_alpha(range = c(0.05, 0.3), guide = FALSE)

# Subset the data to accomedate 2 people, 1 bed in eitherv Manhattan or Brooklyn

# Analyse distrubutions and correlations 

### Create a new data set with just numerical factors
num.data <- data[, c(-2, -3, -6, -7, -11, -12)]

### Check correlations
corrplot(cor(num.data), method = "square")

### Check VIF
simple.lm <- lm(price ~ ., data = num.data)
vif(simple.lm)

# Feature Engineering
