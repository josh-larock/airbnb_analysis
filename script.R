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
#raw.data <- fread("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/data/listings.csv.gz", header = TRUE)

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

### Remove columns with a ton of NAs
data <- subset(raw.data, select = c(host_is_superhost, neighbourhood_group_cleansed,
                                    neighbourhood_cleansed, latitude, longitude, property_type,
                                    room_type, accommodates, bathrooms, beds, bed_type, price,
                                    cleaning_fee, guests_included, extra_people, minimum_nights,
                                    number_of_reviews, review_scores_rating, review_scores_accuracy,
                                    review_scores_cleanliness, review_scores_checkin,
                                    review_scores_communication, review_scores_location,
                                    review_scores_value, calculated_host_listings_count))

### Convert empty strings in cleaning_fee to $0
data$cleaning_fee[data$cleaning_fee == ""] <- "$0.00"

### Convert price, cleaning_fee and extra_people from chr to numeric 
data$price <- as.numeric(gsub("\\$|,", "", data$price))
data$cleaning_fee <- as.numeric(gsub("\\$|,", "", data$cleaning_fee))
data$extra_people <- as.numeric(gsub("\\$|,", "", data$extra_people))

### Clear any entries with NAs left
data <- data[complete.cases(data)]
dim(data)

# Spatial Visualization

### Get a map of Manhattan for visualizations
map <- ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("New York City")$bbox))), zoom = 13))

### plot the data
#map + geom_point(data = data, aes(x = longitude, y =latitude, colour = factor(neighbourhood)),
#                 alpha = 0.1) + guides(colour = guide_legend(override.aes = list(alpha = 1))) +
# theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))

### Density map
map + stat_density2d(mapping = aes(x = longitude, y =latitude, fill = ..level.., 
                                   alpha = ..level..), 
                     size = 0.01, bins = 30,geom = "polygon", data = data) + 
  scale_fill_gradient(low = "dark blue", high = "orange") +
  scale_alpha(range = c(0.05, 0.3), guide = FALSE)

# Analyse distrubutions and correlations 

### Create a new data set with just numerical factors
#num.data <- data[, c(-2, -4, -5, -6, -9, -13)]

### Check correlations
#corrplot(cor(num.data), method = "square")

### Check VIF
#simple.lm <- lm(price ~ ., data = num.data)
#vif(simple.lm)

# Feature Engineering

# Predictive Modelling

###