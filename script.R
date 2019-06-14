# Import Libraries

library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)

#Import AirBnB File
raw_data = read.csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/visualisations/listings.csv", header = TRUE)

# Get an initial feel for the dataset
glimpse(raw_data)
head(raw_data)
str(raw_data)

### Based on what we saw, I'm going to make the following changes ###

# The neighbourhood_group column is the borough, so we'll change the name to make that clear 
colnames(raw_data)[names(raw_data) == "neighbourhood_group"] = "borough"

# The reviews_per_month column has NAs when there hasn't been any reviews, we'll replace these with 0
raw_data$reviews_per_month[is.na(raw_data$reviews_per_month)] = 0

### Let's make a new variable, the dataset without the factors and check out some correlations
### And distributions

# Create a new variable listings_num_only 
num_only = subset(raw_data, select = -c(2, 4, 5, 6, 9, 13))
cor_num_only = cor(num_only)
corrplot(cor_num_only, method = "square", type = "lower")

