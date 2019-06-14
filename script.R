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

# To Publish

#raw_data = read.csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-06-02/visualisations/listings.csv", header = TRUE)
raw_data = read.csv(file="c:/Users/joshl/Desktop/rdata/nyc_airbnb_june1.csv", header = TRUE)

# Get an initial feel for the dataset
glimpse(raw_data)
head(raw_data)
str(raw_data)

### Based on what we saw, I'm going to make the following changes ###

# The neighbourhood_group column is the borough, so we'll change the name to make that clear 
colnames(raw_data)[names(raw_data) == "neighbourhood_group"] = "borough"

# The reviews_per_month column has NAs when there hasn't been any reviews, we'll replace these with 0
raw_data$reviews_per_month[is.na(raw_data$reviews_per_month)] = 0

# Confirm there are any NAs left in the data
anyNA(raw_data)


# Let's make a new variable, the dataset without the factors and check out correlations & distributions
ggplot(data = raw_data) +
  facet_grid(room_type ~ borough) +
  geom_histogram(mapping = aes(price), bins = 30)

# Let's make a new variable, the dataset without the factors and check out correlations & distributions

# Crazy expesive outliers we're going to ignore 
# I want to focus on Entire home/apt in Manhattan
data = subset(raw_data, price < 1250 & borough == "Manhattan" & room_type == "Entire home/apt")

ggplot(data = data) +
  geom_histogram(mapping = aes(price), bins = 50)

# Check for correlations
corrplot(cor(data[, c(-2, -4, -5, -6, -9, -13)]), method = "square")
# Nothing Meaningful

