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

listings = read.csv("C:/Users/joshl/Downloads/listings.csv", header = TRUE)

# Get an initial feel for the dataset

glimpse(listings)
head(listings)
str(listings)

### Based on what we saw, I'm going to make the following changes ###

# The neighbourhood_group column is NA for all entries, so I'll just remove it

listings$neighbourhood_group = NULL

# The reviews_per_month column has NAs when there hasn't been any reviews, we'll replace these with 0

listings$reviews_per_month[is.na(listings$reviews_per_month)] = 0



### Let's make a new variable, the dataset without the factors and check out some correlations
### And istributions

# Create a new variable listings_num_only 
listings_num_only = subset(listings, select = -c(2, 4, 5, 8, 12))
cor_listings_num_only = cor(listings_num_only)
corrplot(cor_listings_num_only, method = "square", type = "lower")
chart.Correlation(listings_num_only)