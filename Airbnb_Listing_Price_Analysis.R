install.packages("readr")
library(readr)

airbnb <- read_csv("C:/Users/Marc Jomerick Lo/Downloads/airbnb.csv")
View(airbnb)

#START
install.packages(c("tidyverse", "ggplot2", "caret", "corrplot", "lubridate"))
library(tidyverse)
library(caret)
library(corrplot)
library(lubridate)

#LOAD DATASET
airbnb <- read.csv("data/airbnb.csv")
str(airbnb)
summary(airbnb)
colSums(is.na(airbnb))

# DATA CLEANING
airbnb <- distinct(airbnb)

# CONVERT
airbnb$price     <- as.numeric(gsub("[^0-9.]", "", airbnb$price))
airbnb$rating    <- as.numeric(airbnb$rating)
airbnb$reviews   <- as.numeric(airbnb$reviews)
airbnb$bedrooms  <- as.numeric(airbnb$bedrooms)
airbnb$bathrooms <- as.numeric(airbnb$bathrooms)
airbnb$beds      <- as.numeric(airbnb$beds)
airbnb$guests    <- as.numeric(airbnb$guests)

airbnb <- airbnb %>% filter(!is.na(price))

# EDA
ggplot(airbnb, aes(x = price)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution of Airbnb Prices",
       x = "Price per Night",
       y = "Count") +
  theme_minimal()

ggplot(airbnb, aes(x = reorder(country, price, median), y = price)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Price Distribution by Country",
       x = "Country",
       y = "Price") +
  theme_minimal()

ggplot(airbnb, aes(x = rating, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Rating vs Price",
       x = "Rating",
       y = "Price") +
  theme_minimal()

# CORRELATION MATRIX
numeric_vars <- airbnb %>%
  select(price, rating, reviews, bedrooms, bathrooms, beds, guests)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# PREDICTION (MACHINE LEARNING)
set.seed(123)

model_data <- airbnb %>%
  select(price, rating, reviews, bedrooms, bathrooms, beds, guests) %>%
  na.omit()

train_index <- createDataPartition(model_data$price, p = 0.8, list = FALSE)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# TRAINING
model <- train(price ~ rating + reviews + bedrooms + bathrooms + beds + guests,
               data = train_data,
               method = "lm")

summary(model)
predictions <- predict(model, test_data)
postResample(predictions, test_data$price)

corrplot(cor_matrix, method = "color", type = "upper")

varImp(model)