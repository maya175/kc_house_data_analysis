library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(shiny)
library(patchwork) 
library(caret)

# Load the data
kc_house<-read.csv('C:/Users/65834/Desktop/School/DANA/Proj/kc_house_data.csv')

# Data Cleaning and Preprocessing
## Check for missing values
any(is.na(kc_house)) 

## Convert date format
kc_house$date<-mdy(kc_house$date)

## Check unusual bedroom values
table(kc_house$bedrooms) 
## Filter bedrooms ≥ 9 and select only (price, bedrooms, bathrooms, sqft_living) columns
subset(kc_house[kc_house$bedrooms >= 9, ],
       select = c(price, bedrooms, bathrooms, sqft_living))

## Remove outliers in 'bedrooms'
kc_house <- kc_house[!(kc_house$bedrooms %in% c(11, 33)), ]

## Check for duplicate rows
sum(duplicated(kc_house))
duplicate_id <- kc_house[kc_house$id %in% kc_house$id[duplicated(kc_house$id)], ]
duplicate_id <- duplicate_id[, c("id", "date", "price", 
                                 "bedrooms", "bathrooms",
                                 "sqft_living", "sqft_lot",
                                 "floors")]

## Remove duplicate entries based on 'id', keeping the most recent entry
kc_house <- kc_house %>%
  group_by(id) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup()

## Verify and correct living area calculation (if necessary)
all(kc_house$sqft_above + kc_house$sqft_basement == kc_house$sqft_living)

## Remove redundant columns
kc_house <- kc_house %>% select(-sqft_above, -sqft_basement, -id)

# Data Splitting
## Set seed for reproducibility
set.seed(123)
n <- nrow(kc_house)

## Create indices for training, testing, and validation sets
train_index <- sample(1:n, 0.8 * n)
train_set <- kc_house[train_index, ]
temp <- kc_house[-train_index, ]
test_index <- sample(1:nrow(temp), 0.5 * nrow(temp))
test_set <- temp[test_index, ]
validation_set <- temp[-test_index, ]

# Visualization of Data Splits
## Create dataset split counts
split_sizes <- data.frame(
  Split = c("Training (80%)", "Testing (10%)", "Validation (10%)"),
  Count = c(nrow(train_set), nrow(test_set), nrow(validation_set))
)

# Price group in ALL datasets ------------------------------------
kc_house <- kc_house %>%
  mutate(price_group = ifelse(price >= 650000, "650K and Above", "Below 650K"))

train_set <- train_set %>%
  mutate(price_group = ifelse(price >= 650000, "650K and Above", "Below 650K"))

test_set <- test_set %>%
  mutate(price_group = ifelse(price >= 650000, "650K and Above", "Below 650K"))

validation_set <- validation_set %>%
  mutate(price_group = ifelse(price >= 650000, "650K and Above", "Below 650K"))

# Colors (same for all charts)
pie_colors <- c("Below 650K" = "skyblue", "650K and Above" = "orange")

# 1) ALL
price_counts_all <- table(kc_house$price_group)
pie(
  price_counts_all,
  main = "Proportion by price group (All)",
  col = pie_colors[names(price_counts_all)],
  labels = paste0(
    names(price_counts_all), "\n",
    round(100 * price_counts_all / sum(price_counts_all), 1), "%"
  )
)

# 2) TRAIN
price_counts_train <- table(train_set$price_group)
pie(
  price_counts_train,
  main = "Proportion by price group (Train)",
  col = pie_colors[names(price_counts_train)],
  labels = paste0(
    names(price_counts_train), "\n",
    round(100 * price_counts_train / sum(price_counts_train), 1), "%"
  )
)

# 3) TEST
price_counts_test <- table(test_set$price_group)
pie(
  price_counts_test,
  main = "Proportion by price group (Test)",
  col = pie_colors[names(price_counts_test)],
  labels = paste0(
    names(price_counts_test), "\n",
    round(100 * price_counts_test / sum(price_counts_test), 1), "%"
  )
)

# 4) VALIDATION
price_counts_val <- table(validation_set$price_group)
pie(
  price_counts_val,
  main = "Proportion by price group (Validation)",
  col = pie_colors[names(price_counts_val)],
  labels = paste0(
    names(price_counts_val), "\n",
    round(100 * price_counts_val / sum(price_counts_val), 1), "%"
  )
)

# Calculate the correlation matrix for numeric features
train_numeric <- train_set[, sapply(train_set, is.numeric)]
cor_kc_train<-cor(train_numeric)

# Linear Regression Models
Model_1 <- lm(price ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade + yr_built + yr_renovated + lat + long +sqft_living15 + sqft_lot15,data = train_set)
summary(Model_1)
Model_2 <- lm(price ~ bathrooms + waterfront +  view + condition + grade + yr_built + yr_renovated + lat + long +sqft_living15 ,data = train_set)
summary(Model_2)

# R2 value for validation
Pred_1<-Model_1%>%predict(validation_set)
R2(Pred_1,validation_set$price)
Pred_2<-Model_2%>%predict(validation_set)
R2(Pred_2,validation_set$price)

# Create a binary target variable based on the price
train_set <- train_set %>%mutate(price_group = ifelse(price >= 650000,1,0))
validation_set <- validation_set %>%mutate(price_group = ifelse(price >= 650000,1,0))
test_set <- test_set %>%mutate(price_group = ifelse(price >= 650000,1,0))

# Logistic Regression Models
Fit_1<-glm(price_group~bedrooms + bathrooms + floors + waterfront + view + condition + grade + yr_built + yr_renovated + lat + long +sqft_living15 + sqft_lot15,family = binomial(),data = train_set)
summary(Fit_1)
Fit_2<-glm(price_group~bedrooms + bathrooms + floors + waterfront + view + condition + grade + yr_built + yr_renovated + lat + long +sqft_living15 ,family = binomial(),data = train_set)
summary(Fit_2)

# Accurancy in validation 
predictions1 <- Fit_1 %>% predict(validation_set, type="response")
y_pred_num1 <- ifelse(predictions1 > 0.5, 1, 0)
y_pred1 <- factor(y_pred_num1, levels=c(0, 1))
mean(y_pred1 == validation_set$price_group)

predictions2 <- Fit_2 %>% predict(validation_set, type="response")
y_pred_num2 <- ifelse(predictions2 > 0.5, 1, 0)
y_pred2 <- factor(y_pred_num2, levels=c(0, 1))
Pred_test<-Model_2%>%predict(test_set)
mean(y_pred2 == validation_set$price_group)

#log
Model_2_log <- lm(log(price) ~ bathrooms + waterfront + view + condition + grade + yr_built + yr_renovated + lat + long + sqft_living15, data = train_set)
summary(Model_2_log)
Pred_2_log <- predict(Model_2_log, validation_set)
Pred_2_price <- exp(Pred_2_log)
R2(Pred_2_price, validation_set$price)

Model_2_log2 <- lm(log(price) ~ bathrooms + waterfront + view + condition + grade + yr_built + yr_renovated + lat + sqft_living15, data = train_set)
summary(Model_2_log2)
Pred_2_log2 <- predict(Model_2_log2, validation_set)
Pred_2_price2 <- exp(Pred_2_log2)
R2(Pred_2_price2, validation_set$price)

# Residual 
# Calculate residuals for Model_2_log
validation_set$residuals <- validation_set$price - Pred_2_price

# Geographic Residual Analysis
ggplot(validation_set, aes(x = long, y = lat, color = residuals)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", labels = scales::comma_format(accuracy = 1)) +
  labs(title = "Geographic Residual Map", 
       subtitle = "Red areas are consistently under-predicted by the model") +
  theme_minimal()

# --- Enhancement: Zipcode-based 'Premium' Factor ---
train_set$zipcode <- as.factor(train_set$zipcode)
validation_set$zipcode <- as.factor(validation_set$zipcode)
test_set$zipcode <- as.factor(test_set$zipcode)

# Enhanced Model (Model_3) including the location premium
Model_3 <- lm(log(price) ~ bathrooms + waterfront + view + condition + grade + yr_built + yr_renovated + lat + long + sqft_living15 + zipcode, data = train_set)
summary(Model_3)
Pred_3 <- predict(Model_3, validation_set)
Pred_3 <- exp(Pred_3)
R2(Pred_3, validation_set$price)

Model_4 <- lm(log(price) ~ bathrooms + waterfront + view + condition + grade + yr_built + yr_renovated + lat + sqft_living15 + zipcode, data = train_set)
summary(Model_4)
Pred_4 <- predict(Model_4, validation_set)
Pred_4 <- exp(Pred_4)
R2(Pred_4, validation_set$price)

# PCA
# Select numeric structural predictors from your Model_2_log
pca_features <- train_set %>% 
  select(bathrooms, grade, sqft_living15)

# Run PCA (Crucial: Scale = TRUE because units differ)
house_pca <- prcomp(pca_features, center = TRUE, scale. = TRUE)

# Check variance explained
summary(house_pca)

# Plot the Scree Plot
var_explained <- house_pca$sdev^2 / sum(house_pca$sdev^2)
plot(var_explained, type = "b", xlab = "Principal Component", 
     ylab = "Proportion of Variance", main = "Scree Plot")


# Add PC scores to your training and validation sets
train_set_pca <- cbind(train_set, house_pca$x)

# Apply the same transformation to validation data using training rotation
val_pca_scores <- predict(house_pca, newdata = validation_set)
validation_set_pca <- cbind(validation_set, val_pca_scores)

# ENHANCED MODEL: Replace structural variables with PC1 and PC2
Model_Enhanced_PCA <- lm(log(price) ~ PC1 + PC2 + waterfront + view + 
                           condition + yr_built + yr_renovated + lat + long, 
                         data = train_set_pca)
summary(Model_Enhanced_PCA)

# View the rotation matrix
house_pca$rotation
# Validation
pred_log_pca <- predict(Model_Enhanced_PCA, newdata = validation_set_pca)
pred_price_pca <- exp(pred_log_pca)
R2(pred_price_pca, validation_set$price)
