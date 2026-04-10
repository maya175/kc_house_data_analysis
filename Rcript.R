library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(shiny)
library(patchwork) 

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
# data prep for advanced
kc_house <- kc_house %>%
  mutate(
    year_sold  = year(date),
    house_age  = year_sold - yr_built,
    years_since_renovation = ifelse(
      yr_renovated == 0,
      NA,
      year_sold - yr_renovated
    ),
    renovated  = ifelse(yr_renovated == 0, 0, 1),
    price_per_sqft_living = price / sqft_living,
    price_per_sqft_lot    = price / sqft_lot,
    month_sold = month(date, label = TRUE, abbr = TRUE),
    high_price = ifelse(price >= 650000, 1, 0),
    zip_prefix = substr(as.character(zipcode), 1, 3)
  )
# If kc_years_renov was just renovated houses, define it here
kc_years_renov <- kc_house %>% filter(!is.na(years_since_renovation))
