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
kc_house <- kc_house %>% select(-sqft_above, -sqft_basement)

## Convert 'waterfront' to categorical variable
kc_house$waterfront[kc_house$waterfront==0]<-"No"
kc_house$waterfront[kc_house$waterfront==1]<-"Yes"

## Convert 'condition' to ordered factor
kc_house$condition <- factor(kc_house$condition,
                             levels = c(1,2,3,4,5),
                             labels = c("Poor", "Fair", "Average", "Good", "Very Good"))


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

# ui
ui <- fluidPage(
  titlePanel("King County House Prices – Combined Interactive Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "price_range",
        "Filter by sale price ($):",
        min = min(kc_house$price, na.rm = TRUE),
        max = max(kc_house$price, na.rm = TRUE),
        value = c(min(kc_house$price, na.rm = TRUE),
                  max(kc_house$price, na.rm = TRUE)),
        step = 50000,
        pre = "$",
        sep = ","
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Part 1: Price Overview",
          fluidRow(
            column(6, plotOutput("pieChart")),
            column(6, plotOutput("histChart"))
          )
        ),
        tabPanel(
          "Part 2: Structure & Price",
          fluidRow(
            column(4, plotOutput("bedroomChart")),
            column(4, plotOutput("bathroomChart")),
            column(4, plotOutput("conditionChart"))
          )
        ),
        tabPanel(
          "Part 3: Size vs Price",
          fluidRow(
            column(6, plotOutput("scatter_living")),
            column(6, plotOutput("scatter_lot"))
          )
        ),
        tabPanel(
          "Part 4: Grade & Quality",
          fluidRow(
            column(6, plotOutput("price_grade_boxplot")),
            column(6, plotOutput("sqft_grade_bar"))
          )
        ),
        tabPanel(
          "Part 5: Location",
          fluidRow(
            column(6, plotOutput("heatMap")),
            column(6, plotOutput("zipcodeBar"))
          )
        ),
        tabPanel(
          "Part 6: Renovation & Size",
          fluidRow(
            column(6, plotOutput("renovated_living")),
            column(6, plotOutput("nonrenovated_living"))
          )
        ),
        tabPanel("Age & Renovation",    plotOutput("age_panel",   height = 400)),
        tabPanel("Price per Sqft",      plotOutput("sqft_panel",  height = 400)),
        tabPanel("Grade vs Price/Sqft", plotOutput("grade_panel", height = 400)),
        tabPanel("Renovation Effects",  plotOutput("renov_panel", height = 400)),
        tabPanel("Seasonality",         plotOutput("month_plot",  height = 400)),
        tabPanel("Zip 980 vs 981",      plotOutput("zip_panel",   height = 400))
      )
    )
  )
)
server <- function(input, output, session) {
  
  pie_colors <- c("Below 650K" = "skyblue", "650K and Above" = "orange")
  standard_color <- "steelblue"
  
  # One reactive dataset filtered by price
  filtered_data <- reactive({
    kc_house %>%
      filter(price >= input$price_range[1],
             price <= input$price_range[2])
  })
  
  # ---------- Part 1 ----------
  output$pieChart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) {
      plot.new(); title("No houses in this price range"); return()
    }
    price_counts <- table(d$price_group)
    pie(
      price_counts,
      main = "Proportion of Houses\n650K and Above vs Below 650K",
      col  = pie_colors[names(price_counts)],
      labels = paste0(
        names(price_counts), "\n",
        round(100 * price_counts / sum(price_counts), 1), "%"
      )
    )
  })
  
  output$histChart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) {
      plot.new(); title("No houses in this price range"); return()
    }
    ggplot(d, aes(x = price)) +
      geom_histogram(binwidth = 50000,
                     fill = "grey70", color = "black") +
      geom_vline(xintercept = 650000,
                 linetype = "dashed",
                 color = "red", linewidth = 1.2) +
      annotate("text",
               x = 5000000,
               y = Inf,
               vjust = 2,
               color = "red",
               fontface = "bold",
               label = "Red dash line = 650K") +
      scale_x_continuous(labels = comma) +
      labs(
        title = "Distribution of House Prices",
        x = "Sale Price ($)",
        y = "Number of Houses"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid   = element_blank(),
        axis.line    = element_line(color = "black"),
        plot.title   = element_text(face = "bold", size = 16)
      )
  })
  
  # ---------- Part 2 ----------
  output$bedroomChart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    d %>%
      group_by(bedrooms) %>%
      summarise(avg_price = mean(price), .groups = "drop") %>%
      ggplot(aes(x = factor(bedrooms), y = avg_price)) +
      geom_bar(stat = "identity", fill = standard_color) +
      labs(
        title = "Average Price by Bedrooms",
        x = "Number of Bedrooms",
        y = "Average Price ($)"
      ) +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
  })
  
  output$bathroomChart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    d %>%
      mutate(bath_bin = round(bathrooms)) %>%
      group_by(bath_bin) %>%
      summarise(avg_price = mean(price), .groups = "drop") %>%
      ggplot(aes(x = factor(bath_bin), y = avg_price)) +
      geom_bar(stat = "identity", fill = standard_color) +
      labs(
        title = "Average Price by Bathrooms",
        x = "Number of Bathrooms",
        y = "Average Price ($)"
      ) +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
  })
  
  output$conditionChart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    d %>%
      group_by(condition) %>%
      summarise(avg_price = mean(price), .groups = "drop") %>%
      ggplot(aes(x = factor(condition), y = avg_price)) +
      geom_bar(stat = "identity", fill = standard_color) +
      labs(
        title = "Average Price by Condition",
        x = "Condition Rating",
        y = "Average Price ($)"
      ) +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
  })
  
  # ---------- Part 3 ----------
  output$scatter_living <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    ggplot(d, aes(x = sqft_living, y = price)) +
      geom_point(color = "#1f78b4", alpha = 0.5, size = 2) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      labs(
        title = "Price vs Living Area (sqft_living)",
        x = "Living Area (sqft)",
        y = "Sale Price ($)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        axis.line   = element_line(color = "black")
      )
  })
  
  output$scatter_lot <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    ggplot(d, aes(x = sqft_lot, y = price)) +
      geom_point(color = "#1f78b4", alpha = 0.4, size = 2) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      labs(
        title = "Price vs Lot Area (sqft_lot)",
        x = "Lot Area (sqft)",
        y = "Sale Price ($)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        axis.line   = element_line(color = "black")
      )
  })
  
  # ---------- Part 4 ----------
  output$price_grade_boxplot <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    ggplot(d, aes(x = factor(grade), y = price)) +
      geom_boxplot(fill = "grey80", color = "black", outlier.alpha = 0.3) +
      geom_hline(yintercept = 650000,
                 linetype = "dashed",
                 color = "red",
                 linewidth = 1) +
      annotate("text",
               x = Inf,
               y = 650000,
               hjust = 1.1,
               vjust = -0.3,
               color = "red",
               fontface = "bold",
               label = "650K") +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Price Distribution by House Grade",
        x = "Grade",
        y = "Sale Price ($)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.line   = element_line(color = "black"),
        plot.title  = element_text(face = "bold", size = 16)
      )
  })
  
  output$sqft_grade_bar <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    avg_sqft <- d %>%
      group_by(grade, price_group) %>%
      summarise(avg_sqft_living = mean(sqft_living), .groups = "drop")
    
    ggplot(avg_sqft, aes(x = factor(grade),
                         y = avg_sqft_living,
                         fill = price_group)) +
      geom_col(position = position_dodge(width = 0.7)) +
      scale_fill_manual(
        name   = "Price Group",
        values = c("Below 650K" = "skyblue",
                   "650K and Above" = "darkorange")
      ) +
      labs(
        title = "Average Living Area by Grade and Price Group",
        x = "Grade",
        y = "Average Living Area (sqft)"
      ) +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid  = element_blank(),
        axis.line   = element_line(color = "black"),
        plot.title  = element_text(face = "bold", size = 16),
        legend.position = "bottom"
      )
  })
  
  # ---------- Part 5 ----------
  output$heatMap <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    ggplot(d, aes(x = long, y = lat, color = price_group)) +
      geom_point(alpha = 0.4, size = 1.5) +
      scale_color_manual(values = c(
        "Below 650K"     = "skyblue",
        "650K and Above" = "darkorange"
      )) +
      labs(
        title = "Heatmap of House Locations by Price Group",
        x = "Longitude",
        y = "Latitude",
        color = "Price Group"
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank())
  })
  
  output$zipcodeBar <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    filtered_zip <- d %>%
      filter(price >= 650000) %>%
      group_by(zipcode) %>%
      summarise(count = n()) %>%
      filter(count >= 50) %>%
      arrange(desc(count))
    
    if (nrow(filtered_zip) == 0) {
      plot.new(); title("No zipcodes with ≥50 houses in this range"); return()
    }
    
    ggplot(filtered_zip, aes(x = reorder(as.factor(zipcode), -count),
                             y = count)) +
      geom_col(fill = "darkorange") +
      labs(
        title = "Zipcodes with 50+ Houses Priced at 650K and Above",
        x = "Zipcode",
        y = "Number of Houses ≥650K"
      ) +
      theme_minimal() +
      theme(
        panel.grid  = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ---------- Part 6 ----------
  output$renovated_living <- renderPlot({
    d <- filtered_data()
    d_renov <- d %>% filter(renovated == 1)
    if (nrow(d_renov) == 0) {
      plot.new(); title("No renovated houses in this price range"); return()
    }
    data_renov <- d_renov %>%
      group_by(grade, price_group) %>%
      summarise(mean_living = mean(sqft_living), .groups = "drop")
    
    ggplot(data_renov,
           aes(x = factor(grade),
               y = mean_living,
               fill = price_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(
        "Below 650K"     = "skyblue",
        "650K and Above" = "darkorange"
      )) +
      labs(
        title = "Renovated Houses:\nAverage Sqft Living by Grade & Price Group",
        x = "Grade",
        y = "Average Sqft Living (sqft)",
        fill = "Price Group"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid  = element_blank(),
        axis.line   = element_line(color = "black"),
        plot.title  = element_text(face = "bold", hjust = 0.5)
      )
  })
  
  output$nonrenovated_living <- renderPlot({
    d <- filtered_data()
    d_non <- d %>% filter(renovated == 0)
    if (nrow(d_non) == 0) {
      plot.new(); title("No non-renovated houses in this price range"); return()
    }
    data_nonrenov <- d_non %>%
      group_by(grade, price_group) %>%
      summarise(mean_living = mean(sqft_living), .groups = "drop")
    
    ggplot(data_nonrenov,
           aes(x = factor(grade),
               y = mean_living,
               fill = price_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(
        "Below 650K"     = "skyblue",
        "650K and Above" = "darkorange"
      )) +
      labs(
        title = "Non-Renovated Houses:\nAverage Sqft Living by Grade & Price Group",
        x = "Grade",
        y = "Average Sqft Living (sqft)",
        fill = "Price Group"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid  = element_blank(),
        axis.line   = element_line(color = "black"),
        plot.title  = element_text(face = "bold", hjust = 0.5)
      )
  })
  
  # ---------- Advanced: Age & Renovation ----------
  output$age_panel <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(
        year_sold  = year(date),
        house_age  = year_sold - yr_built,
        years_since_renovation = ifelse(
          yr_renovated == 0,
          NA,
          year_sold - yr_renovated
        )
      )
    
    d_renov <- d %>% filter(!is.na(years_since_renovation))
    
    p_age_dist <- ggplot(d, aes(x = house_age)) +
      geom_histogram(binwidth = 5, color = "black", fill = "skyblue") +
      labs(
        title = "Distribution of House Age",
        x = "House age (years)",
        y = "Number of houses"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
    
    if (nrow(d_renov) == 0) {
      p_renov_box <- ggplot() + theme_void() +
        ggtitle("No renovated houses in this price range")
    } else {
      kc_box <- d_renov %>%
        mutate(
          renov_bin = cut(
            years_since_renovation,
            breaks = c(0, 5, 10, 20, 40, Inf),
            labels = c("0–5", "5–10", "10–20", "20–40", "40+"),
            right  = FALSE
          )
        ) %>%
        filter(!is.na(renov_bin))
      
      p_renov_box <- ggplot(kc_box, aes(x = renov_bin, y = price)) +
        geom_boxplot(fill = "skyblue", color = "gray30", outlier.alpha = 0.3) +
        scale_y_continuous(labels = label_dollar()) +
        labs(
          title = "Sale Price by Years Since Renovation (Binned)",
          x = "Years Since Renovation (bins)",
          y = "Sale Price (USD)"
        ) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    age_price_bar <- d %>%
      mutate(
        age_bin = cut(
          house_age,
          breaks = c(0, 10, 20, 30, 40, 50, Inf),
          labels = c("0–10", "10–20", "20–30", "30–40", "40–50", "50+"),
          right  = FALSE
        )
      ) %>%
      filter(!is.na(age_bin)) %>%
      group_by(age_bin) %>%
      summarise(
        avg_price = mean(price, na.rm = TRUE),
        .groups   = "drop"
      )
    
    if (nrow(age_price_bar) == 0) {
      p_age_bar <- ggplot() + theme_void() +
        ggtitle("No data for age bins in this range")
    } else {
      p_age_bar <- ggplot(age_price_bar, aes(x = age_bin, y = avg_price)) +
        geom_col(fill = "skyblue", color = "black") +
        scale_y_continuous(labels = label_dollar()) +
        labs(
          title = "Average Sale Price by House Age (Binned)",
          x = "House age (years)",
          y = "Average sale price (USD)"
        ) +
        theme_classic() +
        theme(panel.grid = element_blank())
    }
    
    p_age_dist + p_renov_box + p_age_bar
  })
  
  # ---------- Advanced: Price per Sqft ----------
  output$sqft_panel <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(
        price_per_sqft_living = price / sqft_living,
        price_per_sqft_lot    = price / sqft_lot
      )
    
    p_living_hist <- ggplot(d, aes(x = price_per_sqft_living)) +
      geom_histogram(binwidth = 25, color = "black", fill = "skyblue") +
      scale_x_continuous(labels = dollar_format()) +
      labs(
        title = "Histogram of Price per Sqft (Living)",
        x = "Price per sqft (living area)",
        y = "Number of houses"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
    
    p_lot_hist <- ggplot(d, aes(x = price_per_sqft_lot)) +
      geom_histogram(binwidth = 10, color = "black", fill = "skyblue") +
      scale_x_continuous(labels = label_dollar()) +
      labs(
        title = "Distribution of Price per Sqft (Lot)",
        x = "Price per sqft (lot)",
        y = "Number of houses"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
    
    p_living_hist + p_lot_hist
  })
  
  # ---------- Advanced: Grade vs Price/Sqft ----------
  output$grade_panel <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(
        price_per_sqft_living = price / sqft_living,
        price_per_sqft_lot    = price / sqft_lot
      )
    
    p_living_grade <- ggplot(d,
                             aes(x = factor(grade),
                                 y = price_per_sqft_living)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Price per Sqft (Living) by Grade",
        x = "Grade",
        y = "Price per sqft (living area)"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
    
    p_lot_grade <- ggplot(d,
                          aes(x = factor(grade),
                              y = price_per_sqft_lot)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      scale_y_continuous(labels = dollar_format()) +
      coord_cartesian(
        ylim = c(0, quantile(d$price_per_sqft_lot, 0.95, na.rm = TRUE))
      ) +
      labs(
        title = "Price per Sqft (Lot) by Grade (95th percentile cap)",
        x = "Grade",
        y = "Price per sqft (lot)"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
    
    p_living_grade + p_lot_grade
  })
  
  # ---------- Advanced: Renovation Effects ----------
  output$renov_panel <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(
        price_per_sqft_living = price / sqft_living,
        renovated_flag = ifelse(yr_renovated == 0, 0, 1),
        renov_status   = ifelse(yr_renovated == 0, "Not Renovated", "Renovated"),
        high_price     = ifelse(price >= 650000, 1, 0)
      )
    
    kc_avg_renovation <- d %>%
      group_by(renov_status) %>%
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
    
    p_avg <- ggplot(kc_avg_renovation,
                    aes(x = renov_status, y = avg_price, fill = renov_status)) +
      geom_col(width = 0.6) +
      labs(
        title = "Average House Price based on Renovation",
        x = "Renovation Status",
        y = "Average Price (USD)"
      ) +
      scale_y_continuous(labels = label_dollar()) +
      scale_fill_manual(
        values = c("Not Renovated" = "darkorange", "Renovated" = "skyblue")
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p_box <- ggplot(d,
                    aes(x = factor(renovated_flag),
                        y = price_per_sqft_living,
                        fill = factor(renovated_flag))) +
      geom_boxplot() +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_manual(
        values = c("0" = "darkorange", "1" = "skyblue")
      ) +
      labs(
        title = "Price per Sqft (Living): Renovated vs Not",
        x = "Renovation (0 = Not, 1 = Yes)",
        y = "Price per sqft (living)"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank(),
            legend.position = "none")
    
    pie_data_renov <- d %>%
      group_by(renovated_flag) %>%
      summarise(
        n = n(),
        prop_high = mean(high_price),
        .groups   = "drop"
      ) %>%
      mutate(
        label = paste0(round(prop_high * 100, 1), "%"),
        renov_status2 = ifelse(renovated_flag == 1, "Renovated", "Not renovated")
      )
    
    p_pie <- ggplot(pie_data_renov, aes(x = "", y = prop_high, fill = renov_status2)) +
      geom_col(color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(
        values = c("Not renovated" = "darkorange",
                   "Renovated"    = "skyblue")
      ) +
      labs(
        title = "Share of High-Price Houses (≥ $650K)\nRenovated vs Not Renovated",
        x = NULL, y = NULL, fill = "Renovation status"
      ) +
      theme_void()
    
    p_avg + p_box + p_pie
  })
  
  # ---------- Advanced: Seasonality ----------
  output$month_plot <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(month_sold = month(date, label = TRUE, abbr = TRUE))
    
    price_month <- d %>%
      group_by(month_sold) %>%
      summarise(avg_price = mean(price, na.rm = TRUE),
                .groups   = "drop")
    
    ggplot(price_month,
           aes(x = month_sold, y = avg_price, group = 1)) +
      geom_line(color = "skyblue", linewidth = 1) +
      geom_point(color = "skyblue", size = 2) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Average House Price by Month",
        x = "Month of Sale",
        y = "Average Price"
      ) +
      theme_classic() +
      theme(panel.grid = element_blank())
  })
  
  # ---------- Advanced: Zip 980 vs 981 ----------
  output$zip_panel <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) { plot.new(); title("No data"); return() }
    
    d <- d %>%
      mutate(
        high_price = ifelse(price >= 650000, 1, 0),
        zip_prefix = substr(as.character(zipcode), 1, 3)
      )
    
    pie_data_zip <- d %>%
      filter(zip_prefix %in% c("980", "981")) %>%
      group_by(zip_prefix) %>%
      summarise(
        prop_high = mean(high_price, na.rm = TRUE),
        .groups   = "drop"
      )
    
    map_data_zip <- d %>%
      filter(high_price == 1, zip_prefix %in% c("980", "981"))
    
    if (nrow(pie_data_zip) == 0 | nrow(map_data_zip) == 0) {
      plot.new(); title("No 980/981 houses in this price range"); return()
    }
    
    zip_cols <- c("980" = "darkorange", "981" = "skyblue")
    
    p_zip_pie <- ggplot(pie_data_zip, aes(x = "", y = prop_high, fill = zip_prefix)) +
      geom_col(color = "white") +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = paste0(zip_prefix, ": ", percent(prop_high, 0.1))),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5
      ) +
      scale_fill_manual(values = zip_cols, name = "Zip prefix") +
      labs(
        x = NULL, y = NULL,
        title = "Proportion of ≥$650K Homes\nby Zip Prefix (980 vs 981)"
      ) +
      theme_void()
    
    p_zip_map <- ggplot(map_data_zip, aes(x = long, y = lat)) +
      geom_point(
        aes(color = zip_prefix, size = price),
        alpha = 0.6
      ) +
      scale_color_manual(values = zip_cols, name = "Zip prefix") +
      scale_size_continuous(
        range  = c(1.5, 6),
        labels = label_dollar()
      ) +
      labs(
        title = "≥$650K Homes (980 vs 981)\nLat–Long and Price",
        x = "Longitude",
        y = "Latitude",
        size = "Price"
      ) +
      theme_minimal()
    
    p_zip_pie + p_zip_map
  })
}
shinyApp(ui, server) 