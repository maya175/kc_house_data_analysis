ui <- fluidPage(
  tags$style(HTML("
    .panel-box{background:#fff; padding:10px; border:1px solid #e5e5e5;
               border-radius:8px; margin-bottom:12px;}
    .title{font-size:18px; font-weight:600; margin-bottom:6px;}
    .subtle{color:#666;}
  ")),
  titlePanel("KC House — Regression Dashboard"),
  
  fluidRow(
    column(
      3,
      div(class="panel-box",
          div(class="title","Controls"),
          
          selectInput("dataset", "Dataset:",
                      choices = c("Training","Validation","Test"),
                      selected = "Validation"),
          
          selectInput("regModelPick", "Regression model:",
                      choices = NULL),
          
          uiOutput("priceSlider"),
          
          hr(),
          div(class="subtle",
              "Model performance (R²) — computed on FULL selected dataset:"),
          verbatimTextOutput("regMetrics", placeholder = TRUE)
      )
    ),
    
    column(
      9,
      div(class="panel-box",
          div(class="title","Predicted vs Actual"),
          plotOutput("predVsActual", height="420px")
      ),
      div(class="panel-box",
          div(class="title","Residuals (Actual − Predicted)"),
          plotOutput("residualPlot", height="320px")
      )
    )
  )
)

server <- function(input, output, session){
  
  # =========================================================
  # MAKE Model_2_log2 EXIST (ONLY creates model object, does NOT change datasets)
  # =========================================================
  observe({
    if (!exists("Model_2_log2", envir = .GlobalEnv) &&
        exists("train_set", envir = .GlobalEnv)) {
      
      ts <- get("train_set", envir = .GlobalEnv)
      
      # Create Model_2_log2 (reduced log model)
      # Uses common variables from your earlier Model_2_log2 definition
      Model_2_log2 <- lm(
        log(price) ~ bathrooms + waterfront + view + condition + grade +
          yr_built + yr_renovated + lat + sqft_living15,
        data = ts
      )
      
      assign("Model_2_log2", Model_2_log2, envir = .GlobalEnv)
    }
  })
  
  # ---- available models (auto-detect from environment) ----
  all_models <- c(
    "Model_1",
    "Model_2",
    "Model_2_log",
    "Model_2_log2",      # ✅ now will appear
    "Model_3",
    "Model_4",
    "Model_Enhanced_PCA"
  )
  
  available_models <- Filter(function(m) exists(m, envir = .GlobalEnv), all_models)
  
  observe({
    updateSelectInput(
      session,
      "regModelPick",
      choices = available_models,
      selected = if ("Model_2_log" %in% available_models) "Model_2_log" else available_models[1]
    )
  })
  
  # ---- dataset selection ----
  df_base <- reactive({
    switch(input$dataset,
           "Training"   = train_set,
           "Validation" = validation_set,
           "Test"       = test_set)
  })
  
  # ---- add PCA scores only when needed ----
  df_for_model <- reactive({
    df <- df_base()
    req(nrow(df) > 0)
    
    if (input$regModelPick == "Model_Enhanced_PCA") {
      req(exists("house_pca", envir = .GlobalEnv))
      pcs <- predict(house_pca, newdata = df)
      df <- cbind(df, pcs)
    }
    df
  })
  
  # ---- dynamic price slider (plots only) ----
  output$priceSlider <- renderUI({
    df <- df_for_model()
    max_price <- ceiling(max(df$price, na.rm = TRUE))
    
    sliderInput(
      "priceRange",
      "Price range filter ($) [plots only]:",
      min = 0,
      max = max_price,
      value = c(0, max_price),
      step = max(25000, round(max_price / 200))
    )
  })
  
  # ---- filtered data for plotting only ----
  df_filtered <- reactive({
    df <- df_for_model()
    df %>% filter(price >= input$priceRange[1],
                  price <= input$priceRange[2])
  })
  
  # ---- prediction helper (returns PRICE) ----
  predict_price <- function(model_name, df){
    
    model <- get(model_name, envir = .GlobalEnv)
    
    log_models <- c(
      "Model_2_log",
      "Model_2_log2",
      "Model_3",
      "Model_4",
      "Model_Enhanced_PCA"
    )
    
    if (model_name %in% log_models) {
      exp(predict(model, newdata = df))
    } else {
      predict(model, newdata = df)
    }
  }
  
  # ---- R² (FULL dataset) ----
  output$regMetrics <- renderPrint({
    df <- df_for_model()
    pred <- predict_price(input$regModelPick, df)
    
    cat("Selected model:", input$regModelPick, "\n")
    cat("Dataset:", input$dataset, "\n")
    cat("Rows used:", nrow(df), "\n")
    cat("R²:", round(R2(pred, df$price), 4), "\n")
  })
  
  # ---- Predicted vs Actual (RED LINE ONLY) ----
  output$predVsActual <- renderPlot({
    df <- df_filtered()
    
    if (nrow(df) == 0) {
      ggplot() + theme_void()
    } else {
      pred <- predict_price(input$regModelPick, df)
      
      ggplot(
        data.frame(Actual = df$price, Predicted = pred),
        aes(Actual, Predicted)
      ) +
        geom_point(alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0,
                    color = "red", linewidth = 1.2) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = paste("Predicted vs Actual —", input$regModelPick),
          x = "Actual Price",
          y = "Predicted Price"
        )
    }
  })
  
  # ---- Residual plot ----
  output$residualPlot <- renderPlot({
    df <- df_filtered()
    
    if (nrow(df) == 0) {
      ggplot() + theme_void()
    } else {
      pred <- predict_price(input$regModelPick, df)
      
      ggplot(
        data.frame(Predicted = pred, Residual = df$price - pred),
        aes(Predicted, Residual)
      ) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0,
                   linetype = "dashed",
                   color = "red") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Residual Plot",
          x = "Predicted Price",
          y = "Residual (Actual − Predicted)"
        )
    }
  })
}

shinyApp(ui, server)
