ui <- fluidPage(
  tags$style(HTML("
    body{margin:0; padding:0;}
    .panel-box{
      background:#fff; padding:8px; border:1px solid #e5e5e5;
      border-radius:8px; margin-bottom:8px;
    }
    .title{font-size:16px; font-weight:600; margin-bottom:4px;}
    .subtle{color:#666; font-size:12px;}
    .control-label{font-size:13px;}
  ")),
  titlePanel("KC House — Final Dashboard"),
  
  fluidRow(
    # LEFT sidebar
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
          
          hr(style="margin:8px 0;"),
          div(class="subtle",
              "Model performance (computed on FULL selected dataset):"),
          verbatimTextOutput("regMetrics", placeholder = TRUE)
      )
    ),
    
    # RIGHT main (2x2 grid) — smaller heights to fit one screen
    column(
      9,
      fluidRow(
        column(6, div(class="panel-box",
                      div(class="title","Factors Affecting Probability (Odds Ratio) — Fit_2"),
                      plotOutput("oddsPlot", height = "240px"))),
        column(6, div(class="panel-box",
                      div(class="title","House Locations by Price Group"),
                      plotOutput("mapPlot", height = "240px")))
      ),
      fluidRow(
        column(6, div(class="panel-box",
                      div(class="title","Predicted vs Actual (Selected Model)"),
                      plotOutput("predVsActual", height = "240px"))),
        column(6, div(class="panel-box",
                      div(class="title","Residuals (Actual − Predicted)"),
                      plotOutput("residualPlot", height = "240px")))
      )
    )
  )
)

server <- function(input, output, session){
  
  # =========================================================
  # AUTO-CREATE Model_2_log2 (NO dataset changes)
  # =========================================================
  observe({
    if (!exists("Model_2_log2", envir = .GlobalEnv) &&
        exists("train_set", envir = .GlobalEnv)) {
      
      ts <- get("train_set", envir = .GlobalEnv)
      
      Model_2_log2 <- lm(
        log(price) ~ bathrooms + waterfront + view + condition + grade +
          yr_built + yr_renovated + lat + sqft_living15,
        data = ts
      )
      
      assign("Model_2_log2", Model_2_log2, envir = .GlobalEnv)
    }
  })
  
  # ---- available models (auto-detect) ----
  all_models <- c(
    "Model_1",
    "Model_2",
    "Model_2_log",
    "Model_2_log2",
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
    req(nrow(df) > 0, input$regModelPick)
    
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
    req(input$priceRange)
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
    
    p <- predict(model, newdata = df)
    if (model_name %in% log_models) exp(p) else as.numeric(p)
  }
  
  # ---- Metrics (FULL dataset; R² only) ----
  output$regMetrics <- renderPrint({
    df <- df_for_model()
    pred <- predict_price(input$regModelPick, df)
    
    cat("Selected model:", input$regModelPick, "\n")
    cat("Dataset:", input$dataset, "\n")
    cat("Rows used:", nrow(df), "\n")
    cat("R²:", round(R2(pred, df$price), 4), "\n")
  })
  
  # ---- TOP-LEFT: Odds Ratio Plot (Fit_2) ----
  output$oddsPlot <- renderPlot({
    req(exists("Fit_2", envir = .GlobalEnv))
    Fit_2 <- get("Fit_2", envir = .GlobalEnv)
    
    odds_df <- data.frame(
      Variable = names(coef(Fit_2)),
      OddsRatio = exp(coef(Fit_2))
    ) %>%
      filter(Variable != "(Intercept)") %>%
      arrange(OddsRatio)
    
    ggplot(odds_df, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
      geom_col(fill = "#F2C94C") +
      coord_flip() +
      labs(x = "Housing Factors", y = "Odds Ratio") +
      theme_minimal(base_size = 11)
  })
  
  # ---- TOP-RIGHT: Location plot (blue/orange) ----
  output$mapPlot <- renderPlot({
    df <- df_filtered()
    
    if (nrow(df) == 0) {
      ggplot() + theme_void() + labs(title = "No data in selected price range")
    } else {
      df <- df %>%
        filter(!is.na(lat), !is.na(long), !is.na(price)) %>%
        mutate(price_group = ifelse(price >= 650000, "650K and Above", "Below 650K"))
      
      ggplot(df, aes(x = long, y = lat)) +
        geom_point(
          data = subset(df, price_group == "Below 650K"),
          color = "#9AD0EC", size = 1.7, alpha = 0.8
        ) +
        geom_point(
          data = subset(df, price_group == "650K and Above"),
          color = "#F4A261", size = 2.2, alpha = 0.9
        ) +
        labs(x = "Longitude", y = "Latitude") +
        theme_minimal(base_size = 11)
    }
  })
  
  # ---- BOTTOM-LEFT: Predicted vs Actual (red line only) ----
  output$predVsActual <- renderPlot({
    df <- df_filtered()
    
    if (nrow(df) == 0) {
      ggplot() + theme_void() + labs(title = "No data in selected price range")
    } else {
      pred <- predict_price(input$regModelPick, df)
      
      ggplot(
        data.frame(Actual = df$price, Predicted = pred),
        aes(Actual, Predicted)
      ) +
        geom_point(alpha = 0.55, size = 1.5) +
        geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.1) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal(base_size = 11) +
        labs(x = "Actual Price", y = "Predicted Price")
    }
  })
  
  # ---- BOTTOM-RIGHT: Residual plot ----
  output$residualPlot <- renderPlot({
    df <- df_filtered()
    
    if (nrow(df) == 0) {
      ggplot() + theme_void() + labs(title = "No data in selected price range")
    } else {
      pred <- predict_price(input$regModelPick, df)
      
      ggplot(
        data.frame(Predicted = pred, Residual = df$price - pred),
        aes(Predicted, Residual)
      ) +
        geom_point(alpha = 0.55, size = 1.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal(base_size = 11) +
        labs(x = "Predicted Price", y = "Residual")
    }
  })
}

shinyApp(ui, server)
