# --- Define UI ---
ui <- fluidPage(
  titlePanel("Logistic Regression Model Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the model and dataset to view performance metrics."),
      
      selectInput("model_choice", "Choose Logistic Model:",
                  choices = list("Full Model (Fit_1)" = "fit1", 
                                 "Reduced Model (Fit_2)" = "fit2")),
      
      selectInput("data_choice", "Evaluation Dataset:",
                  choices = list("Validation Set" = "val", 
                                 "Test Set" = "test")),
      
      hr(),
      # Displaying AIC in the sidebar for quick reference
      uiOutput("sidebar_metrics")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Classification Analysis", 
                 fluidRow(
                   column(7, plotOutput("sigmoid_plot")),
                   column(5, plotOutput("accuracy_pie"))
                 )
        ),
        
        tabPanel("Model Coefficients", 
                 verbatimTextOutput("model_summary"))
      )
    )
  )
)

# --- Define Server ---
server <- function(input, output) {
  
  # Reactive function to select model
  selected_model <- reactive({
    if(input$model_choice == "fit1") return(Fit_1)
    return(Fit_2)
  })
  
  # Reactive function to select dataset
  selected_data <- reactive({
    df <- if(input$data_choice == "val") validation_set else test_set
    # Ensure price_group is 0/1 for math
    df$price_group <- ifelse(df$price >= 650000, 1, 0)
    return(df)
  })
  
  # Sidebar AIC Display
  output$sidebar_metrics <- renderUI({
    model <- selected_model()
    tagList(
      tags$h4("Model Quality"),
      tags$p(paste("AIC Value:", round(model$aic, 2)))
    )
  })
  
  # 1. Sigmoid Visualization
  output$sigmoid_plot <- renderPlot({
    model <- selected_model()
    data <- selected_data()
    
    z <- predict(model, data, type = "link")
    probs <- predict(model, data, type = "response")
    df_plot <- data.frame(z = z, prob = probs, Class = as.factor(data$price_group))
    
    ggplot(df_plot, aes(x = z, y = prob)) +
      geom_point(aes(color = Class), alpha = 0.3) +
      stat_function(fun = function(x) 1 / (1 + exp(-x)), color = "red", linewidth = 1) +
      labs(title = "Sigmoid Curve (Log-Odds vs Prob)",
           x = "Log-Odds (Linear Predictor)", y = "Predicted Probability") +
      theme_minimal() +
      scale_color_manual(values = c("0" = "skyblue", "1" = "orange")) +
      theme(legend.position = "bottom")
  })
  
  # 2. Accuracy Pie Chart
  output$accuracy_pie <- renderPlot({
    model <- selected_model()
    data <- selected_data()
    
    probs <- predict(model, data, type = "response")
    preds <- ifelse(probs > 0.5, 1, 0)
    
    # Correct vs Incorrect
    correct <- sum(preds == data$price_group)
    incorrect <- nrow(data) - correct
    total <- nrow(data)
    acc_pct <- round((correct / total) * 100, 1)
    
    pie_data <- data.frame(
      Category = c("Correct", "Incorrect"),
      Count = c(correct, incorrect)
    ) %>%
      mutate(
        Percent = Count / sum(Count),
        Label = paste0(
          round(Percent * 100, 1), "%\n",
          "(n = ", Count, ")"
        )
      )
    
    ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("Correct" = "#2ecc71", "Incorrect" = "#e74c3c")) +
      labs(title = paste0("Model Accuracy: ", acc_pct, "%")) +
      geom_text(
        aes(label = Label),
        position = position_stack(vjust = 0.5),
        color = "black",     # ✅ black text
        size = 3.5           # ✅ smaller font
      )
  })
  
  # 3. Model Summary
  output$model_summary <- renderPrint({
    summary(selected_model())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
