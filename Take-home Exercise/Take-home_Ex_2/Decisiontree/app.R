#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application that draws a histogram
pacman::p_load(shiny, shinydashboard, visNetwork, plotly, tidyverse, rpart, caret, ranger, shinycustomloader)

ui <- dashboardPage(
  dashboardHeader(title = "Regression Model"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Churn Risk Analysis", tabName = "churn_tab", icon = icon("shield-halved"))
    ),
    hr(),
    conditionalPanel(
      condition = "input.model_tabs == 'Decision Tree'",
      h4("  🌲 Tree Parameters", style = "margin-left: 15px;"),
      sliderInput("cp", "Complexity (CP):", min = 0.0001, max = 0.05, value = 0.001, step = 0.0005),
      sliderInput("max_depth", "Max Depth:", min = 1, max = 15, value = 7),
      hr(),
      h4("  👥 Filter Population", style = "margin-left: 15px;"),
      selectInput("filter_income", "Income Bracket:", choices = c("All", "Low", "Medium", "High"))
    ),
    conditionalPanel(
      condition = "input.model_tabs == 'Random Forest' || input.model_tabs == 'Insights'",
      h4("  ⚙️ RF Parameters", style = "margin-left: 15px;"),
      sliderInput("num_trees", "Number of Trees:", min = 10, max = 200, value = 50, step = 10),
      actionButton("run_rf", "Run Random Forest", icon = icon("play"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-left: 15px;")
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabBox(
        id = "model_tabs", width = 12,
        title = tagList(shiny::icon("brain"), "Model Analysis Engine"),
        tabPanel("Decision Tree", 
                 fluidRow(
                   valueBoxOutput("vbox_obs", width = 4),
                   valueBoxOutput("vbox_churn", width = 4),
                   valueBoxOutput("vbox_top_occ", width = 4)
                 ),
                 hr(),
                 fluidRow(
                   box(title = "Interactive Decision Path", width = 8, status = "primary",
                       withLoader(visNetworkOutput("dt_plot", height = "500px"), type="html", loader="loader4")),
                   box(title = "Pruning Diagnostic", width = 4, status = "warning",
                       plotOutput("cp_plot", height = "250px"),
                       helpText("Optimization: Adjust CP to find the balance between accuracy and complexity."))
                 )),
        tabPanel("Random Forest", 
                 fluidRow(
                   column(7, h4("Actual vs Predicted Results"),
                          withLoader(plotlyOutput("rf_scatter"), type="html", loader="loader4")),
                   column(5, h4("Variable Importance"),
                          plotOutput("rf_imp"))
                 ),
                 hr(),
                 h3("🔍 Churn Simulator (What-If Analysis)"),
                 fluidRow(
                   box(title = "Step 1: Input Customer Features", width = 8, status = "info",
                       column(4, numericInput("sim_age", "Age:", value = 35),
                              selectInput("sim_occ", "Occupation:", choices = NULL)),
                       column(4, numericInput("sim_active", "Active Products:", value = 2),
                              numericInput("sim_tenure", "Tenure (Months):", value = 12)),
                       column(4, sliderInput("sim_satisfaction", "Satisfaction Score:", min = 1, max = 6, value = 3),
                              actionButton("predict_sim", "Predict Risk", class = "btn-warning", style="width: 100%; margin-top: 25px;"))),
                   
                   # Model Confidence (R2) is now integrated into this box (Bottom-Right)
                   box(title = "Step 2: Prediction & Confidence", width = 4, status = "danger", solidHeader = TRUE,
                       div(style = "text-align: center; padding: 10px;",
                           h4("Estimated Risk Probability"),
                           uiOutput("sim_result_ui"),
                           hr(),
                           h4("Model Confidence"),
                           uiOutput("vbox_r2_ui")
                       )
                   )
                 )
        ),
        
        tabPanel("Insights", 
                 icon = icon("lightbulb"),
                 # Added error handling logic to ensure Insights display correctly
                 withLoader(uiOutput("dynamic_insights"), type="html", loader="loader4")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  df_raw <- reactive({
    read.csv("colombia_customers_processed.csv", check.names = FALSE) %>% 
      select(churn_probability, age, occupation, income_bracket,
             active_products, tx_count, customer_tenure, failed_transactions,
             credit_utilization_ratio, satisfaction_score, complaint_topics) %>%
      mutate(across(where(is.character), as.factor))
  })
  
  df_filtered <- reactive({
    data <- df_raw()
    if (input$filter_income != "All") data <- data %>% filter(income_bracket == input$filter_income)
    return(data)
  })
  output$vbox_obs <- renderValueBox({ valueBox(nrow(df_filtered()), "Subgroup Size", icon = icon("users"), color = "purple") })
  output$vbox_churn <- renderValueBox({ 
    avg <- mean(df_filtered()$churn_probability) * 100
    valueBox(paste0(round(avg, 1), "%"), "Average Risk", icon = icon("warning"), color = "red") 
  })
  output$vbox_top_occ <- renderValueBox({
    occ_counts <- df_filtered() %>% count(occupation) %>% arrange(desc(n))
    top_val <- if(nrow(occ_counts) > 0) as.character(occ_counts$occupation[1]) else "N/A"
    valueBox(top_val, "Top Occupation", icon = icon("briefcase"), color = "olive")
  })
  output$vbox_r2_ui <- renderUI({
    # Use tryCatch to prevent errors before model is trained
    res <- tryCatch(rf_model_run(), error = function(e) NULL)
    if (is.null(res)) {
      span("N/A (Run RF Model First)", style="color: grey; font-style: italic;")
    } else {
      r2 <- cor(res$test_data$churn_probability, res$test_data$pred)^2
      div(style = "color: #3c8dbc; font-size: 24px; font-weight: bold;",
          paste0("R² = ", round(r2, 3)))
    }
  })
  dt_model <- reactive({ rpart(churn_probability ~ ., data = df_filtered(), method = "anova", control = rpart.control(cp = input$cp, maxdepth = input$max_depth)) })
  output$dt_plot <- renderVisNetwork({ visTree(dt_model(), width = "100%", colorY = c("#E1F5FE", "#01579B")) })
  output$cp_plot <- renderPlot({ plotcp(dt_model()) })
  
  observe({ updateSelectInput(session, "sim_occ", choices = levels(df_raw()$occupation)) })
  rf_model_run <- eventReactive(input$run_rf, {
    data <- df_filtered()
    set.seed(1234)
    trainIndex <- createDataPartition(data$churn_probability, p = 0.8, list = FALSE)
    fit <- ranger(churn_probability ~ ., data = data[trainIndex, ], num.trees = input$num_trees, importance = "impurity")
    test_data <- data[-trainIndex, ]; test_data$pred <- predict(fit, test_data)$predictions
    list(model = fit, test_data = test_data)
  })
  
  output$rf_scatter <- renderPlotly({ ggplotly(ggplot(rf_model_run()$test_data, aes(x = churn_probability, y = pred)) + geom_point(alpha = 0.4, color = "darkgreen") + geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + theme_minimal()) })
  output$rf_imp <- renderPlot({
    imp <- as.data.frame(rf_model_run()$model$variable.importance) %>% rename(Importance = 1) %>% mutate(Variable = rownames(.))
    ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) + geom_bar(stat = "identity", fill = "#3498db") + coord_flip() + theme_minimal()
  })
  output$dynamic_insights <- renderUI({
    res <- tryCatch(rf_model_run(), error = function(e) NULL)
    
    if (is.null(res)) {
      return(h4("Please click 'Run Random Forest' in the sidebar to generate insights.", style="color: grey; padding: 20px;"))
    }
    
    r2 <- cor(res$test_data$churn_probability, res$test_data$pred)^2
    imp <- res$model$variable.importance
    top_feature <- names(sort(imp, decreasing = TRUE)[1])
    
    fluidPage(
      h3(paste("Automated Model Insights (Trees:", input$num_trees, ")"), style="color: #2c3e50; font-weight: bold;"),
      p("Based on the current Random Forest execution, we have identified several key patterns:"),
      tags$ul(
        tags$li(strong("Model Reliability:"), 
                if(r2 > 0.6) "The model shows high explanatory power, meaning predictions are statistically robust." 
                else "The model shows moderate accuracy. Consider refining input features for better confidence."),
        tags$li(strong("Top Predictor:"), 
                paste("The variable", strong(top_feature), "is the most significant driver of churn probability in this subgroup.")),
        tags$li(strong("Key Observation:"), 
                if(input$num_trees < 50) "Low tree count detected. Increasing the number of trees may lead to a more stable R-squared value."
                else "The forest size is sufficient for a stable baseline prediction.")
      )
    )
  })
  sim_prediction <- eventReactive(input$predict_sim, {
    req(rf_model_run())
    new_data <- data.frame(age = input$sim_age, occupation = factor(input$sim_occ, levels = levels(df_raw()$occupation)),
                           income_bracket = factor("Medium", levels = levels(df_raw()$income_bracket)),
                           active_products = input$sim_active, tx_count = 50, customer_tenure = input$sim_tenure,
                           failed_transactions = 0, credit_utilization_ratio = 0.3, satisfaction_score = input$sim_satisfaction,
                           complaint_topics = factor("No Complaints", levels = levels(df_raw()$complaint_topics)))
    round(predict(rf_model_run()$model, new_data)$predictions * 100, 2)
  })
  
  output$sim_result_ui <- renderUI({
    prob <- sim_prediction(); color <- ifelse(prob > 70, "#dd4b39", "#00a65a")
    tagList(div(style = paste0("font-size: 40px; font-weight: bold; color: ", color, ";"), paste0(prob, "%")),
            p(style="font-size: 16px;", ifelse(prob > 70, "High Risk", "Low Risk")))
  })
}

shinyApp(ui, server)