#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(
  shiny, bslib, plotly, tidyverse, caret, 
  ranger, shinycustomloader, bsicons
)

# ---------------------------------------------------------------- 1. UI (使用者介面)
ui <- page_navbar(
  title = "Random Forest Analytics",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # 自定義 CSS：打造一致的奶茶色風格
  tags$head(
    tags$style(HTML("
      /* 奶茶色按鈕樣式 */
      .btn-milktea {
        background-color: #C5A381 !important;
        border-color: #B28F6F !important;
        color: white !important;
        font-weight: 600 !important;
        transition: all 0.3s ease;
      }
      .btn-milktea:hover {
        background-color: #A67B5B !important;
        border-color: #8E6346 !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        color: #f8f9fa !important;
      }
      .btn-milktea:active {
        background-color: #8E6346 !important;
        transform: translateY(2px);
      }
      /* 模擬器標題樣式 */
      .sim-header {
        color: #8D6E63;
        font-weight: bold;
        margin-bottom: 20px;
        border-left: 5px solid #C5A381;
        padding-left: 15px;
      }
    "))
  ),
  
  sidebar = sidebar(
    title = "Model Parameters",
    h5("⚙️ Random Forest Settings"),
    sliderInput("num_trees", "Number of Trees:", min = 10, max = 200, value = 50, step = 10),
    
    hr(),
    # 執行模型按鈕 (奶茶色 + 閃電)
    actionButton("run_rf", "Execute Random Forest", 
                 icon = icon("bolt"), 
                 class = "btn-milktea w-100"),
    br(),
    span("Note: Training may take a few seconds depending on data size.", 
         style = "font-size: 0.8rem; color: gray; margin-top: 10px; display: block;")
  ),
  
  # --- 主要分析面板 ---
  nav_panel(
    title = "Churn Risk - Forest Analysis",
    
    # 上方：模型成效與重要性 (左右並排)
    layout_column_wrap(
      width = 1/2,
      heights_equal = "all",
      card(
        card_header("Actual vs Predicted Results"),
        withLoader(plotlyOutput("rf_scatter", height = "400px"), type="html", loader="loader4"),
        full_screen = TRUE
      ),
      card(
        card_header("Variable Importance (Drivers)"),
        plotOutput("rf_imp", height = "400px"),
        card_footer("Higher values indicate stronger predictors of churn.")
      )
    ),
    
    br(),
    hr(),
    
    # 下方：流失模擬器 (What-If Analysis)
    h3("🔍 Churn Simulator (What-If Analysis)", class = "sim-header"),
    
    layout_column_wrap(
      width = 1,
      card(
        card_body(
          layout_column_wrap(
            width = 1/2,
            # 輸入區域
            div(
              h5("Step 1: Input Customer Features"),
              layout_column_wrap(
                width = 1/2,
                numericInput("sim_age", "Age:", value = 35),
                selectInput("sim_occ", "Occupation:", choices = NULL),
                numericInput("sim_active", "Active Products:", value = 2),
                numericInput("sim_tenure", "Tenure (Months):", value = 12),
                sliderInput("sim_satisfaction", "Satisfaction Score (1-6):", min = 1, max = 6, value = 3)
              ),
              actionButton("predict_sim", "Predict Risk Now", 
                           icon = icon("calculator"), 
                           class = "btn-milktea w-100", style = "margin-top: 20px;")
            ),
            # 結果區域
            div(
              h5("Step 2: Prediction Result"),
              layout_column_wrap(
                width = 1,
                # 預測機率卡片
                uiOutput("sim_result_ui"),
                # 模型信心卡片
                value_box(
                  title = "Model Confidence (R²)",
                  value = textOutput("r2_val"),
                  showcase = bs_icon("shield-check"),
                  theme = "secondary"
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # --- 模型洞察分頁 ---
  nav_panel(
    title = "Strategic Insights",
    card(
      card_header("AI-Generated Insights"),
      withLoader(uiOutput("dynamic_insights"), type="html", loader="loader4")
    )
  )
)

# ---------------------------------------------------------------- 2. Server (伺服器邏輯)
server <- function(input, output, session) {
  
  # A. 數據讀取
  df_raw <- reactive({
    file_path <- "colombia_customers_processed.csv"
    if (!file.exists(file_path)) {
      showNotification("CSV file not found!", type = "error")
      return(NULL)
    }
    
    read.csv(file_path, check.names = FALSE) %>% 
      select(churn_probability, age, occupation, income_bracket,
             active_products, tx_count, customer_tenure, failed_transactions,
             credit_utilization_ratio, satisfaction_score, complaint_topics) %>%
      mutate(across(where(is.character), as.factor))
  })
  
  # 更新下拉選單
  observe({ 
    req(df_raw())
    updateSelectInput(session, "sim_occ", choices = levels(df_raw()$occupation)) 
  })
  
  # B. 訓練 Random Forest 模型
  rf_model_run <- eventReactive(input$run_rf, {
    req(df_raw())
    data <- df_raw()
    set.seed(1234)
    trainIndex <- createDataPartition(data$churn_probability, p = 0.8, list = FALSE)
    
    # 使用 ranger 加速訓練
    fit <- ranger(churn_probability ~ ., 
                  data = data[trainIndex, ], 
                  num.trees = input$num_trees, 
                  importance = "impurity")
    
    test_data <- data[-trainIndex, ]
    test_data$pred <- predict(fit, test_data)$predictions
    list(model = fit, test_data = test_data)
  }, ignoreNULL = TRUE)
  
  # C. 渲染圖表與結果
  output$rf_scatter <- renderPlotly({
    req(rf_model_run())
    p <- ggplot(rf_model_run()$test_data, aes(x = churn_probability, y = pred)) + 
      geom_point(alpha = 0.4, color = "#C5A381") + 
      geom_abline(slope = 1, intercept = 0, color = "#D4A373", linetype = "dashed") + 
      labs(x = "Actual Probability", y = "Predicted Probability") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$rf_imp <- renderPlot({
    req(rf_model_run())
    imp <- as.data.frame(rf_model_run()$model$variable.importance) %>% 
      rename(Importance = 1) %>% 
      mutate(Variable = rownames(.))
    
    ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) + 
      geom_bar(stat = "identity", fill = "#E3D5CA") + 
      geom_text(aes(label = round(Importance, 1)), hjust = -0.1, size = 3) +
      coord_flip() + 
      theme_minimal() +
      labs(x = NULL, y = "Impurity Importance")
  })
  
  output$r2_val <- renderText({
    req(rf_model_run())
    res <- rf_model_run()
    r2 <- cor(res$test_data$churn_probability, res$test_data$pred)^2
    paste0(round(r2 * 100, 1), "%")
  })
  
  # D. 模擬器預測邏輯
  sim_prediction <- eventReactive(input$predict_sim, {
    req(rf_model_run())
    new_data <- data.frame(
      age = input$sim_age, 
      occupation = factor(input$sim_occ, levels = levels(df_raw()$occupation)),
      income_bracket = factor("Medium", levels = levels(df_raw()$income_bracket)),
      active_products = input$sim_active, 
      tx_count = 50, 
      customer_tenure = input$sim_tenure,
      failed_transactions = 0, 
      credit_utilization_ratio = 0.3, 
      satisfaction_score = input$sim_satisfaction,
      complaint_topics = factor("No Complaints", levels = levels(df_raw()$complaint_topics))
    )
    round(predict(rf_model_run()$model, new_data)$predictions * 100, 2)
  })
  
  output$sim_result_ui <- renderUI({
    req(sim_prediction())
    prob <- sim_prediction()
    # 根據風險程度決定顏色 (低風險用奶茶色調，高風險用深咖啡色)
    risk_color <- if(prob > 70) "#8D6E63" else "#C5A381"
    
    value_box(
      title = "Estimated Risk Probability",
      value = paste0(prob, "%"),
      showcase = bs_icon(if(prob > 70) "exclamation-triangle" else "check-circle"),
      theme = value_box_theme(bg = risk_color, fg = "white"),
      p(if(prob > 70) "Priority Action Required" else "Normal Monitoring")
    )
  })
  
  output$dynamic_insights <- renderUI({
    if (is.null(rf_model_run())) return(div(class="welcome-msg", "Please run the model to see insights."))
    tagList(
      h4("Strategic Summary"),
      p("The Random Forest model has identified the primary drivers of customer churn."),
      tags$ul(
        tags$li("Check the variable importance plot to see which factors matter most."),
        tags$li("Use the simulator to test potential customer scenarios.")
      )
    )
  })
}

# 啟動 App
shinyApp(ui, server)