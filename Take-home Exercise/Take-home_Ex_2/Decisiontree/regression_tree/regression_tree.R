pacman::p_load(
  shiny, bslib, tidyverse, rpart, 
  networkD3, data.tree, # 替換用的核心套件
  shinycustomloader, bsicons
)

# ---------------------------------------------------------------- 1. UI
ui <- page_navbar(
  title = "Decision Tree Model",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  # 自定義 CSS：奶茶色按鈕 (完全保留)
  tags$head(
    tags$style(HTML("
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
      /* 額外優化：讓 D3 文字與你的主題更契合 */
      .node text { font-family: 'Inter', sans-serif; font-size: 12px; }
      .link { stroke: #D2B48C !important; stroke-opacity: 0.4; }
    "))
  ),
  
  sidebar = sidebar(
    title = "Model Configuration",
    h5("🌲 Tree Parameters", style = "margin-top: 10px;"),
    sliderInput("cp", "Complexity (CP):", min = 0.0001, max = 0.05, value = 0.001, step = 0.0005),
    sliderInput("max_depth", "Max Depth:", min = 1, max = 15, value = 7),
    
    hr(),
    h5("👥 Filter Population"),
    selectInput("filter_income", "Income Bracket:", 
                choices = c("All", "Low", "Medium", "High")),
    
    hr(),
    # 執行按鈕 (完全保留)
    actionButton(
      "run_model", 
      "Run Analysis", 
      icon = icon("bolt"), 
      class = "btn-milktea w-100"
    ),
    br(),
    span("Adjust parameters and click to update the tree.", 
         style = "font-size: 0.8rem; color: gray; margin-top: 10px; display: block;")
  ),
  
  nav_panel(
    title = "Churn Risk - Tree Analysis",
    
    # 數據卡片 (完全保留)
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Subgroup Size",
        value = textOutput("obs_count"),
        showcase = bs_icon("people-fill"),
        theme = "secondary"
      ),
      value_box(
        title = "Average Risk",
        value = textOutput("avg_risk"),
        showcase = bs_icon("exclamation-triangle-fill"),
        theme = "danger"
      ),
      value_box(
        title = "Top Occupation",
        value = textOutput("top_occ"),
        showcase = bs_icon("briefcase-fill"),
        theme = "info"
      )
    ),
    
    br(),
    
    # 圖表區域：僅將 visNetworkOutput 替換為 diagonalNetworkOutput
    layout_column_wrap(
      width = 1/2, 
      heights_equal = "all",
      card(
        card_header("Interactive Decision Path"),
        withLoader(diagonalNetworkOutput("dt_plot", height = "500px"), 
                   type="html", loader="loader4"),
        full_screen = TRUE
      ),
      card(
        card_header("Pruning Diagnostic (CP Plot)"),
        plotOutput("cp_plot", height = "500px"),
        card_footer("Note: CP plot helps to find the optimal tree size.")
      )
    )
  )
)

# ---------------------------------------------------------------- 2. Server
server <- function(input, output, session) {
  
  # 1. 原始數據讀取 (完全保留)
  df_raw <- reactive({
    file_path <- "colombia_customers_processed.csv"
    validate(
      need(file.exists(file_path), paste("找不到檔案：", file_path, "，請檢查路徑。"))
    )
    
    read.csv(file_path, check.names = FALSE) %>% 
      select(churn_probability, age, occupation, income_bracket,
             active_products, tx_count, customer_tenure, failed_transactions,
             credit_utilization_ratio, satisfaction_score, complaint_topics) %>% 
      mutate(across(where(is.character), as.factor))
  })
  
  # 2. 監聽按鈕 (完全保留模型運算邏輯)
  model_data <- eventReactive(input$run_model, {
    req(df_raw())
    
    data <- df_raw()
    if (input$filter_income != "All") {
      data <- data %>% filter(income_bracket == input$filter_income)
    }
    
    fit <- rpart(churn_probability ~ ., data = data, method = "anova", 
                 control = rpart.control(cp = input$cp, maxdepth = input$max_depth))
    
    list(df = data, model = fit)
  }, ignoreNULL = TRUE) 
  
  # 3. 渲染輸出 (指標部分完全保留)
  output$obs_count <- renderText({ 
    req(model_data())
    nrow(model_data()$df) 
  })
  
  output$avg_risk <- renderText({ 
    req(model_data())
    avg <- mean(model_data()$df$churn_probability) * 100
    paste0(round(avg, 1), "%")
  })
  
  output$top_occ <- renderText({
    req(model_data())
    occ_counts <- model_data()$df %>% count(occupation) %>% arrange(desc(n))
    if(nrow(occ_counts) > 0) as.character(occ_counts$occupation[1]) else "N/A"
  })
  
  # 替換視覺化：使用 networkD3 的 diagonalNetwork
  output$dt_plot <- renderDiagonalNetwork({ 
    req(model_data())
    fit <- model_data()$model
    req(nrow(fit$frame) > 1) 
    
    # 將 rpart 轉換為 networkD3 需要的 List 格式
    tree_node <- as.Node(fit)
    tree_list <- ToListExplicit(tree_node, unname = TRUE)
    
    diagonalNetwork(
      List = tree_list, 
      fontSize = 13, 
      fontFamily = "Inter",
      opacity = 0.9,
      margin = list(top = 20, right = 100, bottom = 20, left = 80)
    )
  })
  
  # CP Plot (完全保留)
  output$cp_plot <- renderPlot({ 
    req(model_data())
    plotcp(model_data()$model) 
  })
}

# 啟動 App
shinyApp(ui, server)