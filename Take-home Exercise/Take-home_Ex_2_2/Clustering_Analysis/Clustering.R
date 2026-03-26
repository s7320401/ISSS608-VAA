#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ==============================================================================
# Take-home Exercise 2: Strategic Visual Analytics - "Active Strategy & Action"
# Module: Dynamic Customer Segmentation & Priority Recommendations
# Author: SU Bo-Han | Updated: March 2026
# ==============================================================================

pacman::p_load(shiny, shinydashboard, tidyverse, scales, 
               lubridate, knitr, tidymodels, fpc, forcats, plotly, DT, shinyWidgets)

# 1. Load Data
# 確保路徑與您的環境一致
df_raw <- read_csv("data/colombia_customers_processed.csv")

# 2. Helper Functions
get_top_n_labels <- function(v, n = 3) {
  v <- na.omit(v)
  if (length(v) == 0) return("None")
  total_count <- length(v)
  top_n <- as.data.frame(table(v)) %>% 
    rename(Occupation = v) %>% arrange(desc(Freq)) %>% slice_head(n = n) %>% 
    mutate(label = paste0(Occupation, " (", round(Freq / total_count * 100, 1), "%)"))
  paste(top_n$label, collapse = ", ")
}

# ==============================================================================
# UI DESIGN
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Columbia Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Strategic Dashboard", tabName = "dash_tab", icon = icon("rocket")),
      menuItem("Data Explorer", tabName = "data_tab", icon = icon("database"))
    ),
    hr(),
    h4(" Settings", style = "margin-left: 15px;"),
    selectInput("sample_size", "Data Sample Size:", 
                choices = c("5,000" = 5000, "10,000" = 10000, "20,000" = 20000), 
                selected = 5000),
    sliderInput("k_val", "Clusters (k):", 2, 8, 3),
    
    h4(" Strategic Weights", style = "margin-left: 15px;"),
    sliderInput("w_vol", "Volume Weight (Value):", 0, 1, 0.6, step = 0.1),
    sliderInput("w_risk", "Risk Weight (Stability):", 0, 1, 0.3, step = 0.1),
    
    # 點擊此按鈕後才會啟動 results() 的運算
    actionButton("run_analysis", "Execute Analysis", icon = icon("sync"), 
                 class = "btn-primary", style = "width: 85%; margin-left: 15px;")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .active-strat-card { 
        border-radius: 15px; padding: 25px; text-align: center; 
        border: 3px solid #3c8dbc; background: #ffffff; 
        box-shadow: 0 6px 20px rgba(0,0,0,0.15); 
        max-width: 350px; margin: 0 auto;
      }
      .active-strat-card i { font-size: 50px; color: #3c8dbc; margin-bottom: 15px; }
      .active-strat-card h3 { font-weight: bold; margin-top: 0; color: #2c3e50; }
      .insight-section { font-size: 16px; line-height: 1.7; color: #34495e; }
      .priority-list { background: #fdfdfd; padding: 20px; border-radius: 10px; border: 1px solid #eee; }
      .welcome-msg { text-align: center; padding: 50px; color: #7f8c8d; }
    "))),
    
    tabItems(
      tabItem(tabName = "dash_tab",
              # 🥇🥈🥉 GOLD, SILVER, BRONZE TOP BOXES
              fluidRow(uiOutput("priority_boxes")),
              
              # 💡 STRATEGIC RECOMMENDATIONS & ACTIVE CARD
              fluidRow(
                box(width = 12, title = "💡 Strategic Analytics & Recommendations", status = "info", solidHeader = TRUE,
                    fluidRow(
                      column(width = 4, uiOutput("active_strategy_card")), 
                      column(width = 8, class = "insight-section", uiOutput("strategic_insight_text")) 
                    )
                )
              ),
              
              # 可視化區塊：如果沒按下按鈕，會顯示等待訊息
              fluidRow(
                box(width = 4, title = "Model Diagnosis (Elbow Plot)", status = "warning", 
                    uiOutput("elbow_ui")),
                box(width = 8, title = "Cluster Behavioral Map", status = "primary", 
                    uiOutput("scatter_ui"))
              ),
              
              fluidRow(
                box(width = 6, title = "Retention Quadrant (Value vs. Risk)", status = "info", 
                    plotlyOutput("quadrant_plot")),
                box(width = 6, title = "Occupation Mix by Cluster", status = "success", 
                    plotlyOutput("occ_dist_plot"))
              ),
              
              fluidRow(
                box(width = 12, title = "Detailed Segment Persona Summary", status = "primary", 
                    DTOutput("persona_table"))
              )
      ),
      tabItem(tabName = "data_tab", box(width = 12, title = "Raw Clustered Data", DTOutput("raw_table")))
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # 1. Reactive Clustering - 設定 ignoreNULL = TRUE 確保啟動時不自動運行
  results <- eventReactive(input$run_analysis, {
    set.seed(123)
    n_s <- as.numeric(input$sample_size)
    df_s <- df_raw %>% sample_n(min(n_s, n()))
    df_t <- df_s %>% mutate(v_f = log10(total_transaction_volume + 1), 
                            a_f = log10(average_transaction_value + 1))
    
    recipe_obj <- recipe(~ v_f + a_f, data = df_t) %>% step_normalize(all_predictors()) %>% prep()
    df_c <- bake(recipe_obj, new_data = NULL)
    
    km <- kmeans(df_c, centers = input$k_val, nstart = 25)
    list(data = df_s %>% mutate(cluster = as.factor(km$cluster)), cleaned = df_c, model = km)
  }, ignoreNULL = TRUE) # 關鍵修改：啟動時不執行
  
  # 2. Priority Logic
  priority_data <- reactive({
    req(results()) # 確保結果存在才運算
    res <- results()$data
    res %>% group_by(occupation) %>%
      summarise(count = n(), avg_vol = mean(total_transaction_volume, na.rm = TRUE),
                avg_sat = mean(satisfaction_score, na.rm = TRUE), avg_churn = mean(churn_probability, na.rm = TRUE)) %>%
      filter(count > 10) %>%
      mutate(score = as.numeric((input$w_vol * scale(log10(avg_vol + 1))) + (0.2 * scale(avg_sat)) - (input$w_risk * scale(avg_churn)))) %>%
      arrange(desc(score)) %>% head(3)
  })
  
  output$priority_boxes <- renderUI({
    req(priority_data())
    d <- priority_data()
    colors <- c("yellow", "teal", "orange"); icons <- c("crown", "medal", "star"); labels <- c("🥇 Rank 1", "🥈 Rank 2", "🥉 Rank 3")
    lapply(seq_len(nrow(d)), function(i) {
      column(width = 4, infoBox(labels[i], d$occupation[i], subtitle = paste("Score:", round(d$score[i], 2)), icon = icon(icons[i]), color = colors[i], fill = TRUE, width = NULL))
    })
  })
  
  # 3. Strategy Mode - 這部分可以即時響應 UI，讓使用者知道目前選的是什麼策略
  current_mode <- reactive({
    v_h <- input$w_vol > 0.5; r_h <- input$w_risk > 0.5
    if (v_h && r_h) return(list(m="Premium", i="gem", d="Elite Growth Focus"))
    if (v_h && !r_h) return(list(m="Aggressive", i="rocket", d="Revenue Expansion"))
    if (!v_h && r_h) return(list(m="Defensive", i="shield-alt", d="Risk Mitigation"))
    return(list(m="Balanced", i="balance-scale", d="Market Stability"))
  })
  
  output$active_strategy_card <- renderUI({
    mode_info <- current_mode()
    div(class = "active-strat-card",
        icon(mode_info$i),
        h3(mode_info$m),
        p(mode_info$d),
        hr(),
        p(strong(if(is.null(results())) "Pending Execution..." else "Active Strategy Mode"))
    )
  })
  
  # 4. Strategic Insight Text
  output$strategic_insight_text <- renderUI({
    if (is.null(results())) {
      return(div(class = "welcome-msg", h3("Welcome to Strategic Dashboard"), p("Please adjust the weights on the left and click 'Execute Analysis' to begin.")))
    }
    
    req(priority_data())
    d <- priority_data()
    mode <- current_mode()$m
    top_occ <- d$occupation[1]
    sec_occ <- d$occupation[2]
    
    tagList(
      h3(paste(mode, "Strategy Execution")),
      p(paste0("Your current weight configuration prioritizes ", tolower(mode), 
               " objectives. This approach identifies segments that contribute most to your specific business goal.")),
      div(class = "priority-list",
          p(strong("Primary Focus: "), paste0("Targeting the '", top_occ, "' segment is crucial. ")),
          tags$ul(
            tags$li(strong("Managerial Action: "), paste0("Launch specialized retention campaigns for '", top_occ, "'.")),
            tags$li(strong("Secondary Opportunity: "), paste0("Monitor '", sec_occ, "' for cross-selling opportunities."))
          )
      )
    )
  })
  
  # 5. Visual Outputs - 全部加上 req(results())
  
  output$elbow_ui <- renderUI({
    if(is.null(results())) p("Waiting for analysis...", class="welcome-msg") else plotOutput("elbow_plot", height = "250px")
  })
  
  output$scatter_ui <- renderUI({
    if(is.null(results())) p("Waiting for analysis...", class="welcome-msg") else plotlyOutput("cluster_scatter", height = "250px")
  })
  
  output$elbow_plot <- renderPlot({
    req(results())
    res <- results()$cleaned; k_range <- 1:min(8, nrow(res)-1)
    wss <- map_dbl(k_range, ~kmeans(res, .x, nstart = 5)$tot.withinss)
    ggplot(data.frame(k = k_range, wss = wss), aes(k, wss)) + geom_line(color = "steelblue", size = 1) + geom_point(size = 3) + scale_x_continuous(breaks = k_range) + theme_minimal()
  })
  
  output$cluster_scatter <- renderPlotly({
    req(results())
    p <- ggplot(results()$data, aes(x = total_transaction_volume, y = average_transaction_value, color = cluster, text = occupation)) +
      geom_point(alpha = 0.5) + scale_x_log10(labels = label_comma()) + scale_y_log10(labels = label_comma()) + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$quadrant_plot <- renderPlotly({
    req(results())
    res <- results()$data %>% group_by(occupation) %>% summarise(v = mean(total_transaction_volume), r = mean(churn_probability), n = n()) %>% filter(n > 5)
    p <- ggplot(res, aes(x = r, y = v, size = n, text = occupation)) + geom_point(color = "darkgrey", alpha = 0.6) + 
      scale_y_log10(labels = label_number(suffix = "M", scale = 1e-6)) + geom_vline(xintercept = mean(res$r, na.rm=T), linetype = "dashed", color = "red") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$occ_dist_plot <- renderPlotly({
    req(results())
    p <- results()$data %>% mutate(occ = fct_lump(occupation, n = 10)) %>% ggplot(aes(x = cluster, fill = occ)) + geom_bar(position = "fill") + scale_y_continuous(labels = label_percent()) + coord_flip() + theme_minimal()
    ggplotly(p)
  })
  
  output$persona_table <- renderDT({
    req(results())
    results()$data %>% group_by(cluster) %>% summarise(n = n(), Personas = get_top_n_labels(occupation, n = 3), Avg_Vol = label_comma()(round(mean(total_transaction_volume), 0)), Churn = label_percent(0.1)(mean(churn_probability))) %>% datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  output$raw_table <- renderDT({ req(results()); results()$data %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
}

shinyApp(ui, server)