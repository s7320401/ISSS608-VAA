#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, shinydashboard, tidyverse, scales, lubridate, knitr, tidymodels, fpc, forcats, plotly, DT, shinyWidgets)

df_raw <- read.csv("data/colombia_customers_processed.csv")

get_top_n_labels <- function(v, n = 3) {
  v <- na.omit(v)
  if (length(v) == 0) return("None")
  total_count <- length(v)
  top_n <- as.data.frame(table(v)) %>% 
    rename(Occupation = v) %>% arrange(desc(Freq)) %>% slice_head(n = n) %>% 
    mutate(label = paste0(Occupation, " (", round(Freq / total_count * 100, 1), "%)"))
  paste(top_n$label, collapse = ", ")
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Clustering Analysis"),
  
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
    
    actionButton("run_analysis", "Execute Analysis", icon = icon("sync"), 
                 class = "btn-primary", style = "width: 85%; margin-left: 15px;")
  ),
  
  dashboardBody(
    # Custom CSS for UI Enhancement
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
              
              # Standard Visual Analytics
              fluidRow(
                box(width = 4, title = "Model Diagnosis (Elbow Plot)", status = "warning", plotOutput("elbow_plot", height = "250px")),
                box(width = 8, title = "Cluster Behavioral Map", status = "primary", plotlyOutput("cluster_scatter", height = "250px"))
              ),
              
              fluidRow(
                box(width = 6, title = "Retention Quadrant (Value vs. Risk)", status = "info", plotlyOutput("quadrant_plot")),
                box(width = 6, title = "Occupation Mix by Cluster", status = "success", plotlyOutput("occ_dist_plot"))
              ),
              
              fluidRow(
                box(width = 12, title = "Detailed Segment Persona Summary", status = "primary", DTOutput("persona_table"))
              )
      ),
      tabItem(tabName = "data_tab", box(width = 12, title = "Raw Clustered Data", DTOutput("raw_table")))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # 1. Reactive Clustering
  results <- eventReactive(input$run_analysis, {
    set.seed(123)
    n_s <- as.numeric(input$sample_size)
    df_s <- df_raw %>% sample_n(min(n_s, n()))
    df_t <- df_s %>% mutate(v_f = log10(total_transaction_volume + 1), a_f = log10(average_transaction_value + 1))
    recipe_obj <- recipe(~ v_f + a_f, data = df_t) %>% step_normalize(all_predictors()) %>% prep()
    df_c <- bake(recipe_obj, new_data = NULL)
    km <- kmeans(df_c, centers = input$k_val, nstart = 25)
    list(data = df_s %>% mutate(cluster = as.factor(km$cluster)), cleaned = df_c, model = km)
  }, ignoreNULL = FALSE)
  
  # 2. Priority Logic (Gold, Silver, Bronze Ranking)
  priority_data <- reactive({
    res <- results()$data
    res %>% group_by(occupation) %>%
      summarise(count = n(), avg_vol = mean(total_transaction_volume, na.rm = TRUE),
                avg_sat = mean(satisfaction_score, na.rm = TRUE), avg_churn = mean(churn_probability, na.rm = TRUE)) %>%
      filter(count > 10) %>%
      mutate(score = as.numeric((input$w_vol * scale(log10(avg_vol + 1))) + (0.2 * scale(avg_sat)) - (input$w_risk * scale(avg_churn)))) %>%
      arrange(desc(score)) %>% head(3)
  })
  
  output$priority_boxes <- renderUI({
    d <- priority_data(); if(nrow(d) == 0) return(NULL)
    colors <- c("yellow", "teal", "orange"); icons <- c("crown", "medal", "star"); labels <- c("🥇 Rank 1", "🥈 Rank 2", "🥉 Rank 3")
    lapply(seq_len(nrow(d)), function(i) {
      column(width = 4, infoBox(labels[i], d$occupation[i], subtitle = paste("Score:", round(d$score[i], 2)), icon = icon(icons[i]), color = colors[i], fill = TRUE, width = NULL))
    })
  })
  
  # 3. Strategy Mode Determination
  current_mode <- reactive({
    v_h <- input$w_vol > 0.5; r_h <- input$w_risk > 0.5
    if (v_h && r_h) return(list(m="Premium", i="gem", d="Elite Growth Focus"))
    if (v_h && !r_h) return(list(m="Aggressive", i="rocket", d="Revenue Expansion"))
    if (!v_h && r_h) return(list(m="Defensive", i="shield-alt", d="Risk Mitigation"))
    return(list(m="Balanced", i="balance-scale", d="Market Stability"))
  })
  
  # Render ONLY the Active Strategy Card
  output$active_strategy_card <- renderUI({
    mode_info <- current_mode()
    div(class = "active-strat-card",
        icon(mode_info$i),
        h3(mode_info$m),
        p(mode_info$d),
        hr(),
        p(strong("Active Strategy Mode"))
    )
  })
  
  # 4. DYNAMIC STRATEGIC RECOMMENDATIONS (Segment description removed)
  output$strategic_insight_text <- renderUI({
    d <- priority_data()
    if(nrow(d) == 0) return("Insufficient data for analysis. Please adjust filters.")
    
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
            tags$li(strong("Managerial Action: "), paste0("Given their top-tier priority score, we recommend launching specialized retention campaigns for '", top_occ, "' immediately.")),
            tags$li(strong("Secondary Opportunity: "), paste0("Monitoring the '", sec_occ, "' group for potential cross-selling opportunities as they align closely with your current strategy."))
          )
      ),
      br(),
      p(em("Note: Priorities are dynamically re-calculated based on standardized Volume, Satisfaction, and Risk metrics whenever you adjust the sliders."))
    )
  })
  
  # 5. Standard Visual Outputs
  output$elbow_plot <- renderPlot({
    res <- results()$cleaned; k_range <- 1:min(8, nrow(res)-1)
    wss <- map_dbl(k_range, ~kmeans(res, .x, nstart = 5)$tot.withinss)
    ggplot(data.frame(k = k_range, wss = wss), aes(k, wss)) + geom_line(color = "steelblue", size = 1) + geom_point(size = 3) + scale_x_continuous(breaks = k_range) + theme_minimal() + labs(x="Number of Clusters", y="WSS")
  })
  
  output$cluster_scatter <- renderPlotly({
    p <- ggplot(results()$data, aes(x = total_transaction_volume, y = average_transaction_value, color = cluster, text = occupation)) +
      geom_point(alpha = 0.5) + scale_x_log10(labels = label_comma()) + scale_y_log10(labels = label_comma()) + theme_minimal() + labs(x="Volume (Log Scale)", y="Value (Log Scale)")
    ggplotly(p, tooltip = "text")
  })
  
  output$quadrant_plot <- renderPlotly({
    res <- results()$data %>% group_by(occupation) %>% summarise(v = mean(total_transaction_volume), r = mean(churn_probability), n = n()) %>% filter(n > 5)
    p <- ggplot(res, aes(x = r, y = v, size = n, text = occupation)) + geom_point(color = "darkgrey", alpha = 0.6) + 
      scale_y_log10(labels = label_number(suffix = "M", scale = 1e-6)) + geom_vline(xintercept = mean(res$r, na.rm=T), linetype = "dashed", color = "red") + theme_minimal() + labs(x="Churn Risk", y="Avg Volume (M)")
    ggplotly(p, tooltip = "text")
  })
  
  output$occ_dist_plot <- renderPlotly({
    p <- results()$data %>% mutate(occ = fct_lump(occupation, n = 10)) %>% ggplot(aes(x = cluster, fill = occ)) + geom_bar(position = "fill") + scale_y_continuous(labels = label_percent()) + coord_flip() + theme_minimal() + labs(x="Cluster", y="Proportion", fill="Occupation")
    ggplotly(p)
  })
  
  output$persona_table <- renderDT({
    results()$data %>% group_by(cluster) %>% summarise(n = n(), Personas = get_top_n_labels(occupation, n = 3), Avg_Vol = label_comma()(round(mean(total_transaction_volume), 0)), Churn = label_percent(0.1)(mean(churn_probability))) %>% datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  output$raw_table <- renderDT({ results()$data %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
}

shinyApp(ui, server)