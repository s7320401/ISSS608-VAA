pacman::p_load(shiny, bslib, tidyverse, ggstatsplot, plotly, DT, shinyWidgets)

ui <- page_navbar(
  title = "Confirmatory Analysis (Sampled)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Global Data Filters",
    checkboxGroupInput("filter_gender", "Select Gender:", 
                       choices = c("Male", "Female", "Other"), selected = c("Male", "Female", "Other")),
    pickerInput("filter_location", "Location:", 
                choices = NULL, options = list(`actions-box` = TRUE), multiple = TRUE),
    hr(),
    sliderInput("conf_level", "Confidence Level (alpha):", 
                min = 0.90, max = 0.99, value = 0.95, step = 0.01),
    hr(),
    # --- New Execution Button ---
    actionButton("run_analysis", "Run Analysis", 
                 class = "btn-primary", width = "100%", icon = icon("play"))
  ),
  
  nav_panel("Group Comparisons",
            layout_columns(
              card(
                card_header("Test Configuration"),
                selectInput("x_var", "Categorical Variable (X):", 
                            choices = c("income_bracket", "customer_segment", "acquisition_channel")),
                selectInput("y_var", "Numerical Variable (Y):", 
                            choices = c("churn_probability", "app_logins_frequency", "satisfaction_score")),
                radioButtons("test_type", "Statistical Approach:",
                             choices = c("Parametric" = "p", "Non-parametric" = "np"), inline = TRUE)
              ),
              card(
                card_header("Statistical Visualization"),
                plotOutput("cda_plot", height = "500px")
              )
            )
  ),
  
  nav_panel("Correlation Analysis",
            card(
              card_header("Variable Association"),
              plotlyOutput("corr_plot")
            )
  ),
  
  nav_panel("Data Explorer",
            card(
              DTOutput("raw_table")
            )
  )
)

server <- function(input, output, session) {
  
  # 1. Load Data
  df <- read_csv("colombia_customers_processed.csv")
  
  # Initial update for the picker
  updatePickerInput(session, "filter_location", 
                    choices = sort(unique(df$location)), 
                    selected = unique(df$location))
  
  # 2. Reactive Filtered & Sampled Data
  # eventReactive ensures this only runs when 'run_analysis' is clicked
  filtered_df <- eventReactive(input$run_analysis, {
    
    data <- df %>%
      filter(gender %in% input$filter_gender,
             location %in% input$filter_location)
    
    # Check if data size is larger than 5000 before sampling
    if(nrow(data) > 5000) {
      data <- data %>% sample_n(5000)
    }
    
    return(data)
    
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE ensures it runs once on startup
  
  # 3. Render CDA Plot
  output$cda_plot <- renderPlot({
    # Use req() to ensure data exists
    req(filtered_df())
    
    ggbetweenstats(
      data = filtered_df(),
      x = !!sym(input$x_var),
      y = !!sym(input$y_var),
      type = input$test_type,
      conf.level = input$conf_level,
      package = "ggthemes",
      palette = "Tableau_10",
      title = paste("Comparison of", input$y_var, "by", input$x_var, "(n=5000 max)")
    )
  })
  
  # 4. Render Correlation Plot
  output$corr_plot <- renderPlotly({
    req(filtered_df())
    
    p <- ggplot(filtered_df(), aes(x = satisfaction_score, y = churn_probability, color = customer_segment)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm") +
      theme_minimal() +
      labs(title = "Satisfaction vs. Churn Risk (Sampled Data)")
    
    ggplotly(p)
  })
  
  # 5. Render Data Table
  output$raw_table <- renderDT({
    req(filtered_df())
    datatable(filtered_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)