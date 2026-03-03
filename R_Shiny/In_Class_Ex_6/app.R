#pacman 可以取代liabrary
pacman::p_load(shiny, tidyverse)

exam <- read_csv("data/Exam_data.csv")

#print(exam)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pupils Exam Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bin",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value = 10)
    ),
    mainPanel(
      #heatmaply r 
      plotOutput("distPlot") #need to be unique 
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- unlist(exam[,input$variable])
    ggplot(exam,aes(x))+
      geom_histogram(bins = input$bin,
                     color = "black",
                     fill = "light blue")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)