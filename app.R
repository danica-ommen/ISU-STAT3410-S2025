library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 341: Lab #0"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Instruction ----
      h4("Welcome to STAT 341!"),
      p("As an introduction to using Shiny apps in R, please click the"),
      h6('Show Plot', align="center"),
      p("button below to see a summary of the first major for all the other 
        students in the class."),
      
      # Click Button ----
      actionButton("do", "Show Plot")
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Bar Chart of Majors ----
      plotOutput(outputId = "BarChart")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  observeEvent(input$do, {
    # Bar Chart of Majors ----
    output$BarChart <- renderPlot({
      major_data <- read.csv("STAT341_majors.csv")
      barplot(table(major_data), las=2)
    })
  })
  
}

shinyApp(ui = ui, server = server)