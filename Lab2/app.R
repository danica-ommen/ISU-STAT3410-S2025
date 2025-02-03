library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #2"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Numeric entry for choosing room size ----
      numericInput(inputId = "size",
                  label = "Number of People in Room:",
                  value=20),
      # Input: Numeric entry for number of trials ----
      numericInput(inputId = "sims",
                   label = "Number of Repeated Trials:",
                   value = 1),
      # Input: Action button to run experiment with specified inputs ----
      actionButton("do", "Run Experiment")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      # Output: Bar Chart ----
      plotOutput("bar"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  v <- reactiveValues(countID = 0)
  
  observeEvent(input$do, {
    v$countID <- v$countID + 1
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    set.seed(8889+v$countID)
    birthdays <- vector("logical", length=input$sims)
    for (i in 1:input$sims){
      experiment <- sample(c(1:365), input$size, replace=T)
      birthdays[i] <- sum(table(experiment)>1)>0
    }
    if(input$sims==1){
      # print the number of days that are a shared birthday
      print(paste("Number of Days with Shared Birthday = ", sum(table(experiment)>1)))
    }else{
      # display proportion of trials that had at least one shared birthday
      print(paste("Proportion of Trials with at least 1 Shared Birthday = ", mean(birthdays)))
    }
  })
  
  # Data Display ----
  
    # display table of birthdays
    output$view <- renderTable({
      if(input$sims==1){
        set.seed(8889+v$countID)
        experiment <- sample(c(1:365), input$size, replace=T)
        head(data.frame(Person=c(1:input$size), Birthday=experiment), 20)
      }
    })
    
    # display bar plots of shared birthdays
    output$bar <- renderPlot({
      if(input$sims>1){
        set.seed(8889+v$countID)
        birthdays <- vector("logical", length=input$sims)
        for (i in 1:input$sims){
          experiment <- sample(c(1:365), input$size, replace=T)
          birthdays[i] <- sum(table(experiment)>1)>0
       }
        barplot(table(birthdays), xlab="Shared Birthday?",
                col="#75AADB", border="white",)
      }
    }) 
}

shinyApp(ui = ui, server = server)