library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #1"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for choosing number of flips ----
      numericInput(inputId = "flips",
                  label = "Number of Coin Flips:",
                  value=100),
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "sims",
                   label = "Number of Repeated Trials:",
                   value = 10000),
      # Input: Action button to run experiment with specified inputs ----
      actionButton("do", "Run Experiment")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      # Output: Histogram ----
      plotOutput(outputId = "histPlot"),
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
    Nsims <- input$sims
    Nflips <- input$flips
    flipData <- vector("numeric", length=Nsims)
    for (i in 1:Nsims){
      experiment <- sample(c(0,1), Nflips, replace = T)
      flipData[i] <- sum(experiment)
    }
    if(Nsims==1){
      print(paste("Proportion of Heads = ", flipData/Nflips))
    }else{
      summary(as.numeric(flipData/Nflips)) 
    }
  })
  
  # Histogram of the Coin Flip Data ----
  output$histPlot <- renderPlot({
    set.seed(8889+v$countID)
    Nsims <- input$sims
    Nflips <- input$flips
    flipData <- vector("numeric", length=Nsims)
    for (i in 1:Nsims){
      experiment <- sample(c(0,1), Nflips, replace = T)
      flipData[i] <- sum(experiment)
    }
    if(Nsims==1){
      barplot(table(experiment), xlab="Number of Tails(0) and Heads(1)",
              col="#75AADB", border="white",)
    }else{
      hist(x=as.numeric(flipData/Nflips), breaks=15, col="#75AADB", border="white",
           xlim=c(0,1), xlab = "Proportion of Heads",
           main = "Histogram of Repeated Coin Flipping Experiments") 
    }
  })
}

shinyApp(ui = ui, server = server)