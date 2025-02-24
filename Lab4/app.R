library(shiny)
#runApp("~/Box Sync/STAT-341-DMO/Spring2025/Labs/Lab4/Lab4_shiny")

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #4"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("Activity #1", 
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           radioButtons("experiment", "Experiment Type:",
                                        c("Individual" = "indiv",
                                          "Whole Class" = "class"))
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: HTML table of observations ----
                           tableOutput("tab")
                         )
                       )),
              tabPanel("Activity #2", 
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Number of sides on the dice ----
                           selectInput("sides", "Number of Sides on the Dice:",
                                       c("6" = "six",
                                         "10" = "ten")),
                           radioButtons("select", "Output Type:",
                                        c("Outcomes: y" = "y",
                                          "Probabilities: p(y)" = "py",
                                          "Expected Value: E(Y)" = "mean",
                                          "2nd Raw Moment: E(Y^2)" = "mom2",
                                          "Variance: V(Y)" = "var"))
                           
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Verbatim text for R code & results ----
                           verbatimTextOutput("result")
                         )
                       )),
              tabPanel("Activity #3", 
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Custom currency format for with basic animation ----
                           sliderInput("grand", "Grand Prize (in millions):",
                                       min = 22, max = 722,
                                       value = 402, step = 20,
                                       pre = "$", post = ",000,000",
                                       animate = TRUE)
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Graph of Grand Prize vs Expected Value ----
                           plotOutput("plot"),
                           # Output: Display the Mean & Variance ----
                           verbatimTextOutput("summary")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # display table of outcomes and frequencies
  output$tab <- renderTable({
    set.seed(3341)
    n <- function(){
      switch(input$experiment,
             indiv = 20,
             class = 20*40)
    }
    Outcome = sample(1:6, size=n(), replace=T)
    table(Outcome)
  })
  
  # Generate the R code and output for selected problem ----
  output$result <- renderPrint({
    
    if(input$sides == "six"){
      y = c(2:12)
      py = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36
      cally = "y = c(2:12)"
      callpy = "py = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36"
      Ey = sum(y*py)
      Ey2 = sum(y^2 * py)
      Vy = Ey2 - Ey^2
    }
    if(input$sides == "ten"){
      y = c(0:18)
      py = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)/100
      cally = "y = c(0:18)"
      callpy = "py = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)/100"
      Ey = sum(y*py)
      Ey2 = sum(y^2 * py)
      Vy = Ey2 - Ey^2
    }
    
    
    d <- function(){
      switch(input$select,
             y = 1,
             py = 2,
             mean = 3,
             mom2 = 4,
             var = 5)
    }
    if(d() == 1){
      cat(paste("R Code:", cally, "\nResult:", paste(y, collapse=" "), sep="\n"))
    }else if(d() == 2){
      cat(paste("R Code:", callpy, "\nResult:", paste(round(py, 4), collapse=" "), sep="\n"))
    }else if(d() == 3){
      cat(paste("R Code:", "Ey = sum(y*py)", "\nResult:", Ey, sep="\n"))
    }else if(d()==4){
      cat(paste("R Code:", "Ey2 = sum(y^2 * py)", "\nResult:", Ey2, sep="\n"))
    }else{
      cat(paste("R Code:", "Vy = Ey2 - Ey^2", "\nResult:", Vy, sep="\n"))
    }
  })
  

  # Display plot of grand prize vs. expected value
  output$plot <- renderPlot({
    thegrands = seq(22, 722, by=20)
    themeans = vector("numeric", length=length(thegrands))
    probs <- c(1, 25, 320, 28160, 920640, 10801392, 280450800)/292201338
    for(i in 1:length(thegrands)){
      prizes <- c(thegrands[i]*1e6, 1000000, 50000, 100, 7, 4, 0)
      themeans[i] = sum(prizes*probs)
    }
    plot(thegrands, themeans, xlab="Grand Prize (in millions of $)", ylab="Expected Winnings", type='l')
    abline(v=input$grand, col="red")
  }) 
  
  # Display mean and variance ----
  output$summary <- renderPrint({
    prizes <- c(input$grand*1e6, 1000000, 50000, 100, 7, 4, 0)
    probs <- c(1, 25, 320, 28160, 920640, 10801392, 280450800)/292201338
    mean <- sum(prizes*probs)
    mom2 <- sum(prizes^2*probs)
    var <- mom2 - mean^2
    cat(paste("Expected Winnings ($):", round(mean,2), "\nVariance of Winnings (sq. $):", round(var,0), sep="\n"))
  })
  
}

shinyApp(ui = ui, server = server)
