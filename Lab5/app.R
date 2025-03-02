library(shiny)
library(ggplot2)
#runApp("~/Box Sync/STAT-341-DMO/Fall2022/Labs/Lab4/Lab4_shiny")
#runApp("~/Library/CloudStorage/Box-Box/STAT-341-DMO/Fall2022/Labs/Lab4/Lab4_shiny")

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #5"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("Activity #1", 
                       h3("Binomial Distribution"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Number of Trials (n) ----
                           numericInput(inputId = "nBinom",
                                        label = "Size",
                                        value = 1,
                                        min = 1),
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pBinom",
                                        label = "Prob",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Action button to run experiment with specified inputs ----
                           actionButton("doBinom", "Plot")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Binomial RV ----
                           plotOutput("BinomHist")
                         )
                       )),
              tabPanel("Activity #2", 
                       h3("Geometric Distribution"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pGeom",
                                        label = "Prob",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Action button to run experiment with specified inputs ----
                           actionButton("doGeom", "Plot")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Binomial RV ----
                           plotOutput("GeomHist")
                         )
                       )),
              tabPanel("Activity #3", 
                       h3("Negative Binomial Distribution"),
                       
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Numeric entry for choosing Number of Successes (r) ----
                           numericInput(inputId = "rNegBin",
                                        label = "Size",
                                        value = 1,
                                        min = 1),
                           # Input: Numeric entry for probability of success (p) ----
                           numericInput(inputId = "pNegBin",
                                        label = "Prob",
                                        value = 0.5,
                                        min = 0, max = 1),
                           # Input: Action button to run experiment with specified inputs ----
                           actionButton("doNegBin", "Plot")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Probability Histogram for Binomial RV ----
                           plotOutput("NegBinHist")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  observeEvent(input$doBinom, {
    # display probability histogram for the Binomial distribution
    output$BinomHist <- renderPlot({
      y<- c(0:input$nBinom)
      proby<- dbinom(0:input$nBinom, input$nBinom, input$pBinom)
      Bars<- as.data.frame(cbind(y, proby))
      #Dist<- paste("n = ", as.character(input$nBinom), ",", 
      #             " p = ", as.character(input$pBinom), sep = "")
      ggplot(Bars, aes(x = y, y = proby))+ 
        geom_bar(stat="identity", width = 1, fill = "blue", 
                 colour = "black")+
        labs(x = "y = number of successes",
             y = "p(y)")+#,
             #title = "Binomial Distribution",
             #subtitle = Dist)+
        theme_bw()+
        theme(axis.title.y = element_text(size = rel(1.4)),
              axis.title.x = element_text(size = rel(1.4)),
              axis.text.x = element_text(size = rel(1.2)),
              axis.text.y = element_text(size = rel(1.2)),
              plot.title = element_text(hjust=0.5, size = rel(1.75)),
              plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
    }) 
  })
  
  observeEvent(input$doGeom, {
    # display probability histogram for the Geometric distribution
    output$GeomHist <- renderPlot({
      y<- c(0:qgeom(0.9999, input$pGeom))
      proby<- dgeom(y, input$pGeom)
      Bars<- as.data.frame(cbind(y, proby))
      #Dist<- paste("p =", as.character(input$pGeom))
      ggplot(Bars, aes(x = y+1, y = proby))+ 
        geom_bar(stat="identity", width = 1, fill = "blue", 
                 colour = "black")+
        labs(x = "y = number of trials to get 1st success",
             y = "p(y)")+#,
             #title = "Geometric Distribution",
             #subtitle = Dist)+
        theme_bw()+
        theme(axis.title.y = element_text(size = rel(1.4)),
              axis.title.x = element_text(size = rel(1.4)),
              axis.text.x = element_text(size = rel(1.2)),
              axis.text.y = element_text(size = rel(1.2)),
              plot.title = element_text(hjust=0.5, size = rel(1.75)),
              plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
    }) 
  })
  
  observeEvent(input$doNegBin, {
    # display probability histogram for the Negative Binomial distribution
    output$NegBinHist <- renderPlot({
      y<- c(0:qnbinom(0.9999, input$rNegBin, input$pNegBin))
      proby<- dnbinom(y, input$rNegBin, input$pNegBin)
      Bars<- as.data.frame(cbind(y, proby))
      #Dist<- paste("r = ", as.character(input$rNegBin), ",",
      #             " p = ", as.character(input$pNegBin), sep = "")
      ggplot(Bars, aes(x = y+input$rNegBin, y = proby))+ 
        geom_bar(stat="identity", width = 1, fill = "blue", 
                 colour = "black")+
        labs(x = "y = number of trials to get r successes",
             y = "p(y)")+#,
             #title = "Negative Binomial Distribution",
             #subtitle = Dist)+
        theme_bw()+
        theme(axis.title.y = element_text(size = rel(1.4)),
              axis.title.x = element_text(size = rel(1.4)),
              axis.text.x = element_text(size = rel(1.2)),
              axis.text.y = element_text(size = rel(1.2)),
              plot.title = element_text(hjust=0.5, size = rel(1.75)),
              plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
    }) 
  })
  
}

shinyApp(ui = ui, server = server)