library(shiny)
library(ggplot2)
library(extraDistr)

# Define UI for app ----
ui <- fluidPage(
  
  titlePanel("STAT 3410: Lab #6"),
  
  tabsetPanel(type="tabs",
              tabPanel("Activity #1",
                h3("Hypergeometric Distribution"),
                
                # sidebar layout with input and output definitions
                sidebarLayout(
                  sidebarPanel(
                    # input
                    numericInput(inputId = 'mHyper',
                      label = "M - Pop. Success",
                      value = 1,
                      min = 0
                    ),
                    
                    numericInput(inputId = 'NHyper',
                      label = 'N - Pop. Size',
                      value = 1,
                      min = 0
                    ),
                    
                    numericInput(inputId = 'drawHyper',
                      label = 'n - Sample Size',
                      value = 1,
                      min = 0
                    ),
                    
                    actionButton("plotHyperGeom", "Plot")
                  ),
                  mainPanel(plotOutput("HyperGeom"))
                )
              ),
              
              tabPanel("Activity #2",
                h3("Negative Hypergeometric Distribution"),
                
                # sidebar layout with input and output definitions
                sidebarLayout(
                  sidebarPanel(
                    numericInput(inputId = 'mNegHyper',
                      label = 'M - Pop. Success',
                      value = 1,
                      min = 0
                    ),
                    
                    numericInput(inputId = 'NNegHyper',
                      label = 'N - Pop. Size',
                      value = 1,
                      min = 0
                    ),
                    
                    numericInput(inputId = 'rNegHyper',
                      label = 'r - Num. Success',
                      value = 1,
                      min = 0
                    ),
                    
                    actionButton("plotNegHyperGeom", "Plot")
                  ),
                  mainPanel(plotOutput("NegHyperGeom"))
                )
              ),
              
              tabPanel("Activity #3",
                h3("Poisson Distribution"),
                
                # sidebar layout with input and output definitions
                sidebarLayout(
                  sidebarPanel(
                    numericInput(inputId = 'rate',
                      label = 'Rate',
                      value = 1,
                      min = 0
                    ),
                    
                    actionButton("plotPoisson", "Plot")
                  ),
                  
                  mainPanel(plotOutput("Poisson"))
                )
              )
  )
)


server <- function(input, output) {
  
  plot.hyper<- function(m, n, k){
    miny<- max(0, k-n)
    maxy<- min(k, m)
    y<- c(miny:maxy)
    proby<- dhyper(y, m, n, k)
    Bars<- as.data.frame(cbind(y, proby))
    Dist<- paste("n = ", as.character(k), ",", 
                 " M = ", as.character(m), ",",
                 " N = ", as.character(m+n),
                 sep = "")
    ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = "blue", 
               colour = "black")+
      labs(x = "y",
           y = "p(y)",
           title = "Hypergeometric Distribution",
           subtitle = Dist)+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  }
  
  
  plot.nhyper<- function(m, n, k){
    y<- c(0:(qnhyper(0.9999, n, m, k)))
    proby<- dnhyper(y+k, n, m, k)
    Bars<- as.data.frame(cbind(y, proby))
    Dist<- paste("r = ", as.character(k), ",",
                 " M = ", as.character(m), ",",
                 " N = ", as.character(m+n),
                 sep = "")
    plot <- ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = "blue", 
               colour = "black")+
      labs(x = "y",
           y = "p(y)",
           title = "Negative Hypergeometric Distribution",
           subtitle = Dist)+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
    
    return(plot)
  }
  
  plot.pois<- function(lambda){
    y<- c(0:qpois(0.9999, lambda))
    proby<- dpois(y, lambda)
    Bars<- as.data.frame(cbind(y, proby))
    Dist<- paste("lambda =", as.character(lambda))
    plot <- ggplot(Bars, aes(x = y, y = proby))+ 
      geom_bar(stat="identity", width = 1, fill = "blue", 
               colour = "black")+
      labs(x = "y",
           y = "p(y)",
           title = "Poisson Distribution",
           subtitle = Dist)+
      theme_bw()+
      theme(axis.title.y = element_text(size = rel(1.4)),
            axis.title.x = element_text(size = rel(1.4)),
            axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            plot.title = element_text(hjust=0.5, size = rel(1.75)),
            plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
    return(plot)
  }
  
  observeEvent(
    input$plotHyperGeom,
    output$HyperGeom <- renderPlot(plot.hyper(input$mHyper, 
                                              input$NHyper - input$mHyper,
                                              input$drawHyper))
  )
  
  observeEvent(
    input$plotNegHyperGeom,
    output$NegHyperGeom <- renderPlot(plot.nhyper(input$mNegHyper, 
                                                 input$NNegHyper - input$mNegHyper,
                                                 input$rNegHyper))
  )
  
  observeEvent(
    input$plotPoisson,
    output$Poisson <- renderPlot(plot.pois(input$rate))
  )
}

shinyApp(ui = ui, server = server)