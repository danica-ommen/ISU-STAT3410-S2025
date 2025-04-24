source("plot_generators.r")
library(shiny)
library(extraDistr)

server <- function(input, output) {
  
  getFirstSample <- reactive({
    set.seed(input$seed)
    sample(c(0:9), input$size, replace = TRUE)
  })
  
  getSecondSample <- reactive({
    set.seed(input$seed)
    rmultinom(input$trialsNum, input$size, rep(1/10, 10))
  })
  
  observeEvent(
    input$roll, 
    {
      output$table1 <- renderTable({
        outcomes <- getFirstSample()
        table <- as.data.frame(rbind(c(1:min(30, length(outcomes))), head(outcomes, 30)))
        rownames(table) <- c("Roll", "Value")
        table            
      }, rownames = TRUE, colnames = FALSE 
      )
      
      output$plot1 <- renderPlot({
        outcomes <- getFirstSample()
        barplot(table(outcomes), main="Histogram of rolled values")
      })
      
      output$print1 <- renderPrint({
        "Trial 1, result of the first 30 rolls:"})
    }
  )
  
  observeEvent(
    input$sim,
    
    {
      output$print2 <- renderPrint(cat("Showing simulation of the first", min(input$trialsNum, 15), "trials"))
      
      output$table2 <- renderTable({
        sample <- as.data.frame(getSecondSample())
        rownames(sample) <- c('Y0', 'Y1', 'Y2', 'Y3', 'Y4', 'Y5', 'Y6', 'Y7', 'Y8', 'Y9')
        sample[1:min(15, input$size)]
        
      }, rownames = TRUE, colnames = FALSE )
      
      output$print4 <- renderPrint(
        {
          sample <- getSecondSample()
          mean <- rowMeans(sample)
          var <- apply(sample, 1, var)
          rbind(mean, var)
        }
      )
      
      output$print5 <- renderPrint(
        {
          sample <- getSecondSample()
          cov(t(sample))
        }
      )
      
    }
  )
  
  observeEvent(
    input$calcPeas,
    {
      output$print6 <- renderPrint({
        cat("R Code:", 
            "\ndmultinom(x = c(",input$pea1, ",",input$pea2,",",input$pea3, ",",input$pea4,
            "), prob=c(9, 3, 3, 1)/16)",
            "\n", dmultinom(c(input$pea1, input$pea2, input$pea3, input$pea4), prob = c(9, 3, 3, 1)/16))
      })
    }
  )
  
  observeEvent(
    input$weights,
    {
      if (input$weights == 'equal') {
        alpha <- rep(1, 10)
      } else if (input$weights == 'sequential') {
        alpha <- c(1:10)
      } else {
        alpha <- 2 ^ c(0:9)
      }
      
      p <- rdirichlet(n = 100, alpha)
      
      tally.100.rolls <- rowSums(apply(p, 1, function(x) {rmultinom(1, 1, x)}))
      
      output$print7 <- renderPrint({
        cat("Tally of 100 rolls:", 
            "\n", tally.100.rolls,
            "\nProbability of this outcome, assuming fair dice:",
            "\n", dmultinom(x=tally.100.rolls, prob=rep(1/10, 10)) )
      })
    }
  )
}