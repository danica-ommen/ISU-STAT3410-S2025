source("plot_generators.r")
source("helpers.r")
library(shiny)
library(EnvStats)
library(fabCI)

server <- function(input, output) {
  observeEvent(
    input$plotUnif,
    output$Unif <- renderPlot(plot.unif(input$lowerUnif,
                                        input$upperUnif))
  )
  
  observeEvent(
    input$plotNormal,
    output$Normal <- renderPlot(plot.norm(input$muNormal,
                                          input$sigmaNormal))
  )
  
  observeEvent(
    input$plotGamma,
    output$Gamma <- renderPlot(plot.gamma(input$shapeGamma,
                                          input$scaleGamma))
  )
  
  observeEvent(
    input$plotExp,
    output$Exp <- renderPlot(plot.exp(input$rateExp))
  )
  
  observeEvent(
    input$plotChisq,
    output$Chisq <- renderPlot(plot.chisq(input$dfChisq))
  )
  
  observeEvent(
    input$plotBeta,
    output$Beta <- renderPlot(plot.beta(input$shapeBeta,
                                        input$scaleBeta))
  )
  
  # activity 2
  data("randu")
  
  output$randuSummary <- renderPrint({columns_summary(randu)})
  
  output$randuHistogram <- renderPlot(
    {
      randuVar <- switch(input$randuVarChoice,
                         randux = randu$x,
                         randuy = randu$y,
                         randuz = randu$z
      )
      
      hist(randuVar, 
           main="Histogram of the selected variable from randu",
           xlab="value")
    }
  )
  
  
  observeEvent(
    input$plotqq,
    output$qqplot <- renderPlot(
      {
        x <- switch(
          input$randuEmp,
          randuempx = randu$x,
          randuempy = randu$y,
          randuempz = randu$z
        )
        
        # setting parameters to go with the distribution
        if (input$theoreticalDist == 'beta') {
          parameters <- list(shape1 = input$qqShapeBeta, shape2 = input$qqScaleBeta)
        } else if (input$theoreticalDist == 'norm') {
          parameters <- list(mean = input$qqMuNormal, sd = input$qqSigmaNormal)
        } else if (input$theoreticalDist == 'chisq') {
          parameters <- list(df = input$qqDfChisq)
        } else if (input$theoreticalDist == 'exp') {
          parameters <- list(rate = input$qqRateExp)
        } else if (input$theoreticalDist == 'gamma') {
          parameters <- list(shape = input$qqShapeGamma, scale = input$qqScaleGamma)
        } else if (input$theoreticalDist == 'unif') {
          parameters <- list(min = input$qqLowerUnif, max = input$qqUpperUnif)
        } else {
          parameters <- NULL
        }
        
        qqPlot(x, distribution = input$theoreticalDist, param.list = parameters,
               add.line = TRUE, 
               qq.line.type = "0-1")
      }
    )
  )
  
  # activity 3
  data(radon)
  radon <- as.data.frame(radon)
  
  output$radonSummary <- renderPrint({columns_summary(radon)})
  
  output$radonHistogram <- renderPlot(
    {
      hist(radon$radon, 
           main="Histogram of radon",
           xlab="value")
    }
  )
  
  observeEvent(
    input$plotqq3,
    output$qqplot3 <- renderPlot(
      {
        x <- radon$radon
        
        # setting parameters to go with the distribution
        if (input$theoreticalDist3 == 'beta') {
          parameters <- list(shape1 = input$qqShapeBeta3, shape2 = input$qqScaleBeta3)
        } else if (input$theoreticalDist3 == 'norm') {
          parameters <- list(mean = input$qqMuNormal3, sd = input$qqSigmaNormal3)
        } else if (input$theoreticalDist3 == 'chisq') {
          parameters <- list(df = input$qqDfChisq3)
        } else if (input$theoreticalDist3 == 'exp') {
          parameters <- list(rate = input$qqRateExp3)
        } else if (input$theoreticalDist3 == 'gamma') {
          parameters <- list(shape = input$qqShapeGamma3, scale = input$qqScaleGamma3)
        } else if (input$theoreticalDist3 == 'unif') {
          parameters <- list(min = input$qqLowerUnif3, max = input$qqUpperUnif3)
        } else {
          parameters <- NULL
        }
        
        qqPlot(x, distribution = input$theoreticalDist3, param.list = parameters,
               add.line = TRUE, 
               qq.line.type = "0-1")
      }
    )
  )
  
  # activity 4
  refractID <- read.csv("RI_database.txt", sep = "\t")
  refractID <- refractID["RI"]
  
  output$refSummary <- renderPrint({columns_summary(refractID)})
  
  output$refHistogram <- renderPlot(
    {
      hist(refractID$RI, 
           main="Histogram of Refractive Index",
           xlab="value")
    }
  )
  
  observeEvent(
    input$plotqq4,
    output$qqplot4 <- renderPlot(
      {
        
        # setting parameters to go with the distribution
        if (input$theoreticalDist4 == 'beta') {
          parameters <- list(shape1 = input$qqShapeBeta4, shape2 = input$qqScaleBeta4)
        } else if (input$theoreticalDist4 == 'norm') {
          parameters <- list(mean = input$qqMuNormal4, sd = input$qqSigmaNormal4)
        } else if (input$theoreticalDist4 == 'chisq') {
          parameters <- list(df = input$qqDfChisq4)
        } else if (input$theoreticalDist4 == 'exp') {
          parameters <- list(rate = input$qqRateExp4)
        } else if (input$theoreticalDist4 == 'gamma') {
          parameters <- list(shape = input$qqShapeGamma4, scale = input$qqScaleGamma4)
        } else if (input$theoreticalDist4 == 'unif') {
          parameters <- list(min = input$qqLowerUnif4, max = input$qqUpperUnif4)
        } else {
          parameters <- NULL
        }
        
        qqPlot(refractID$RI, distribution = input$theoreticalDist4, param.list = parameters,
               add.line = TRUE, 
               qq.line.type = "0-1")
      }
    )
  )
  
  
}