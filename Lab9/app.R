library(shiny)
#runApp("~/Box Sync/STAT-341-DMO/Fall2022/Labs/Lab8/Lab8_shiny")
#runApp("~/Library/CloudStorage/Box-Box/STAT-341-DMO/Fall2022/Labs/Lab8/Lab8_shiny")

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #9"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("Activity #1", 
                       h3("Transformations with a General Continuous Distribution"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           actionButton("genR", "Generate R"),
                           br(),br(),
                           actionButton("genS", "Transform R to S"),
                           br(),br(),
                           actionButton("genT", "Transform R to T")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plot1"),
                           # Output: Display the Mean & Std. Dev. ----
                           tableOutput("tab1"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summary1")
                         )
                       )),
              tabPanel("Activity #2", 
                       h3("Transformations with the Normal Distribution"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'mean',
                                        label = "Location (mu)",
                                        value = 0),
                           numericInput(inputId = 'sd',
                                        label = "Scale (sigma)",
                                        value = 1),
                           actionButton("genX", "Generate X"),
                           br(),br(),
                           actionButton("genY", "Transform X to Y"),
                           br(),br(),
                           actionButton("genW", "Transform Z to W"),
                           br(),br(),
                           hr(style = "border-top: 1px solid #000000;"),
                           h4("Check Fit Panel"),
                           selectInput(
                             inputId = "theoreticalDist",
                             label = "Theoretical Distribution",
                             choices = c("Uniform" = "unif",
                                         "Normal" = "norm",
                                         "Gamma" = "gamma",
                                         "Exponential" = "exp",
                                         "Chi Square" = "chisq",
                                         "Beta" = "beta")
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'beta'",
                             numericInput(
                               inputId = 'qqShapeBeta',
                               label = 'Shape 1 (alpha)',
                               value = 1,
                               min = 0),
                             numericInput(
                               inputId = 'qqScaleBeta',
                               label = 'Shape 2 (beta)',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'norm'",
                             numericInput(
                               inputId = 'qqMuNormal',
                               label = 'Mean',
                               value = 0),
                             numericInput(
                               inputId = 'qqSigmaNormal',
                               label = 'Sigma',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'chisq'",
                             numericInput(
                               inputId = 'qqDfChisq',
                               label = 'Degrees of freedom',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'exp'",
                             numericInput(
                               inputId = 'qqRateExp',
                               label = 'Rate',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'gamma'",
                             numericInput(
                               inputId = 'qqShapeGamma',
                               label = 'Shape (alpha)',
                               value = 1,
                               min = 0),
                             
                             numericInput(
                               inputId = 'qqScaleGamma',
                               label = 'Scale (beta)',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist == 'unif'",
                             numericInput(
                               inputId = 'qqLowerUnif',
                               label = 'Lower Bound (A)',
                               value = 0),
                             
                             numericInput(
                               inputId = 'qqUpperUnif',
                               label = 'Upper Bound (B)',
                               value = 1)
                           ),
                           
                           actionButton("checkY", "Check Fit to Y"),
                           br(),br(),
                           actionButton("checkW", "Check Fit to W")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plot2"),
                           # Output: Display the Mean & Std. Dev. ----
                           tableOutput("tab2"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summary2")
                         )
                       )),
              tabPanel("Activity #3", 
                       h3("Transformation with the Gamma Distribution"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'alpha',
                                        label = "Shape (alpha)",
                                        value = 1),
                           numericInput(inputId = 'beta',
                                        label = "Scale (beta)",
                                        value = 1),
                           actionButton("genU", "Generate U"),
                           br(),br(),
                           actionButton("genV", "Transform U to V"),
                           br(),br(),
                           hr(style = "border-top: 1px solid #000000;"),
                           h4("Check Fit Panel"),
                           selectInput(
                             inputId = "theoreticalDist2",
                             label = "Theoretical Distribution",
                             choices = c("Uniform" = "unif",
                                         "Normal" = "norm",
                                         "Gamma" = "gamma",
                                         "Exponential" = "exp",
                                         "Chi Square" = "chisq",
                                         "Beta" = "beta")
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'beta'",
                             numericInput(
                               inputId = 'qqShapeBeta2',
                               label = 'Shape 1 (alpha)',
                               value = 1,
                               min = 0),
                             numericInput(
                               inputId = 'qqScaleBeta2',
                               label = 'Shape 2 (beta)',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'norm'",
                             numericInput(
                               inputId = 'qqMuNormal2',
                               label = 'Mean',
                               value = 0),
                             numericInput(
                               inputId = 'qqSigmaNormal2',
                               label = 'Sigma',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'chisq'",
                             numericInput(
                               inputId = 'qqDfChisq2',
                               label = 'Degrees of freedom',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'exp'",
                             numericInput(
                               inputId = 'qqRateExp2',
                               label = 'Rate',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'gamma'",
                             numericInput(
                               inputId = 'qqShapeGamma2',
                               label = 'Shape (alpha)',
                               value = 1,
                               min = 0),
                             
                             numericInput(
                               inputId = 'qqScaleGamma2',
                               label = 'Scale (beta)',
                               value = 1,
                               min = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.theoreticalDist2 == 'unif'",
                             numericInput(
                               inputId = 'qqLowerUnif2',
                               label = 'Lower Bound (A)',
                               value = 0),
                             
                             numericInput(
                               inputId = 'qqUpperUnif2',
                               label = 'Upper Bound (B)',
                               value = 1)
                           ),
                           actionButton("checkV", "Check Fit to V")
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plot3"),
                           # Output: Display the Mean & Std. Dev. ----
                           tableOutput("tab3"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summary3")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # local variable definitions
  n=100000
  c1 <- rgb(173,216,230, max=255, alpha=80)
  c2 <- rgb(255,192,203, max=255, alpha=80)
  
  ##################### ACTIVITY 1 #####################
  
  # Get the data for R
  dR <- reactive({
    p <- runif(n, 0, 1)
    r <- 2*p^(1/3)
    return(r)
  })
  
  # "Generate" Button for Activity 1
  observeEvent(input$genR, {
    
    # Display histogram for R
    output$plot1 <- renderPlot({
      hgR <- hist(dR())
      plot(hgR, col=c1, xlim=c(0,2), freq=F, main="R", xlab="value")
    }) 
    
    # Display mean & variance for R
    output$tab1 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 R=c(mean(dR()), sd(dR())))
    }, digits=4
    )
    
    # Display R code for R
    output$summary1 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n p <- runif(n, 0, 1)",
          "\n r <- 2*p^(1/3)",
          "\n hgR <- hist(r)",
          "\n plot(hgR, col=c1, xlim=c(0,2), freq=F, main=\"R\", xlab=\"value\")",
          "\n mean(r)",
          "\n sd(r)", sep="")
    })
  })
  
  
  # "Transform R to S" Button for Activity 1
  observeEvent(input$genS, {
    
    # Get the data for S
    dS <- reactive({
      s <- 3*dR()
      return(s)
    })
    
    # Display histogram for S
    output$plot1 <- renderPlot({
      hgR <- hist(dR())
      hgS <- hist(dS())
      plot(hgR, col=c1, xlim=c(0,6), freq=F, main="S=3R", xlab="value")
      plot(hgS, col=c2, add=TRUE, freq=F) 
      legend("topright", legend=c("R", "S"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for S
    output$tab1 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 R=c(mean(dR()), sd(dR())),
                 S=c(mean(dS()), sd(dS())))
    }, digits=4
    )
    
    # Display R code for S
    output$summary1 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n p <- runif(n, 0, 1)",
          "\n r <- 2*p^(1/3)",
          "\n s <- 3*r",
          "\n hgR <- hist(r)",
          "\n hgS <- hist(s)",
          "\n plot(hgR, col=c1, xlim=c(0,6), freq=F, main=\"S=3R\", xlab=\"value\")",
          "\n plot(hgS, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"R\", \"S\"), fill=c(c1,c2))",
          "\n mean(s)",
          "\n sd(s)", sep="")
    })
  })
  
  
  # "Transform R to T" Button for Activity 1
  observeEvent(input$genT, {
    
    # Get the data for T
    dT <- reactive({
      t <- dR()^2
      return(t)
    })
    
    # Display histogram for T
    output$plot1 <- renderPlot({
      hgR <- hist(dR())
      hgT <- hist(dT())
      plot(hgR, col=c1, xlim=c(0,4), freq=F, main="T=R^2", xlab="value")
      plot(hgT, col=c2, add=TRUE, freq=F) 
      legend("topright", legend=c("R", "T"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for T
    output$tab1 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 R=c(mean(dR()), sd(dR())),
                 T=c(mean(dT()), sd(dT())))
    }, digits=4
    )
    
    # Display R code for T
    output$summary1 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n p <- runif(n, 0, 1)",
          "\n r <- 2*p^(1/3)",
          "\n t <- r^2",
          "\n hgR <- hist(r)",
          "\n hgT <- hist(t)",
          "\n plot(hgR, col=c1, xlim=c(0,4), freq=F, main=\"T=R^2\", xlab=\"value\")",
          "\n plot(hgT, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"R\", \"T\"), fill=c(c1,c2))",
          "\n mean(t)",
          "\n sd(t)", sep="")
    })
  })
  
  
  ##################### ACTIVITY 2 #####################
  
  # Get the data for X
  dX <- reactive({
    x <- rnorm(n, input$mean, input$sd)
    return(x)
  })
  
  # "Generate" Button for Activity 2
  observeEvent(input$genX, {
    
    # Display histogram for X
    output$plot2 <- renderPlot({
      hgX <- hist(dX())
      plot(hgX, col=c1, freq=F, main="X", xlab="value")
    }) 
    
    # Display mean & variance for X
    output$tab2 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 X=c(mean(dX()), sd(dX())))
    }, digits=4
    )
    
    # Display R code for X
    output$summary2 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n x <- rnorm(n, ", input$mean, ", ", input$sd, ")",
          "\n hgX <- hist(x)",
          "\n plot(hgX, col=c1, freq=F, main=\"X\", xlab=\"value\")",
          "\n mean(x)",
          "\n sd(x)", sep="")
    })
  })
  
  # Get the data for Y
  dY <- reactive({
    y <- (dX() - input$mean)/input$sd
    return(y)
  })
  
  # "Transform X to Y" Button for Activity 2
  observeEvent(input$genY, {
    
    # Display histogram for Y
    output$plot2 <- renderPlot({
      hgX <- hist(dX())
      hgY <- hist(dY())
      plot(hgX, col=c1, xlim=c(min(dX(), dY()), max(dX(), dY())), ylim=c(0, max(hgX$density, hgY$density)),
           freq=F, main="Y=(X-mu)/sigma", xlab="value") 
      plot(hgY, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("X", "Y"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for Y
    output$tab2 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 X=c(mean(dX()), sd(dX())),
                 Y=c(mean(dY()), sd(dY())))
    }, digits=4
    )
    
    # Display R code for Y
    output$summary2 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n x <- rnorm(n, ", input$mean, ", ", input$sd, ")",
          "\n y <- (x - ", input$mean, ")/", input$sd,
          "\n hgX <- hist(x)",
          "\n hgY <- hist(y)",
          "\n plot(hgX, col=c1, xlim=c(min(x, y), max(x, y)),",
          "\n      ylim=c(0, max(hgX$density, hgY$density)),",
          "\n      freq=F, main=\"Y=(X-mu)/sigma\", xlab=\"value\")",
          "\n plot(hgY, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"X\", \"Y\"), fill=c(c1,c2))",
          "\n mean(y)",
          "\n sd(y)", sep="")
    })
  })
  
  
  # Get the data for Z
  dZ <- reactive({
    z <- rnorm(n, 0, 1)
    return(z)
  })
  
  # Get the data for W
  dW <- reactive({
    w <- dZ()^2
    return(w)
  })
  
  # "Transform Z to W" Button for Activity 2
  observeEvent(input$genW, {
    
    # Display histogram for Y
    output$plot2 <- renderPlot({
      hgZ <- hist(dZ())
      hgW <- hist(dW())
      plot(hgZ, col=c1, xlim=c(min(dZ(), dW()), max(dZ(), dW())), ylim=c(0, max(hgZ$density, hgW$density)), 
           freq=F, main="W = Z^2", xlab="value") 
      plot(hgW, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("Z", "W"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for Z & W
    output$tab2 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 Z=c(mean(dZ()), sd(dW())),
                 W=c(mean(dW()), sd(dW())))
    }, digits=4
    )
    
    # Display R code for Z & W
    output$summary2 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n z <- rnorm(n, 0, 1)",
          "\n w <- z^2",
          "\n hgZ <- hist(z)",
          "\n hgW <- hist(w)",
          "\n plot(hgZ, col=c1, xlim=c(min(z, w), max(z, w)),",
          "\n      ylim=c(0, max(hgZ$density, hgW$density)),",
          "\n      freq=F, main=\"W = Z^2\", xlab=\"value\")",
          "\n plot(hgW, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"Z\", \"W\"), fill=c(c1,c2))",
          "\n c(mean(z), mean(w))",
          "\n c(sd(z), sd(w)", sep="")
    })
  })
  
  
  # Get the data for comparing against Y or W
  dTh <- reactive({
    if (input$theoreticalDist == 'beta') {
      dat <- rbeta(n, input$qqShapeBeta, input$qqScaleBeta)
      distr <- 'beta'
      params <- c(input$qqShapeBeta, input$qqScaleBeta)
    } else if (input$theoreticalDist == 'norm') {
      dat <- rnorm(n, input$qqMuNormal, input$qqSigmaNormal)
      distr <- 'norm'
      params <- c(input$qqMuNormal, input$qqSigmaNormal)
    } else if (input$theoreticalDist == 'chisq') {
      dat <- rchisq(n, input$qqDfChisq)
      distr <- 'chisq'
      params <- c(input$qqDfChisq)
    } else if (input$theoreticalDist == 'exp') {
      dat <- rexp(n, input$qqRateExp)
      distr <- 'exp'
      params <- c(input$qqRateExp)
    } else if (input$theoreticalDist == 'gamma') {
      dat <- rgamma(n, shape = input$qqShapeGamma, scale = input$qqScaleGamma)
      distr <- 'gamma'
      params <- c(input$qqShapeGamma, input$qqScaleGamma)
    } else if (input$theoreticalDist == 'unif') {
      dat <- runif(n, input$qqLowerUnif, input$qqUpperUnif)
      distr <- 'unif'
      params <- c(input$qqLowerUnif, input$qqUpperUnif)
    } else {
      dat <- NULL
      distr <- 'NULL'
      params <- 'NULL'
    }
    return(list(dat=dat, distr=distr, params=params))
  })
  
  
  # "Check Fit Y" Button for Activity 2
  observeEvent(input$checkY, {
    
    # Display histogram for Check Fit Y
    output$plot2 <- renderPlot({
      hgX <- hist(dTh()$dat)
      hgY <- hist(dY())
      plot(hgX, col=c1, xlim=c(min(dTh()$dat, dY()), max(dTh()$dat, dY())), 
           ylim=c(0, max(hgX$density, hgY$density)),
           freq=F, main="Check Fit Y", xlab="value") 
      plot(hgY, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("Theor", "Y"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for Y
    output$tab2 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 Theoretical=c(mean(dTh()$dat), sd(dTh()$dat)),
                 Y=c(mean(dY()), sd(dY())))
    }, digits=4
    )
    
    # Display R code for Y
    output$summary2 <- renderPrint({
      if(dTh()$distr == 'gamma'){
        print_params = paste("shape=", dTh()$params[1], ", scale=", dTh()$params[2], sep="")
      }else{
        print_params = paste(dTh()$params, collapse=", ")
      }
      
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n x <- rnorm(n, ", input$mean, ", ", input$sd, ")",
          "\n y <- (x - ", input$mean, ")/", input$sd,
          "\n Theor <- r", dTh()$distr, "(n, ", print_params, ")",
          "\n hgY <- hist(y)",
          "\n hgT <- hist(Theor)",
          "\n plot(hgT, col=c1, xlim=c(min(Theor, y), max(Theor, y)),",
          "\n      ylim=c(0, max(hgT$density, hgY$density)),",
          "\n      freq=F, main=\"Check Fit Y\", xlab=\"value\")",
          "\n plot(hgY, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"Theor\", \"Y\"), fill=c(c1,c2))",
          "\n mean(Theor)",
          "\n sd(Theor)", sep="")
    })
  })
  
  
  # "Check Fit W" Button for Activity 2
  observeEvent(input$checkW, {
    
    # Display histogram for Check Fit Y
    output$plot2 <- renderPlot({
      hgX <- hist(dTh()$dat)
      hgW <- hist(dW())
      plot(hgX, col=c1, xlim=c(min(dTh()$dat, dW()), max(dTh()$dat, dW())), 
           ylim=c(0, max(hgX$density, hgW$density)),
           freq=F, main="Check Fit W", xlab="value") 
      plot(hgW, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("Theor", "W"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for W
    output$tab2 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 Theoretical=c(mean(dTh()$dat), sd(dTh()$dat)),
                 W=c(mean(dW()), sd(dW())))
    }, digits=4
    )
    
    # Display R code for Check Fit W
    output$summary2 <- renderPrint({
      if(dTh()$distr == 'gamma'){
        print_params = paste("shape=", dTh()$params[1], ", scale=", dTh()$params[2], sep="")
      }else{
        print_params = paste(dTh()$params, collapse=", ")
      }
      
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n z <- rnorm(n, 0, 1)",
          "\n w <- z^2",
          "\n Theor <- r", dTh()$distr, "(n, ", print_params, ")",
          "\n hgW <- hist(w)",
          "\n hgT <- hist(Theor)",
          "\n plot(hgT, col=c1, xlim=c(min(Theor, w), max(Theor, w)),",
          "\n      ylim=c(0, max(hgT$density, hgW$density)),",
          "\n      freq=F, main=\"Check Fit W\", xlab=\"value\")",
          "\n plot(hgW, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"Theor\", \"W\"), fill=c(c1,c2))",
          "\n mean(Theor)",
          "\n sd(Theor)", sep="")
    })
    
  })
  
  
  
  ##################### ACTIVITY 3 #####################
  
  # Get the data for U
  dU <- reactive({
    u <- rgamma(n, shape=input$alpha, scale=input$beta)
    return(u)
  })
  
  # "Generate" Button for Activity 3
  observeEvent(input$genU, {
    
    # Display histogram for U
    output$plot3 <- renderPlot({
      hgU <- hist(dU())
      plot(hgU, col=c1, freq=F, main="U", xlab="value")
    }) 
    
    # Display mean & variance for U
    output$tab3 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 U=c(mean(dU()), sd(dU())))
    }, digits=4
    )
    
    # Display R code for U
    output$summary3 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n u <- rgamma(n, shape=", input$alpha, ", scale=", input$beta, ")",
          "\n hgU <- hist(u)",
          "\n plot(hgU, col=c1, freq=F, main=\"U\", xlab=\"value\")",
          "\n mean(u)",
          "\n sd(u)", sep="")
    })
  })
  
  
  # Get the data for V
  dV <- reactive({
    v <- dU()/10
    return(v)
  })
  
  # "Transform U to V" Button for Activity 3
  observeEvent(input$genV, {
    
    # Display histogram for V
    output$plot3 <- renderPlot({
      hgU <- hist(dU())
      hgV <- hist(dV())
      plot(hgU, col=c1, xlim=c(min(dU(), dV()), max(dU(), dV())), ylim=c(0, max(hgU$density, hgV$density)),
           freq=F, main="V = U/10", xlab="value") 
      plot(hgV, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("U", "V"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for V
    output$tab3 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 U=c(mean(dU()), sd(dU())),
                 V=c(mean(dV()), sd(dV())))
    }, digits=4
    )
    
    # Display R code for V
    output$summary3 <- renderPrint({
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n u <- rgamma(n, shape=", input$alpha, ", scale=", input$beta, ")",
          "\n v <- u/10",
          "\n hgU <- hist(U)",
          "\n hgV <- hist(V)",
          "\n plot(hgU, col=c1, xlim=c(min(u, v), max(u, v)),",
          "\n      ylim=c(0, max(hgU$density, hgV$density)),",
          "\n      freq=F, main=\"V = U/10\", xlab=\"value\")",
          "\n plot(hgV, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"U\", \"V\"), fill=c(c1,c2))",
          "\n mean(v)",
          "\n sd(v)", sep="")
    })
  })
  
  
  # Get the data for comparing against V
  vTh <- reactive({
    if (input$theoreticalDist2 == 'beta') {
      dat <- rbeta(n, input$qqShapeBeta2, input$qqScaleBeta2)
      distr <- 'beta'
      params <- c(input$qqShapeBeta2, input$qqScaleBeta2)
    } else if (input$theoreticalDist2 == 'norm') {
      dat <- rnorm(n, input$qqMuNormal2, input$qqSigmaNormal2)
      distr <- 'norm'
      params <- c(input$qqMuNormal2, input$qqSigmaNormal2)
    } else if (input$theoreticalDist2 == 'chisq') {
      dat <- rchisq(n, input$qqDfChisq2)
      distr <- 'chisq'
      params <- c(input$qqDfChisq2)
    } else if (input$theoreticalDist2 == 'exp') {
      dat <- rexp(n, input$qqRateExp2)
      distr <- 'exp'
      params <- c(input$qqRateExp2)
    } else if (input$theoreticalDist2 == 'gamma') {
      dat <- rgamma(n, shape = input$qqShapeGamma2, scale = input$qqScaleGamma2)
      distr <- 'gamma'
      params <- c(input$qqShapeGamma2, input$qqScaleGamma2)
    } else if (input$theoreticalDist2 == 'unif') {
      dat <- runif(n, input$qqLowerUnif2, input$qqUpperUnif2)
      distr <- 'unif'
      params <- c(input$qqLowerUnif2, input$qqUpperUnif2)
    } else {
      dat <- NULL
      distr <- 'NULL'
      params <- 'NULL'
    }
    return(list(dat=dat, distr=distr, params=params))
  })

  # "Check Fit V" Button for Activity 2
  observeEvent(input$checkV, {
    
    # Display histogram for Check Fit V
    output$plot3 <- renderPlot({
      hgX <- hist(vTh()$dat)
      hgV <- hist(dV())
      plot(hgX, col=c1, xlim=c(min(vTh()$dat, dV()), max(vTh()$dat, dV())), 
           ylim=c(0, max(hgX$density, hgV$density)),
           freq=F, main="Check Fit V", xlab="value") 
      plot(hgV, col=c2, add=TRUE, freq=F)
      legend("topright", legend=c("Theor", "V"), fill=c(c1,c2))
    }) 
    
    # Display mean & variance for W
    output$tab3 <- renderTable({
      data.frame(Property=c("Mean", "Std.Dev."),
                 Theoretical=c(mean(vTh()$dat), sd(vTh()$dat)),
                 V=c(mean(dV()), sd(dV())))
    }, digits=4
    )
    
    # Display R code for Check Fit V
    output$summary3 <- renderPrint({
      if(vTh()$distr == 'gamma'){
        print_params = paste("shape=", vTh()$params[1], ", scale=", vTh()$params[2], sep="")
      }else{
        print_params = paste(vTh()$params, sep=", ")
      }
      
      cat("R Code:", 
          "\n n = 100000",
          "\n c1 <- rgb(173,216,230, max=255, alpha=80)",
          "\n c2 <- rgb(255,192,203, max=255, alpha=80)",
          "\n u <- rgamma(n, shape=", input$alpha, ", scale=", input$beta, ")",
          "\n v <- u/10",
          "\n Theor <- r", vTh()$distr, "(n, ", print_params, ")",
          "\n hgV <- hist(v)",
          "\n hgT <- hist(Theor)",
          "\n plot(hgT, col=c1, xlim=c(min(Theor, v), max(Theor, v)),",
          "\n      ylim=c(0, max(hgT$density, hgV$density)),",
          "\n      freq=F, main=\"Check Fit V\", xlab=\"value\")",
          "\n plot(hgV, col=c2, add=TRUE, freq=F)",
          "\n legend(\"topright\", legend=c(\"Theor\", \"V\"), fill=c(c1,c2))",
          "\n mean(Theor)",
          "\n sd(Theor)", sep="")
    })
    
  })
  
  
}

shinyApp(ui = ui, server = server)