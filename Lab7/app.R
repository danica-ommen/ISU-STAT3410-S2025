library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("STAT 3410: Lab #7"),
  
  # Output: Tabset for different Activities ----
  tabsetPanel(type = "tabs",
              tabPanel("Activity #1", 
                       h3("X: RV with Single Interval Support"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'nX',
                                        label = "Sample Size (n)",
                                        value = 10000,
                                        min = 10
                           )
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plotX"),
                           # Output: Display the Mean & Variance ----
                           tableOutput("tabX"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summaryX")
                         )
                       )),
              tabPanel("Activity #2", 
                       h3("W: RV with Union of Intervals Support"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'nW',
                                        label = "Sample Size (n)",
                                        value = 10000,
                                        min = 10
                           )
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plotW"),
                           # Output: Display the Mean & Variance ----
                           tableOutput("tabW"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summaryW")
                         )
                       )),
              tabPanel("Activity #3", 
                       h3("Y: RV with Pareto Distribution"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'nY',
                                        label = "Sample Size (n)",
                                        value = 10000,
                                        min = 10
                           ),
                           numericInput(inputId = 'lambda',
                                        label = "Scale Parameter (lambda)",
                                        value = 1,
                                        min = 0
                           ),
                           numericInput(inputId = 'k',
                                        label = "Shape Parameter (k)",
                                        value = 1,
                                        min = 0
                           )
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plotY"),
                           # Output: Display the Mean & Variance ----
                           tableOutput("tabY"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summaryY")
                         )
                       )),
              tabPanel("Activity #4", 
                       h3("Z: RV with Rayleigh Distribution"),
                       
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           numericInput(inputId = 'nZ',
                                        label = "Sample Size (n)",
                                        value = 10000,
                                        min = 10
                           ),
                           numericInput(inputId = 'sig',
                                        label = "Scale Parameter (sigma)",
                                        value = 1,
                                        min = 0
                           )
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput("plotZ"),
                           # Output: Display the Mean & Variance ----
                           tableOutput("tabZ"),
                           # Output: Display the R Code ----
                           verbatimTextOutput("summaryZ")
                         )
                       ))
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  dX <- reactive({
    p <- runif(input$nX,0,1)
    XinvCDF <- function(p){ 2*(1-p)^(-1/3) }
    x <- XinvCDF(p)
    return(x)
  })
  
  # Display histogram for X
  output$plotX <- renderPlot({
    hist(dX(), freq=F, breaks=25, xlab="x", main="Histogram of X samples")
  }) 
  
  # Display mean & variance for X ----
  output$tabX <- renderTable({
    data.frame(Moment=c("Mean", "Variance"),
               Theoretical=c(3, 3), 
               Empirical=c(mean(dX()), var(dX())))
    }, digits=4
    )
  
  # Display R code for X ----
  output$summaryX <- renderPrint({
    cat("R Code:", 
        "\n n =", input$nX,
        "\n p <- runif(n,0,1)",
        "\n invCDFx <- function(p){ 2*(1-p)^(-1/3) }",
        "\n x <- invCDFx(p)",
        "\n hist(x, freq=F, breaks=25)",
        "\n mean(x)",
        "\n var(x)")
  })
  
  dW <- reactive({
    p <- runif(input$nW,0,1)
    WinvCDF <- function(p){
      W = vector("numeric", length(p))
      for(i in 1:length(p)){
        if(p[i] < 0.25){
          W[i] = 8*p[i]
        }else{
          W[i] = 4*sqrt(p[i])
        } 
      }
      return(W)
    }
    w <- WinvCDF(p)
    return(w)
  })
  
  # Display histogram for W
  output$plotW <- renderPlot({
    hist(dW(), freq=F, breaks=25, xlab="w", main="Histogram of W samples")
  }) 
  
  # Display mean & variance for W ----
  output$tabW <- renderTable({
    data.frame(Moment=c("Mean", "Variance"),
               Theoretical=c(31/12, 167/144), 
               Empirical=c(mean(dW()), var(dW())))
  }, digits=4
  )
  
  # Display R code for W ----
  output$summaryW <- renderPrint({
    cat("R Code:", 
        "\n n =", input$nW,
        "\n p <- runif(n,0,1)",
        "\n invCDFw <- function(p){",
        "\n   W = vector(\"numeric\", length(p))",
        "\n   for(i in 1:length(p)){",
        "\n     if(p[i] < 0.25){",
        "\n       W[i] = 8*p[i]",
        "\n     }else{",
        "\n       W[i] = 4*sqrt(p[i])",
        "\n     } ",
        "\n   }",
        "\n   return(W)",
        "\n }",
        "\n w <- invCDFw(p)",
        "\n hist(w, freq=F, breaks=25)",
        "\n mean(w)",
        "\n var(w)")
  })
  
  dY <- reactive({
    p <- runif(input$nY,0,1)
    YinvCDF <- function(p, L, K){ L*(1-p)^(-1/K) }
    y <- YinvCDF(p, input$lambda, input$k)
    return(y)
  })
  
  # Display histogram for Y
  output$plotY <- renderPlot({
    hist(dY(), freq=F, breaks=25, xlab="y", main="Histogram of Y samples")
  }) 
  
  # Display mean & variance for Y ----
  output$tabY <- renderTable({
    data.frame(Moment=c("Mean", "Variance"),
               Theoretical=c(input$k*input$lambda/(input$k-1), 
                             input$k*(input$lambda^2)/(((input$k-1)^2)*(input$k-2))), 
               Empirical=c(mean(dY()), var(dY())))
  }, digits=4
  )
  
  # Display R code for Y ----
  output$summaryY <- renderPrint({
    cat("R Code:",
        "\n n =", input$nY,
        "\n p <- runif(n,0,1)",
        "\n L =", input$lambda,
        "\n K =", input$k,
        "\n invCDFy <- function(p, lambda, k){ lambda*(1-p)^(-1/k) }",
        "\n y <- invCDFy(p, lambda=L, k=K)",
        "\n hist(y, freq=F, breaks=25)",
        "\n mean(y)",
        "\n var(y)")
  })
  
  dZ <- reactive({
    p <- runif(input$nZ,0,1)
    ZinvCDF <- function(p, sigma){ sqrt(-log(1-p)*2*(sigma^2)) }
    z <- ZinvCDF(p, input$sig)
    return(z)
  })
  
  # Display histogram for Z
  output$plotZ <- renderPlot({
    hist(dZ(), freq=F, breaks=25, xlab="z", main="Histogram of Z samples")
  }) 
  
  # Display mean & variance for Z ----
  output$tabZ <- renderTable({
    data.frame(Moment=c("Mean", "Variance"),
               Theoretical=c(input$sig*sqrt(pi/2), input$sig^2*(4-pi)/2), 
               Empirical=c(mean(dZ()), var(dZ())))
  }, digits=4
  )
  
  # Display R code for Z ----
  output$summaryZ <- renderPrint({
    cat("R Code:",
        "\n n =", input$nZ,
        "\n p <- runif(n,0,1)",
        "\n sig =", input$sig,
        "\n invCDFz <- function(p, sigma){ sqrt(-log(1-p)*2*(sigma^2)) }",
        "\n z <- invCDFz(p, sig)",
        "\n hist(z, freq=F, breaks=25)",
        "\n mean(z)",
        "\n var(z)")
  })
  
}

shinyApp(ui = ui, server = server)