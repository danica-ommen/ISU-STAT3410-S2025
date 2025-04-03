library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  titlePanel("STAT 3410: Lab #8"),
  
  tabsetPanel(type="tabs",
              
              tabPanel(
                h3("Activity 1"),
                tabsetPanel(
                  tabPanel(
                    h4("Uniform Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        # input
                        numericInput(
                          inputId = 'lowerUnif',
                          label = "Lower Bound - A",
                          value = 0
                        ),
                        
                        numericInput(
                          inputId = 'upperUnif',
                          label = 'Upper Bound - B',
                          value = 1
                        ),
                        
                        actionButton("plotUnif", "Plot")
                      ),
                      mainPanel(plotOutput("Unif"))
                    )
                  ),
                  
                  tabPanel(
                    h4("Normal Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = 'muNormal',
                          label = 'Mean - mu',
                          value = 0
                        ),
                        
                        numericInput(
                          inputId = 'sigmaNormal',
                          label = 'Standard Deviation - sigma',
                          value = 1,
                          min = 0
                        ),
                        
                        actionButton("plotNormal", "Plot")
                      ),
                      mainPanel(plotOutput("Normal"))
                    )
                  ),
                  
                  tabPanel(
                    h4("Gamma Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = 'shapeGamma',
                          label = 'Shape - alpha',
                          value = 1,
                          min = 0
                        ),
                        
                        numericInput(
                          inputId = 'scaleGamma',
                          label = 'Scale - beta',
                          value = 1,
                          min = 0
                        ),
                        
                        actionButton("plotGamma", "Plot")
                      ),
                      
                      mainPanel(plotOutput("Gamma"))
                    )
                  ),
                  
                  tabPanel(
                    h4("Exponential Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = 'rateExp',
                          label = 'Rate - lambda',
                          value = 1,
                          min = 0
                        ),
                        
                        actionButton("plotExp", "Plot")
                      ),
                      
                      mainPanel(plotOutput("Exp"))
                    )
                  ),
                  
                  tabPanel(
                    h4("Chi-square Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = 'dfChisq',
                          label = 'Degrees of freedom - nu',
                          value = 1,
                          min = 0
                        ),
                        
                        actionButton("plotChisq", "Plot")
                      ),
                      
                      mainPanel(plotOutput("Chisq"))
                    )
                  ),
                  
                  tabPanel(
                    h4("Beta Distribution"),
                    
                    # sidebar layout with input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = 'shapeBeta',
                          label = 'Shape 1 - alpha',
                          value = 1,
                          min = 0
                        ),
                        
                        numericInput(
                          inputId = 'scaleBeta',
                          label = 'Shape 2 - beta',
                          value = 1,
                          min = 0
                        ),
                        
                        actionButton("plotBeta", "Plot")
                      ),
                      
                      mainPanel(plotOutput("Beta"))
                    )
                  )
                )
              ),
              
              tabPanel(
                h3("Activity 2"),
                tabsetPanel(
                  tabPanel(
                    h4("Data Summary"),
                    verbatimTextOutput("randuSummary")
                  ),
                  
                  tabPanel(
                    h4("Histograms"),
                    sidebarLayout(
                      sidebarPanel(
                        # input
                        radioButtons(
                          inputId = "randuVarChoice",
                          label = "Selecting Variable",
                          choices = c("X" = "randux", "Y" = "randuy", "Z" = "randuz")
                        )
                      ),
                      mainPanel(plotOutput("randuHistogram"))
                    )
                  ),
                  
                  tabPanel(
                    h4("QQ Plot"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(
                          inputId = "randuEmp",
                          label = "Empirical Data",
                          choices = c("X" = "randuempx", 
                                      "Y" = "randuempy", 
                                      "Z" = "randuempz")
                        ),
                        
                        selectInput(
                          inputId = "theoreticalDist",
                          label = "Theoretical Distribution",
                          choices = c("Beta" = "beta",
                                      "Normal" = "norm", 
                                      "Chi Square" = "chisq", 
                                      "Exponential" = "exp",
                                      "Gamma" = "gamma",
                                      "Uniform" = "unif")
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'beta'",
                          numericInput(
                            inputId = 'qqShapeBeta',
                            label = 'Shape - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleBeta',
                            label = 'Scale - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'norm'",
                          numericInput(
                            inputId = 'qqMuNormal',
                            label = 'Mean',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqSigmaNormal',
                            label = 'Sigma',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'chisq'",
                          numericInput(
                            inputId = 'qqDfChisq',
                            label = 'Degrees of freedom',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'exp'",
                          numericInput(
                            inputId = 'qqRateExp',
                            label = 'Rate',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'gamma'",
                          numericInput(
                            inputId = 'qqShapeGamma',
                            label = 'Shape - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleGamma',
                            label = 'Scale - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist == 'unif'",
                          numericInput(
                            inputId = 'qqLowerUnif',
                            label = 'Lower Bound',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqUpperUnif',
                            label = 'Upper Bound',
                            value = 1
                          )
                        ),
                        
                        actionButton("plotqq", "Plot")
                      ),
                      
                      mainPanel(plotOutput("qqplot"))
                    )
                  )
                )
              ),
              
              tabPanel(
                h3("Activity 3"),
                tabsetPanel(
                  tabPanel(
                    h4("Data Summary"),
                    verbatimTextOutput("radonSummary")
                  ),
                  
                  tabPanel(
                    h4("Histograms"),
                    
                    mainPanel(plotOutput("radonHistogram"))
                  ),
                  
                  tabPanel(
                    h4("QQ Plot"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput(
                          inputId = "theoreticalDist3",
                          label = "Theoretical Distribution",
                          choices = c("Beta" = "beta",
                                      "Normal" = "norm", 
                                      "Chi Square" = "chisq", 
                                      "Exponential" = "exp",
                                      "Gamma" = "gamma",
                                      "Uniform" = "unif")
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'beta'",
                          numericInput(
                            inputId = 'qqShapeBeta3',
                            label = 'Shape 1 - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleBeta3',
                            label = 'Shape 2 - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'norm'",
                          numericInput(
                            inputId = 'qqMuNormal3',
                            label = 'Mean',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqSigmaNormal3',
                            label = 'Sigma',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'chisq'",
                          numericInput(
                            inputId = 'qqDfChisq3',
                            label = 'Degrees of freedom',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'exp'",
                          numericInput(
                            inputId = 'qqRateExp3',
                            label = 'Rate',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'gamma'",
                          numericInput(
                            inputId = 'qqShapeGamma3',
                            label = 'Shape - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleGamma3',
                            label = 'Scale - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist3 == 'unif'",
                          numericInput(
                            inputId = 'qqLowerUnif3',
                            label = 'Lower Bound',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqUpperUnif3',
                            label = 'Upper Bound',
                            value = 1
                          )
                        ),
                        
                        actionButton("plotqq3", "Plot")
                      ),
                      
                      mainPanel(plotOutput("qqplot3"))
                    )
                  )
                )
              ),
              
              tabPanel(
                h3("Activity 4"),
                tabsetPanel(
                  tabPanel(
                    h4("Data Summary"),
                    verbatimTextOutput("refSummary")
                  ),
                  
                  tabPanel(
                    h4("Histograms"),
                    
                    mainPanel(plotOutput("refHistogram"))
                  ),
                  
                  tabPanel(
                    h4("QQ Plot"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput(
                          inputId = "theoreticalDist4",
                          label = "Theoretical Distribution",
                          choices = c("Beta" = "beta",
                                      "Normal" = "norm", 
                                      "Chi Square" = "chisq", 
                                      "Exponential" = "exp",
                                      "Gamma" = "gamma",
                                      "Uniform" = "unif")
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'beta'",
                          numericInput(
                            inputId = 'qqShapeBeta4',
                            label = 'Shape 1 - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleBeta4',
                            label = 'Shape 2 - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'norm'",
                          numericInput(
                            inputId = 'qqMuNormal4',
                            label = 'Mean',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqSigmaNormal4',
                            label = 'Sigma',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'chisq'",
                          numericInput(
                            inputId = 'qqDfChisq4',
                            label = 'Degrees of freedom',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'exp'",
                          numericInput(
                            inputId = 'qqRateExp4',
                            label = 'Rate',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'gamma'",
                          numericInput(
                            inputId = 'qqShapeGamma4',
                            label = 'Shape - alpha',
                            value = 1,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqScaleGamma4',
                            label = 'Scale - beta',
                            value = 1,
                            min = 0
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.theoreticalDist4 == 'unif'",
                          numericInput(
                            inputId = 'qqLowerUnif4',
                            label = 'Lower Bound',
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = 'qqUpperUnif4',
                            label = 'Upper Bound',
                            value = 1
                          )
                        ),
                        
                        actionButton("plotqq4", "Plot")
                      ),
                      
                      mainPanel(plotOutput("qqplot4"))
                    )
                  )
                )
              ),
  )
)