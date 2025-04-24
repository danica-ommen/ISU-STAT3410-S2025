library(shiny)

ui <- fluidPage(
  
  titlePanel("STAT 3410: Lab #10"),
  
  tabsetPanel(type="tabs",
              
              tabPanel(
                h4("Activity 1: Mandel's Peas"),
                
                sidebarLayout(
                  sidebarPanel(
                    numericInput(
                      inputId="pea1",
                      label="Yellow and round peas",
                      value = 1,
                      min = 1
                    ),
                    
                    numericInput(
                      inputId="pea2",
                      label="Yellow and wrinkled peas",
                      value = 1,
                      min = 1
                    ),
                    
                    numericInput(
                      inputId="pea3",
                      label="Green and round peas",
                      value = 1,
                      min = 1
                    ),
                    
                    numericInput(
                      inputId="pea4",
                      label="Green and wrinkled peas",
                      value = 1,
                      min = 1
                    ),
                    
                    actionButton('calcPeas', "Calculate probability")
                  ),
                  mainPanel(
                    verbatimTextOutput('print6')
                  )
                )
              ),
              
              tabPanel(
                h4("Activity 2: Simplified Dice"),
                
                sidebarLayout(
                  sidebarPanel(
                    numericInput(
                      inputId = 'seed',
                      label = "Seed",
                      value = 1
                    ),
                    
                    numericInput(
                      inputId = 'size',
                      label = "Number of times to roll this dice",
                      value = 100
                    ),
                    
                    actionButton(
                      inputId = 'roll',
                      label = "Roll"
                    ),
                    
                    numericInput(
                      inputId = "trialsNum",
                      label = "Number of trials to run",
                      value = 20
                    ),
                    
                    actionButton(
                      inputId = 'sim',
                      label = "Simulate"
                    )                
                  ),
                  mainPanel(
                    verbatimTextOutput('print1'),
                    tableOutput("table1"),
                    
                    plotOutput("plot1"),
                    verbatimTextOutput('print2'),
                    tableOutput('table2'),
                    verbatimTextOutput('print4'),
                    verbatimTextOutput('print5')
                  )
                )
              ),
              
              tabPanel(
                h4("Activity 3: Realistic Dice"),
                
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      inputId='weights',
                      label="Choice of weight parameters",
                      choices = c(
                        "a1 = a2 =... = a10" = 'equal',
                        "a1 = 1, a2 = 2, a3 = 3,..., a10 = 10" = 'sequential',
                        "a1 = 1, a2 = 2, a3 = 4,..., a10 = 512" = 'expo'
                      ),
                      selected='equal'
                    )
                  ),
                  mainPanel(
                    verbatimTextOutput("print7")
                  )
                )
                
              )
  )
)