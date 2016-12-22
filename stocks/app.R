library(quantmod)

# TODO Separate Server and UI files 
server <- function(input, output) {
  
  dateButtons <- reactiveValues(data = 'last 3 months')
  
  observeEvent(input$y1, {  dateButtons$data <- 'last 1 year'   })
  observeEvent(input$y3, {  dateButtons$data <- 'last 3 years'  })
  observeEvent(input$y5, {  dateButtons$data <- 'last 5 years'  })
  observeEvent(input$y10,{  dateButtons$data <- 'last 10 years' })
  observeEvent(input$d3, {  dateButtons$data <- 'last 3 days'   }) 
  observeEvent(input$w1, {  dateButtons$data <- 'last week'     })
  observeEvent(input$m3, {  dateButtons$data <- 'last 3 months' })
  observeEvent(input$m6, {  dateButtons$data <- 'last 6 months' })
  
  #minimum code because its the mostly run code
  output$mainPlot <- renderPlot({
    
    if(input$symb=="GOOG") outputSrc = "google"
    else outputSrc = "yahoo"
  
      #TODO add it into a Reaction
    data <- getSymbols(input$symb, src = outputSrc, auto.assign = FALSE)
    
    chartSeries(data, theme = chartTheme("white"),  subset=dateButtons$data,
                  type = "line", log.scale = FALSE, TA = c(addVo(),addBBands() ))

      #addCCI(),addWPR() wiliams percentage , addROC() - rate of change, , addEVWMA(), addMACD()
      # TODO Add more info
      # TODO Show all the data it brings down so I understand what else I can use
      
      #getSymbols("USD/EUR",src="oanda") <- get exchange rates
      #getFinancials Download and View Financial Statements
  })
  
  output$text1 <- renderText({ {paste("Output Stock: ", input$symb)}})
  output$text2 <- renderText({ {paste("Output Range: ", dateButtons$data)} })
  
  #output$fin <- getFin(input$symb,auto.assign = TRUE)
  #output$stats <- renderPrint({ summary(rnorm(data) )  })
}

ui <- fluidPage(
    titlePanel( h3("Stockmarket View of Data", align = "center") ),
    
      sidebarLayout(
      position = "right",
      sidebarPanel( 
        fluidRow( 
          column(2,
                 actionButton("d3", "3D"), 
                 actionButton("y1", "1Y")
          ),
          
          column(2, offset = 1,
                 actionButton("w1", "1W"), 
                 actionButton("y3", "3Y")
          ),
          
          column(2, offset = 1,
                 actionButton("m3", "3M"), 
                 actionButton("y5", "5Y")
          ),
          
          column(2, offset = 1,
                 actionButton("m6", "6M"),
                 actionButton("y10", "10Y")
          )
        ), 
    
        br(),
        selectInput("symb", "Select Stock:",
                    c("SPY" = "SPY", "FTSE"="^FTSE", "Google" = "GOOG","Amazon" = "AMZN", "Microsoft"="MSFT" ))
        
        #br(),
        #dateRangeInput("dates", 
        #               "Date range",
        #               start = "2013-01-01", 
        #               end = as.character(Sys.Date()))
        
        #br(),
        #sliderInput("range", 
        #            label = "Range of interest:",
        #            min = 0, max = 100, value = c(0, 100))
      ),
      mainPanel( plotOutput("mainPlot"), textOutput("text1") ,textOutput("text2"))
      
      #tabsetPanel(
      #  tabPanel("Plot", plotOutput("plot")), 
      #  tabPanel("Summary", verbatimTextOutput("summary")), 
      #  tabPanel("Table", tableOutput("table"))
      )
    )
#)

shinyApp(ui = ui, server = server)