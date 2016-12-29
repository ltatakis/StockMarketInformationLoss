library(quantmod)

# TODO Separate Server and UI files 
server <- function(input, output) {
  
  dateButtons <- reactiveValues(data = 'last 1 year')
  
  observeEvent(input$y1, {  dateButtons$data <- 'last 1 year'   })
  observeEvent(input$y3, {  dateButtons$data <- 'last 3 years'  })
  observeEvent(input$y5, {  dateButtons$data <- 'last 5 years'  })
  observeEvent(input$y10,{  dateButtons$data <- 'last 10 years' })
  observeEvent(input$d3, {  dateButtons$data <- 'last 3 days'   }) 
  observeEvent(input$w1, {  dateButtons$data <- 'last 1 week'   })
  observeEvent(input$m3, {  dateButtons$data <- 'last 3 months' })
  observeEvent(input$m6, {  dateButtons$data <- 'last 6 months' })
  
  stockData <- reactive({
    if(input$symb=="GOOG") outputSrc = "google"
    else outputSrc = "yahoo"
    getSymbols(input$symb, src = outputSrc,  auto.assign = FALSE)
  })
  
  currencyData <- reactive({ 
    getFX(input$currency, from = "2012-01-01", to = Sys.Date(), auto.assign = FALSE)
  })
  
  output$table <- renderDataTable(stockData())
  
  output$currencyPlot <-renderPlot({
    chartSeries(currencyData(), theme = chartTheme("white"), subset=dateButtons$data, type = "line", log.scale = FALSE, TA =NULL)
  })
  
  output$mainPlot <- renderPlot({
      chartSeries(stockData(), theme = chartTheme("white"),  subset=dateButtons$data,
                  type = "line", log.scale = FALSE, TA = c(addROC(),addWPR() ))
  })
  
  #getFinancials Download and View Financial Statements
}

ui <- fluidPage(
    titlePanel( h3("Stockmarket View of Data", align = "center") ),
    sidebarLayout(
    position = "right",
      sidebarPanel( 
        h4("Select time span:"),
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
                      c("SPY" = "SPY", "FTSE"="^FTSE", "Google" = "GOOG","Amazon" = "AMZN", "Microsoft"="MSFT" )),
        br(),
        selectInput("currency", "Select Currency:",
                    c("GBP/EUR", "EUR/GBP", "EUR/USD", "USD/EUR", "GBP/USD", "USD/GBP"))
      ),
    
      #mainPanel( plotOutput("mainPlot"), textOutput("text1") ,textOutput("text2"), plotOutput("currencyPlot"))
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("mainPlot"), plotlyOutput("plotlyPlot"), verbatimTextOutput("event")), 
          tabPanel("Currency", plotOutput("currencyPlot")), 
          tabPanel("Table", dataTableOutput("table"))
        )
      )
   )
)


shinyApp(ui = ui, server = server)