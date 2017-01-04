library(quantmod)
library(ggplot2)
library(shiny)

server <- function(input, output) {
  # need to add this in a reactive expression and add multiple stocks
  stockData <- getSymbols("GOOG", src = "google",  auto.assign = FALSE)
  colnames(stockData) <- c("Open", "High", "Low", "Price", "Volume")
  dataInput = data.frame( Date = index(stockData), stockData)
  zoom <- reactiveValues(data = dataInput)
  
  #state array
  arrayOfChange = as.data.frame(matrix( 0, ncol=2, nrow=1))
  names(arrayOfChange) = c( "State", "Data" )
  stateData <- reactiveValues( states = 0 )
  arrayData <- reactiveValues( stateArray = arrayOfChange )
  
  data <- reactiveValues(newPoints = dataInput)
  
  observeEvent(input$plot_brush, {
    data$newPoints <-  brushedPoints(dataInput, input$plot_brush, "Date", "Price") 
    stateData$states <- stateData$states + 1
    arrayData$stateArray = rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoints) ))
  })
 
  
  #start up plot
  output$zoomPlot <- renderPlot({

    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    ggplot(data = zoom$data , aes(x= Date, y = Price ) ) + 
      geom_line( colour = "#0072B2"  ) +
      theme_bw()
    
  })
  
  
  output$mainPlot <- renderPlot({
    ggplot(data = data$newPoints, aes(x= Date, y = Price ) ) + 
      geom_line( colour = "#0072B2"  ) +
      theme_bw() 
  })
  
  output$stateChangePlot <- renderPlot({
    ggplot(data = arrayData$stateArray, aes(x= State, y = Data ) ) + 
      geom_line( colour = "#0072B2"  ) +
      labs( title = "Amount of information shown in the main plot due to interactions by the user") +
      theme_bw() 
  })
  
  output$infoOfPoint <- renderPrint({
    if (is.null(input$main_plot_click)) return("No selected data.")
    else {
      keeprows <- nearPoints(data$newPoints, input$main_plot_click , xvar = "Date", yvar ="Price", threshold = 10, maxpoints = 1)
      print(subset(keeprows, select=c("Price", "Open", "High", "Low")))
    }
  })
  
  output$financialsText <- renderTable({
    x.f <- getFinancials("GOOG", "BS" , "A", auto.assign=FALSE)
    viewFinancials(x.f)
  })
  
  #lapply(fin, function(x) x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",])
  
  #co.f <- getFinancials("GOOG", "BS" , "A" ,auto.assign=FALSE)
  #ratios = data.frame( Ratio = index( co.f),  co.f)
  #viewFinancials(co.f) 
  #print("-------------1")
  #print(head(co.f))
  #output$financial <- renderDataTable (viewFinancials(co.f))
  
  observeEvent(input$y1, {  zoom$data <- subset(dataInput, Date > as.Date("2016-01-4") )
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray <- rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint) ))
                          })
  observeEvent(input$y5, {  zoom$data <- subset(dataInput, Date > as.Date("2012-01-4") )
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray <- rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint) ))
                          })
  observeEvent(input$y10,{ zoom$data <- subset(dataInput, Date > as.Date("2007-01-4") ) 
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray <- rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint) ))
                          })
  observeEvent(input$m3, {  zoom$data <- subset(dataInput, Date > as.Date("2016-09-1") ) 
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray = rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint) ))
                          })
  observeEvent(input$m6, { zoom$data <- subset(dataInput, Date > as.Date("2016-06-1") ) 
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray = rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint) ))
                          })
  observeEvent(input$all,{  zoom$data <- dataInput 
                            #stateData$states <- stateData$states + 1
                            #arrayData$stateArray = rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoint)))
                          })
}


ui <- fluidPage(
  h3("Stockmarket View of Data" , align = "center"),
  hr(),
  sidebarLayout(
    sidebarPanel( h3("Information of Selected point"),
                  verbatimTextOutput("infoOfPoint"),  
                  h3("Results of Interactions"), 
                  p("Amount of information in each state of the system"), 
                  plotOutput("stateChangePlot"),
                  width = 4),
  
  mainPanel(
    # TODO add navbar ? for currency also?
    fluidRow( column(1, p("Stock selection:") ),
              column(7, selectInput("select", label = NULL,  choices = list("GOOG" = 1, "AMZN" = 2, "FTSE" = 3), 
                                                        selected = 1)
                        )
              ),
    
    fluidRow( column(12, plotOutput("mainPlot", hover = "main_plot_hover" , click = "main_plot_click"))),
    fluidRow( column(12, plotOutput("zoomPlot", 
                              hover = "plot_hover",
                              brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x", resetOnNew = FALSE),
                              height= "150"
                          )
            )
    ),
    fluidRow( column(7, 
                     h4("Define time period of enquiry:"),
                     actionButton("w1", "1W"),
                     actionButton("M3", "3M"), 
                     actionButton("m6", "6M"),
                     actionButton("y1", "1Y"),
                     actionButton("y5", "5Y"),
                     actionButton("y10", "10Y"),
                     actionButton("all", "All")
     ))
    ), 
  position = "right"
  )
  #, dataTableOutput("table"))
  #sidebarPanel(tableOutput("financialsText"), width = 3)
  #sidebarPanel(tableOutput("earnings"), width = 3)
)


shinyApp(ui = ui, server = server)