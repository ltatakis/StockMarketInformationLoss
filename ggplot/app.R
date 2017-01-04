library(quantmod)

library(ggplot2)
library(shiny)

server <- function(input, output) {
  
  stockData <- getSymbols("GOOG", src = "google",  auto.assign = FALSE)
  colnames(stockData) <- c("Open", "High", "Low", "Price", "Volume")
  dataInput = data.frame( Date = index(stockData), stockData)
  #print(head(dataInput))
  
  arrayOfChange = as.data.frame(matrix(0, ncol=2, nrow=1))
  names(arrayOfChange) = c( "State", "Data" )
  #print(arrayOfChange)
  
  stateData <- reactiveValues(states = 0)
  arrayData <- reactiveValues( stateArray = arrayOfChange )
  
  data <- reactiveValues(newPoints = dataInput)
  observeEvent(input$plot_brush, {
    data$newPoints <-  brushedPoints(dataInput, input$plot_brush, "Date", "Price") 
    stateData$states <- stateData$states + 1
    arrayData$stateArray = rbind(arrayData$stateArray, c(stateData$states, nrow(data$newPoints) ))
    #print(arrayData$stateArray)
  })

  outputPlot <- ggplot(data = dataInput , aes(x= Date, y = Price ) ) + 
        geom_line( colour = "#0072B2"  ) +
        theme_bw()
 
  
  #start up plot
  output$zoomPlot <- renderPlot({outputPlot})
  
  
  output$mainPlot <- renderPlot({
    ggplot(data = data$newPoints, aes(x= Date, y = Price ) ) + 
      geom_line( colour = "#0072B2"  ) +
      #labs( title = "S&P 500 v2 (ggplot2::ggplot)") +
      #guides(colour=FALSE) + 
      theme_bw() 
  })
  
  output$stateChangePlot <- renderPlot({
    ggplot(data = arrayData$stateArray, aes(x= State, y = Data ) ) + 
      geom_line( colour = "#0072B2"  ) +
      labs( title = "Amount of information shown in Main Plot due to interactions by the user") +
      theme_bw() 
  })
  
  output$infoOfPoint <- renderPrint({
    nearPoints(data$newPoints, input$main_plot_click , xvar = "Date", yvar =  "Price",threshold = 10, maxpoints = 2)
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
  
}


ui <- fluidPage(
  h1("Stockmarket View of Data" , align = "center"),
  hr(),
  sidebarLayout(
    sidebarPanel( h3("Results of Interactions"), 
                  p("Amount of information in each state of the system"), 
                  plotOutput("stateChangePlot"),
                  hr(),
                  h2("Information of Selected point"),
                  verbatimTextOutput("infoOfPoint"),  width = 4),
  mainPanel(
    fluidRow(
      column(12, plotOutput("mainPlot", hover = "main_plot_hover" , click = "main_plot_click")),
      column(12,  plotOutput("zoomPlot", 
                              hover = "plot_hover",
                              brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x"),
                              height= "150"
                             )
                  )
      )
    ), position = "right"
  )
  #, dataTableOutput("table"))
  #sidebarPanel(tableOutput("financialsText"), width = 3)
  #sidebarPanel(tableOutput("earnings"), width = 3)
)


shinyApp(ui = ui, server = server)