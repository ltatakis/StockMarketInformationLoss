library(quantmod)
library(ggplot2)

server <- function(input, output) {
  
  stockData <- getSymbols("GOOG", src = "google",  auto.assign = FALSE)
  colnames(stockData)[4] = 'price'
  dataInput = data.frame( Date = index(stockData), stockData)
  print(head(dataInput) )
  
 #currencyData <- reactive({ 
  #  getFX(input$currency, from = "2012-01-01", to = Sys.Date(), auto.assign = FALSE)
  #  })
  #output$table <- renderDataTable(stockData())
  
  outputPlot <- ggplot(data = dataInput , aes(x= Date, y = price ) ) + 
        geom_line( colour = "#0072B2"  ) +
       labs( title = "S&P 500 (ggplot2::ggplot)") +
      #guides(colour=FALSE) + 
      theme_bw()
 
  
  #start up plot
  output$mainPlot <- renderPlot({outputPlot })
  
  output$info <- renderPrint({
    brushedPoints(dataInput, input$plot_brush, "Date", "price")
  })
  
  #observeEvent(input$plot_hover, {
  #  x = input$plot_hover$x
  #  y = input$plot_hover$y
  #  print(x)
  #  nearPoint <- nearPoints(dataInput, input$plot_hover,  "Date","price",
  #                          threshold = 10, maxpoints = 1)
    
  #  output$mainPlot <- renderPlot({
  #   if (nrow(nearPoint) == 1) {
  #      print("In if")
  #     print(nearPoint)
  #        outputPlot + 
  #        geom_vline(xintercept = as.numeric(nearPoint$Date) ) +
  #        geom_label(x = x + 1.5, y = y, 
  #                   label = paste(rownames(nearPoint), "\n", nearPoint$price))
  #    } else {
  #     print("In else")
  #       outputPlot
  #    }
  #  })
  #})
  
}


ui <- fluidPage(
  titlePanel( h3("Stockmarket View of Data", align = "center") ),

  mainPanel( plotOutput("mainPlot", hover = "plot_hover", brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x")),
             verbatimTextOutput("info")
            )#, dataTableOutput("table"))
)


shinyApp(ui = ui, server = server)