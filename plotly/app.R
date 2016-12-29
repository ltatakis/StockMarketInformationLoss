library(quantmod)
library(plotly)
packageVersion('plotly')

# TODO Separate Server and UI files 
server <- function(input, output) {
  
  output$currencyPlot <-renderPlotly({
    returnedData<- getFX("GBP/EUR", from = "2012-01-01", to = Sys.Date(), auto.assign = FALSE)
    ds <- data.frame(Date = index(returnedData),coredata(returnedData))
    
    p <- plot_ly(ds, x = ~Date) %>%
      add_lines(y = ~GBP.EUR, name = "GBR/EUR") %>%
      layout(
        title = "Currency Prices for GBR/EUR",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list( count = 3, label = "3 mo", step = "month", stepmode = "backward"),
              list( count = 6, label = "6 mo", step = "month", stepmode = "backward"),
              list( count = 1, label = "1 yr", step = "year",  stepmode = "backward"),
              list( count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list( count = 3, label = "3 yr", step = "year", stepmode = "backward"),
              list( count = 5, label = "5 yr", stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Price"))
  })
  
  
  output$newplotly <-renderPlotly({
    outputSrc = "yahoo"
    getSymbols(Symbols = c("AAPL", "MSFT"), src = outputSrc)
    
    ds <- data.frame(Date = index(AAPL), AAPL[,6], MSFT[,6])
    
    p <- plot_ly(ds, x = ~Date) %>%
      add_lines(y = ~AAPL.Adjusted, name = "Apple") %>%
      add_lines(y = ~MSFT.Adjusted, name = "Microsoft") %>%
      layout(
        title = "Stock Prices",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list( count = 3, label = "3 mo", step = "month", stepmode = "backward"),
              list( count = 6, label = "6 mo", step = "month", stepmode = "backward"),
              list( count = 1, label = "1 yr", step = "year",  stepmode = "backward"),
              list( count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(step = "all"))),
              rangeslider = list(type = "date")),
              yaxis = list(title = "Price"))
  })
}

ui <- fluidPage(
  titlePanel( "Stockmarket View of Data" ),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h3("Interactive visualisation using plot.ly")
    ),
    
    mainPanel(plotlyOutput("newplotly"),plotlyOutput("currencyPlot"))
  
  )
)


shinyApp(ui = ui, server = server)