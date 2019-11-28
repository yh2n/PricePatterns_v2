# _____________________deployment____________
# library(rsconnect)
#rsconnect::deployApp('/Users/yohann/Desktop/projects/R/PricePatterns_v2')

library(shiny)
library(quantmod)

nasdaq100 <- (c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM",
                "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", "ATVI",
                "AVGO","BBBY", "BIDU", "BIIB", "CA", "CELG",
                "CERN", "CHKP", "CHRW", "CHTR", "CMCSA","COST", "CSCO",
                "CTSH", "CTXS", "DISCA", "DISCK", "DISH", "DLTR", 
                "DTV","EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST", "FB",
                "FFIV", "FOXA", "GILD", "GOOG", "GOOGL", "GRMN", "HSIC",
                "ILMN", "INTC", "INTC", "INTU", "ISRG", "KLAC", "LBTYA",
                "MAR", "MAT", "MDLZ"
))

#error: 2012/44, 2014 2012 2014/33, 2008/41, 2008/42, 2011/28, 2010/41,
#2009/44, ROBO, SMH, 2008/5, 2011/5 starts in september, 2014/28, 2013/28
companies <- (c("SPY", "QQQ","XLU", "EMB", "XAR", "XLU",
"EMB",
"XAR",
"ITB",
"XLP",
"ITA",
"IHI",
"XHB",
"XLK",
"XLY",
"SMH",
"QTEC",
"RWR",
"IVV",
"IVE",
"IGV",
"XLF",
"XLI",
"XNTK",
"XLB",
"SKYY",
"XLV",
"FDN",
"EWJ",
"EFA",
"IXP",
"VOX",
"EEM",
"EWH",
"IBB",
"XRT",
"XTL",
"IXC",
"KWEB",
"XLE",
"USO",
"UNG",
"UNL",
"XOP",
"IEZ"))
server <- function(input, output, session) {
  randomSymbolIndex <- function () {
    index <- round(runif(1, 1, 49))
    index
  }
  
  #picks random symbol from Nasdaq 100
  #a <- nasdaq100[randomSymbolIndex()]
  # a <- symbols[randomSymbolIndex(), 1]
  #getSymbols(a)
  #converts to "xts" symbol
  #Prices <- get(a)
  
  x <- round(runif(1, 2007, 2019))
  y <- x + 2
  
  charFromNum1 <- as.character(x)
  charFromNum2 <- as.character(y)
  year1 <- format(as.Date(charFromNum1, format="%Y"))
  year2 <- format(as.Date(charFromNum2, format="%Y"))
  # a <- companies[45]
  a <- companies[randomSymbolIndex()]
  getSymbols(a, from="2013-01-01", to="2014-01-01")
  #converts to "xts" symbol
  Prices <- get(a)
  
  output$PriceChart1 <- renderPlot({
    lineChart(Prices,
              name= a,
              type="line",
              show.grid = T,
              minor.ticks = TRUE,
              up.col = "#510F59",
              color.vol = T,
              theme="white",
              log.scale = T,
              TA= NULL
    )
  })
  

  output$value <- renderPrint({ input$action })
  
  #----------reactive events
  
  # http://www.bradfordtuckfield.com/rproblems.html
  observeEvent(input$mydata, {
    year1 <- input$mydata[1]
    year2 <- input$mydata[2]
    # year1 <- as.character(input$mydata[1])
    # year2 <- as.character(input$mydata[2])
    
    yearA <- format(as.Date(year1))
    yearB <- format(as.Date(year2))
    # yearA <- format(as.Date(input$mydata[1]), "%Y")
    # yearB <- format(as.Date(input$mydata[2]), "%Y")
    
    # year1 <- as.character(input$mydata[1])
    # we are getting a character from the FE...
    b <- companies[as.numeric(input$mydata[3])]
    getSymbols(b, from=yearA, to=yearB)
    p <- get(b)
    output$PriceChart1 <- renderPlot({
      lineChart(p,
                name= b,
                type="line",
                # subset = "2010::2011",
                show.grid = T,
                up.col = "#510F59",
                minor.ticks = TRUE,
                color.vol = T,
                theme="white",
                log.scale = T,
                TA= NULL
      )
    })
  })
}

shinyApp(ui = htmlTemplate("www/index.html"), server)

