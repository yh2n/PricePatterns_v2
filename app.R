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

#error: 2012/44, xlu, 2014 2012 2014 33
companies <- (c("SPY", "QQQ","XLU", "EMB", "XAR", " XLU",
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
"SPY",
"QQQ",
"IVE",
"IGV",
"XLF",
"XLI",
"XNTK",
"XLB",
"XT",
"SKYY",
"XLV",
"FDN",
"EWJ",
"EFA",
"IXP",
"VOX",
"ROBO",
"EEM",
"EWH",
"IBB",
"XRT",
"EMQQ",
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
    index <- round(runif(1, 1, 57))
    index
  }
  
  #picks random symbol from Nasdaq 100
  #a <- nasdaq100[randomSymbolIndex()]
  # a <- symbols[randomSymbolIndex(), 1]
  #getSymbols(a)
  #converts to "xts" symbol
  #Prices <- get(a)
  
  a <- companies[randomSymbolIndex()]
  getSymbols(a, from="2013-04-01", to="2016-04-01")
  Prices <- get(a)
  
  
  x <- round(runif(1, 2007, 2019))
  y <- round(runif(1, 2007, 2019))
  if(x < y) {
    randNum1 <- x
    randNum2 <- y
  }else {
    randNum1 <- y
    randNum2 <- x
  }
  charFromNum1 <- as.character(randNum1)
  charFromNum2 <- as.character(randNum2)
  
  
  output$PriceChart1 <- renderPlot({
    lineChart(Prices,
              name= a,
              type="line",
              # subset = "2008",
              show.grid = T,
              minor.ticks = TRUE,
              up.col = "#510F59",
              color.vol = T,
              theme="white",
              log.scale = T,
              TA= NULL
    )
  })
  
  # output$PriceChart2 <- renderPlot({
  #  lineChart(Prices,
  #            name= a,
  #            type="line",
  #            # subset = "2018",
  #            show.grid = T,
  #            up.col = "#F24E29",
  #            minor.ticks = TRUE,
  #            color.vol = T,
  #            theme="black",
  #            log.scale = T,
  #            TA= NULL
  #  )
  # })

  output$value <- renderPrint({ input$action })
  
  #reactive events
  # http://www.bradfordtuckfield.com/rproblems.html
  observeEvent(input$mydata, {
    yearA <- format(as.Date(input$mydata[1]), "%Y")
    yearB <- format(as.Date(input$mydata[2]), "%Y")
    if(yearA < yearB) {
      year1 <- yearA
    }else {
      year1 <- yearB
    }
    # year1 <- as.character(input$mydata[1])
    # we are getting a character from the FE...
    b <- companies[as.numeric(input$mydata[3])]
    getSymbols(b)
    p <- get(b)
    output$PriceChart1 <- renderPlot({
      lineChart(p,
                name= b,
                type="line",
                subset = year1,
                show.grid = T,
                dn.col = "pink",
                up.col = "#254159",
                minor.ticks = TRUE,
                color.vol = T,
                theme="white",
                log.scale = T,
                TA= NULL
      )
    })
  })
  observeEvent(input$mydata, {
    yearA <- format(as.Date(input$mydata[1]), "%Y")
    yearB <- format(as.Date(input$mydata[2]), "%Y")
    if(yearA < yearB) {
      year2 <- yearB
    }else {
      year2 <- yearA
    }
    b <- companies[as.numeric(input$mydata[3])]
    getSymbols(b)
    p <- get(b)
    output$PriceChart2 <- renderPlot({
      lineChart(p,
                name= b,
                type="line",
                subset = year2,
                show.grid = T,
                up.col = "#F24E29",
                dn.col = "pink",
                minor.ticks = TRUE,
                color.vol = T,
                theme="black",
                log.scale = T,
                TA= NULL
      )
    })
  })
}

shinyApp(ui = htmlTemplate("www/index.html"), server)

