library(shiny)
library(ggplot2)
library(plyr)
library(quantmod)
library(lubridate)
library(reshape2)

my.stock <- function(symbol, n) {
  tmp.xts <- getSymbols(symbol, src='yahoo', auto.assign=F)
  df.raw <- data.frame(index(tmp.xts), tmp.xts[,4]) # Close is better than adjusted close
  colnames(df.raw) <- c("Date", "Close")
  df.raw$year <- factor(year(df.raw$Date))
  df.raw$month <- month(df.raw$Date)
  df.raw$ret <- ROC(df.raw$Close)
  df.raw$l2f2 <- FALSE
  df.raw[c(1:n),]$l2f2 <- TRUE  # set first 2 dates to TRUE
  
  for (i in ((n+1):(nrow(df.raw) - n)) ) { 
    curm <- month(df.raw[i,]$Date)
    pm <- month(df.raw[(i-n),]$Date)
    nm <- month(df.raw[(i+n),]$Date)
    if (curm != pm || curm != nm) {df.raw[i,]$l2f2 <- TRUE}
  }
  df.out <- ddply(df.raw, c('year','l2f2'), summarise, total_return=sum(ret, na.rm=T)*100.0)
  
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  dataset <- reactive(my.stock(input$symbol, input$n))
  
  output$plot <- renderPlot({
	df.in <- dataset()
	p <- ggplot(df.in, aes(x=year, y=total_return, fill=l2f2)) + geom_bar(stat='identity', position='dodge')
	p <- p + scale_fill_manual(values=c('pink','blue'))
	print(p)
  })
  	
  output$summary <- renderTable({
	df.in <- dataset()
	df.wide <- dcast(df.in, year ~ l2f2, value.var="total_return")
	rownames(df.wide) <- NULL
	df.wide
  })
})
