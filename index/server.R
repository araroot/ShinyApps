library(shiny)
library(ggplot2)
library(plyr)
library(quantmod)
library(lubridate)
library(reshape2)

get.data <- function(symbol, years, ma.period) {
	df.index <- read.csv(paste('data/',symbol,'.csv',sep=''))
	df.index$Date <- dmy(df.index$Date)
	df.processed <- analyze.index(df.index, as.numeric(ma.period)) 
	df.processed <- subset(df.processed, year(Date) >= years[1] & year(Date) <= years[2])
	df.processed
}

analyze.index <- function(df.in, n) {
  k = 2 # hardcoded for lkfk
  df.raw <- df.in[,c("Date","Close")]
  colnames(df.raw) <- c("Date", "Close")
  df.raw$year <- factor(year(df.raw$Date))
  df.raw$month <- month(df.raw$Date)
  df.raw$ret <- ROC(df.raw$Close)
  #
  df.raw$l2f2 <- FALSE
  df.raw[c(1:k),]$l2f2 <- TRUE  # set first 2 dates to TRUE
  for (i in ((k+1):(nrow(df.raw) - k)) ) { 
    curm <- month(df.raw[i,]$Date)
    pm <- month(df.raw[(i-k),]$Date)
    nm <- month(df.raw[(i+k),]$Date)
    if (curm != pm || curm != nm) {df.raw[i,]$l2f2 <- TRUE}
  }
  #
  df.raw$sma <- SMA(df.raw$Close,n)
  df.raw$flag <- ifelse(df.raw$Close > df.raw$sma, TRUE, FALSE)
  df.raw$smaflag <-c(NA, df.raw$flag[1:(nrow(df.raw)-1)]) # Lag the flag by 1 day
  #
  df.raw$anyc <- ifelse(df.raw$smaflag == TRUE | df.raw$l2f2 == TRUE, TRUE, FALSE)
  
  df.raw
}

sma.stock <- function(in.xts, ndays) {
  #df.raw <- data.frame(index(in.xts), in.xts[,4]) # Close is better than adjusted close
  df.raw <- in.xts[,c("Date","Close")]
  colnames(df.raw) <- c("Date", "Close")
  df.raw$year <- factor(year(df.raw$Date))
  df.raw$sma <- SMA(df.raw$Close,n=ndays)
  df.raw$ret <- ROC(df.raw$Close)
  df.raw$flag <- ifelse(df.raw$Close > df.raw$sma, TRUE, FALSE)
  df.raw$smaflag <-c(NA, df.raw$flag[1:(nrow(df.raw)-1)]) # Lag the flag by 1 day
  df.raw <- df.raw[!is.na(df.raw$smaflag),] 
  df.raw$flip <- ifelse(df.raw$flag == df.raw$smaflag, 0, 1)
  df.raw
}


l2f2.stock <- function(in.xts, n) {
  #df.raw <- data.frame(index(in.xts), in.xts[,4]) # Close is better than adjusted close
  df.raw <- in.xts[,c("Date","Close")]
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
  dataset <- reactive(get.data(input$index, input$ayears, input$ma.period)) 
  
  output$plot_l2f2 <- renderPlot({
	df.raw <- dataset()
	df.out <- ddply(df.raw, c('year','l2f2'), summarise, total_return=sum(ret, na.rm=T)*100.0)
	p <- ggplot(df.out, aes(x=year, y=total_return, fill=l2f2)) + geom_bar(stat='identity', position='dodge')
	p <- p + scale_fill_manual(values=c('pink','blue'))
	p <- p + ggtitle('Analysis of last 2 and first 2 days of month')
	p <- p + guides(fill=FALSE) 
	p <- p + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
	print(p)
  })
  
  output$plot_sma <- renderPlot({
	df.raw <- dataset()
	df.in <- ddply(df.raw, c('year','smaflag'), summarise, total_return=sum(ret, na.rm=T)*100.0)
	p <- ggplot(df.in, aes(x=year, y=total_return, fill=smaflag)) + geom_bar(stat='identity', position='dodge')
	p <- p + scale_fill_manual(values=c('pink','blue'))
	p <- p + ggtitle('Analysis of returns above and below SMA')
	p <- p + guides(fill=FALSE) 
	p <- p + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
	print(p)
  })
  
  
  output$plot_equity <- renderPlot({
	df.raw <- dataset()
	df.raw <- df.raw[complete.cases(df.raw),]
	#cat(summary(df.raw))
	df.raw$s.bh <- 1000.0
	df.raw$s.l2f2 <- 1000.0
	df.raw$s.sma <- 1000.0
	df.raw$s.any <- 1000.0
	
	for (i in (2:nrow(df.raw)) ) {
		eret = exp(df.raw$ret[i])
		df.raw$s.bh[i]   <- df.raw$s.bh[i-1] * eret
		df.raw$s.l2f2[i] <- ifelse(df.raw$l2f2[i], df.raw$s.l2f2[i-1] * eret, df.raw$s.l2f2[i-1])
		df.raw$s.sma[i]  <- ifelse(df.raw$smaflag[i], df.raw$s.sma[i-1] * eret, df.raw$s.sma[i-1])
		df.raw$s.any[i] <- ifelse(df.raw$anyc[i], df.raw$s.any[i-1] * eret, df.raw$s.any[i-1])
		
	}
	p <- ggplot(df.raw, aes(x=Date, y=s.bh)) 
	p <- p + geom_line(color='black')
	p <- p + geom_line(aes(x=Date, y=s.l2f2, color='L2F2 only'))
	p <- p + geom_line(aes(x=Date, y=s.sma, color='SMA only'))
	p <- p + geom_line(aes(x=Date, y=s.any, color='Both'))
	p <- p + scale_color_manual(values=c('blue', 'red', 'green'), name='')
	
	print(p)
  })
  	
  #output$summary_l2f2 <- renderTable({
	#df.in <- l2f2.stock(dataset(), n=2)
	#df.wide <- dcast(df.in, year ~ l2f2, value.var="total_return")
	#rownames(df.wide) <- NULL
	#df.wide
  #})
  
})
