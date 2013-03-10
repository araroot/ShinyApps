
add.eom <- function(df.in) {
  results <- data.frame()
  for (yyyy in unique(year(df.in$Date))) {
    df.year <- subset(df.in, year(Date)==yyyy)
    for (mm in unique(month(df.year$Date))) {
      df <- subset(df.year, month(Date)==mm)
      df$eom <- NA
      n <- nrow(df)
      if (n < 15) { next }
      for (i in c(1:10)) {
        df[i,]$eom <- i
      }
      for (i in c(0:10)) {
        df[(n -i),]$eom <- -1 * i
      }
      results <- rbind(results, df)
    }
  }
  return(results)
}

plot.df <- function(df.in) {
  df <- ddply(df.in, .(year, eom), summarize, mret = mean(ret, na.rm=T))
  df$pos <- ifelse(df$mret > 0, 1, -1)
  p <- ggplot(subset(df, year <= 2012), aes(x=eom, y=mret, fill=factor(pos))) 
  p <- p + geom_bar(stat='identity') 
  p <- p + facet_grid(year ~ .) 
  p <- p + scale_fill_manual(values=c('red','green'), guide=FALSE) 
  p <- p + xlab('Day of Month') + ylab('Mean Returns') + theme(axis.title.y=element_blank())
  p
}