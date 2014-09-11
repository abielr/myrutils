GG.ts <- function(x, labels) {
  if (missing(labels))
    labels <- names(x)[names(x)!='date']
  
  gdata <- NULL
  
  dates <- x[[1]]
  for (i in 1:(ncol(x)-1))
    gdata <- rbind(gdata, data.table(date=dates, value=x[[i+1]], label=labels[i]))
  
  ggplot(gdata, aes(x=date, y=value, color=label))+geom_line()
}