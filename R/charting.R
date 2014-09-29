gg.tsline <- function(..., labels) {
  series <- list(...)
  if (missing(labels))
    labels <- unlist(lapply(series, function(x) names(x)[-1]))
  
  gdata <- NULL
  
  dates <- x[[1]]
  counter <- 1
  for (j in 1:length(series)) {
    x <- series[[j]]
    dates <- x[[1]]
    for (i in 1:(ncol(x)-1)) {
      gdata <- rbind(gdata, data.table(date=dates, value=x[[i+1]], label=labels[counter]))
      counter <- counter + 1
    }
  }

  ggplot(gdata, aes(x=as.Date(date), y=value, color=label))+geom_line()
}


gg.bar <- function(x, labels) {
  if (missing(labels))
    labels <- names(x)[names(x)!='date']
  
  gdata <- NULL
  
  dates <- x[[1]]
  for (i in 1:(ncol(x)-1))
    gdata <- rbind(gdata, data.table(date=dates, value=x[[i+1]], label=labels[i]))
  
  ggplot(gdata, aes(x=as.Date(date), y=value, fill=label))+geom_bar(stat="identity", position="dodge")
}

ggplotdef <- function(...) ggplot(...)+xlab("")+ylab("")

ggsave.halfwidth <- function(filename) {
  ggsave(filename, width=3.5, height=3.5)
}

ggsave.fullwidth <- function(filename) {
  ggsave(filename, width=7, height=4)
}

rescale <- function(x, min.val, max.val) {
  numcols <- which(sapply(x, class)=='numeric')
  if (missing(min.val) && missing(max.val)) {
    min.val <- min(x[, numcols, with=T], na.rm=T)
    max.val <- max(x[, numcols, with=T], na.rm=T)
  }
  
  for (i in numcols) {
    a <- min(x[[i]], na.rm=T)
    b <- max(x[[i]], na.rm=T)
    x[[i]] <- (max.val-min.val)*(x[[i]]-a)/(b-a) + min.val
  }
  
  x
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ggplot2 theme
mytheme <- theme_bw()+theme(
  legend.title=element_blank()
  )

theme_set(mytheme)