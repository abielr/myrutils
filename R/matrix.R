repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}

# Split a vector into chunks
splitvec <- function(data, n, len) {
  if (missing(len))
    len <- ceiling(length(data)/n)
  breaks <- seq(1, length(data), by=len)
  ix <- c(0, breaks[-1]-1, length(data))
  
  ret <- list()
  for (i in 1:(length(ix)-1))
    ret[[i]] <- data[(ix[i]+1):(ix[i+1])]
  
  ret
}