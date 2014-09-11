# Lags
L <- function(x, k=1) {
  ret <- matrix(NA, nrow=length(x), ncol=length(k))
  embedded <- embed(x, max(k)+1)
  lags <- embedded[, k+1]
  if (!is.matrix(lags))
    lags <- matrix(lags)
  ret[(nrow(ret)-nrow(lags)+1):nrow(ret), ] <- lags
  ret
}

# Percent change
PC <- function(x, k=1, periodicity=NULL) {
  if (is.null(periodicity))
    x/L(x,k)-1
  else
    (x/L(x,k))^(periodicity/k)-1
}

# Difference
D <- function(x, k=1) {
  x-L(x,k)
}

# Log difference
LD <- function(x, k=1, periodicity=NULL) {
  if (is.null(periodicity))
    log(x)-log(L(x,k))
  else
    periodicity/k*(log(x)-log(L(x,k)))
}

# Moving average
MA <- function(x, k=2, sides=1) {
  as.vector(filter(x, rep(1/k, k), sides=sides))
}

# Convert time series frequency
convert <- function(x, freq, method, ...) {
  if (!"date" %in% names(x))
    stop("data must have column 'date'")
  freq <- toupper(freq)
  
  datacols <- setdiff(names(x), "date")
  
  if (freq=='Q')
    ret <- x[, lapply(.SD, sum), by=quarter_end(date), .SDcols=datacols]
  else
    stop("Unrecognized frequency")
  
  ret
}

# Return end of annual date
year_end <- function(date) {
  as.Date(ISOdate(year(date),12,31))
}

# Return end of semiannual date
halfyear_end <- function(date) {
  day_lookup <- c(30,30,30,30,30,30,31,31,31,31,31,31)
  months <- (floor((month(date)-1)/6)+1)*6
  days <- day_lookup[months]
  as.Date(ISOdate(year(date),months,days))
}

# Return end of quarter date
quarter_end <- function(date) {
  day_lookup <- c(31,31,31,30,30,30,30,30,30,31,31,31)
  months <- (floor((month(date)-1)/3)+1)*3
  days <- day_lookup[months]
  as.Date(ISOdate(year(date),months,days))
}

# Return end of month date
month_end <- function(date) {
  day(date) <- days_in_month(date)
  date
}