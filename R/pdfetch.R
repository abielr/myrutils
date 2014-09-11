pdf_GENERIC <- function(x) {
  dates <- index(x)
  x <- data.table(as.data.frame(x))
  x <- cbind(date=dates, x)
  x
}

pdf_FRED <- function(...) {
  pdf_GENERIC(pdfetch_FRED(...))
}