getGridDim <- function(widthToHeight, n) {
  rows <- sqrt(n / widthToHeight)
  cols <- widthToHeight * rows
  
  result <- matrix(c(
    floor(rows), floor(cols),
    floor(rows), ceiling(cols),
    ceiling(rows), floor(cols),
    ceiling(rows), ceiling(cols)), 2L)
  
  sizes <- apply(result, 2L, prod)
  validResults <- which(sizes >= n)
  
  result <- result[,validResults]
  sizes  <- sizes[validResults]
  
  if (length(sizes) == 1L) result else result[,which.min(sizes)]
}

plotBookingRateGrid <- function(rates, sizes = NULL, ratio = 1.6) {
  ratesDim <- dim(rates)

  numJurisdictions <- dim(rates)[1L]
  if (length(ratesDim) == 2L) {
    numOffenseCodes <- 1L
    numYears        <- dim(rates)[2L]
    years <- as.integer(dimnames(rates)[[2L]])
  } else {
    numOffenseCodes <- dim(rates)[2L]
    numYears        <- dim(rates)[3L]
    years <- as.integer(dimnames(rates)[[3L]])
  }
  
  xVals <- rep(c(years, NA), numOffenseCodes)
  
  par(mfrow = getGridDim(ratio, numJurisdictions), mar = c(0.05, 0.05, 0.05, 0.05))
  for (i in seq_len(numJurisdictions)) {
    yVals <- if (length(ratesDim) == 2L) c(rates[i,], NA) else rbind(t(rates[i,,]), NA)
    plot(xVals, yVals, type = "l",
         xaxt = "n", yaxt = "n", bty = "n",
         xaxs = "i", yaxs = "i",
         lwd = 0.75, xlab = "", ylab = "", main = "",
         xlim = range(xVals, na.rm = TRUE), ylim = c(0, 1))
  }
  invisible(NULL)
}
