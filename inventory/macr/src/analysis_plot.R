plotArray <- function(y, indices, predictions, totals, ylim = NULL, plotNames = TRUE, plotAxes = TRUE) {
  predictionsAreMissing <- missing(predictions)
  totalsAreMissing <- missing(totals)
  
  outputFormat <- rmdGetOutputFormat()
  
  ratio <- if (outputFormat == "latex") 8.5 / 11 else 1.6
  
  scaleToYAxis <- function(x, xRange, yRange) diff(yRange) * (x - xRange[1L]) / diff(xRange) + yRange[1L]
  
  years <- as.integer(colnames(y))
  
  mar <- if (plotAxes) c(1.05, 1.05, 0,  if (!totalsAreMissing) 1.15 else 0) else c(0.05, 0.05, 0, 0)
  if (plotNames) mar[3L] <- 1.2
  
  par(mfrow = getGridDim(ratio, length(indices)),
      mar = mar,
      mgp = c(0.8, 0.2, 0))
  
  for (j in seq_along(indices)) {
    y_j <- y[indices[j],]
    plotYears <- if (!totalsAreMissing) totals[indices[j],] > 0 else y_j > 0
    
    if (!predictionsAreMissing) {
      if (is.list(predictions)) {
        predictions_j <- predictions[[j]]
        pred <- apply(predictions_j, 1L, mean)
        limits <- apply(predictions_j, 1L, quantile, c(0.025, 0.975), na.rm = TRUE)
      } else {
        pred <- predictions[j,,1L]
        limits <- t(predictions[j,,c(2L, 3L)])
      }
    }
    
    if (is.null(ylim)) {
      if (!predictionsAreMissing) {
        ylim_j <- range(limits[,plotYears], y_j)
      } else {
        ylim_j <- range(y_j)
      }
    } else {
      ylim_j <- ylim
    }
    plot(NULL, type = "n", xlim = range(years), ylim = ylim_j,
         xlab = "", ylab = "",
         bty = if (plotAxes) "L" else "n",
         xaxt = if (plotAxes) "s" else "n",
         yaxt = if (plotAxes) "s" else "n")
    
    if (!predictionsAreMissing) {
      polygon(c(years[plotYears], rev(years[plotYears])), c(limits[1L,plotYears], rev(limits[2L,plotYears])), col = rgb(0.95, 0.95, 0.95), border = "NA")
      lines(years, pred, col = "gray", lwd = 1.5)
    }
    if (plotAxes) {
      axis(1L, lwd = 0.7)
      axis(2L, lwd = 0.7)
    }
    if (!totalsAreMissing) {
      t_j <- totals[indices[j],]
      lines(years, scaleToYAxis(t_j, range(t_j), ylim_j), lty = 2)
      if (plotAxes) {
        tickLabels <- axTicks(4L, axp = c(range(t_j), 5))
        tickLocations <- scaleToYAxis(tickLabels, range(t_j), ylim_j)
        axis(4L, at = tickLocations, labels = tickLabels)
      }
    }
    if (plotNames)
      title(getNameForJurisdiction(rownames(y)[indices[j]]), line = 0)
    
    lines(years, y_j, lwd = 0.8)
  }
  
  invisible(NULL)
}
