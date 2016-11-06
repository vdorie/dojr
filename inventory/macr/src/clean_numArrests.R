txtFiles <- c(
  file.path("..", txtPath, "clean_arrestNum_1.txt"),
  file.path("..", txtPath, "clean_arrestNum_2.txt"))
imgFiles <- c(
  file.path("..", imgPath, "clean_arrestNumHistogram.pdf"),
  file.path("..", imgPath, "clean_numArrestsZeroCounts.pdf"))

if (any(!file.exists(txtFiles)) || any(!file.exists(imgFiles))) error("all files exist")

counts <- table(macr.clean[,c("ncic_jurisdiction", "arrest_year")])
  
plotCountsArray <- function(counts, indices, plotAxes = TRUE) {
  outputFormat <- rmdGetOutputFormat()
  
  ratio <- if (outputFormat == "latex") 8.5 / 11 else 1.6
  
  par(mfrow = getGridDim(ratio, length(indices)),
      mar = if (plotAxes) c(1, 1, 0, 0) else c(0.05, 0.05, 0, 0),
      mgp = c(0.8, 0.2, 0))
  
  for (i in seq_along(indices)) {
    plot(NULL, type = "n", xlim = c(1L, ncol(counts)), ylim = range(counts[indices[i],]),
         xlab = "", ylab = "",
         bty = if (plotAxes) "L" else "n",
         xaxt = if (plotAxes) "s" else "n",
         yaxt = if (plotAxes) "s" else "n")
    if (plotAxes) {
      axis(1L, lwd = 0.7)
      axis(2L, lwd = 0.7)
    }
    lines(seq_len(ncol(counts)), counts[indices[i],], lwd = 0.8)
  }
  
  invisible(NULL)
}

numJurisdictions <- nlevels(macr.clean$ncic_jurisdiction)
numYears <- ncol(counts)
totalCounts <- rowSums(counts)
minCount <- min(totalCounts)
maxCount <- max(totalCounts)

counts.order <- order(totalCounts)
counts.order.inv <- counts.order; counts.order.inv[counts.order] <- seq_along(counts.order)

percentage <- cumsum(totalCounts[counts.order]) / sum(totalCounts)
  
minLargeJurisdictions <- totalCounts[totalCount.order[which.max(percentage >= 0.05) - 1L]]

largeJurisdictions <- totalCounts >= minLargeJurisdictions
numLargeJurisdictions <- sum(largeJurisdictions)


pdf(imgFiles[1L], 3, 3)
hist(totalCounts, breaks = 20, xlab = "count", main = "Num Arrests by Jurisdiction", freq = TRUE)
dev.off()
  
    
writeLines(as.character(c(numJurisdictions, minCount, maxCount, numYears, minLargeJurisdictions, numLargeJurisdictions)),
           txtFiles[1L])

longestRuns <- t(apply(counts, 1L, function(row) {
  if (all(row > 0L)) return(c(1L, length(row)))
  
  zeroIndices <- which(row == 0L)
  if (row[length(row)] > 0L) zeroIndices <- c(zeroIndices, length(row) + 1L)
  runLengths  <- c(zeroIndices[1L], diff(zeroIndices)) - 1L
  
  longestRun <- which.max(runLengths)
  runStart <- zeroIndices[longestRun] - runLengths[longestRun]
  runEnd   <- runStart + runLengths[longestRun] - 1L
  
  c(runStart, runEnd)
}))
runLengths <- longestRuns[,2L] - longestRuns[,1L] + 1L

numRuns <- apply(counts, 1L, function(row) {
  if (all(row > 0L)) return(1L)
  
  zeroIndices <- which(row == 0L)
  if (row[length(row)] > 0L) zeroIndices <- c(zeroIndices, length(row) + 1L)
  runLengths  <- c(zeroIndices[1L], diff(zeroIndices)) - 1L
  
  length(runLengths[runLengths > 0L])
})

zerosInMiddle <- largeJurisdictions & numRuns >= 2L

widthToHeightRatio <- if (rmdGetOutputFormat() == "latex") 8.5 / 11 else 1.6

numZerosInMiddle <- sum(zerosInMiddle)

pdf(file.path("..", imgPath, "clean_numArrestsZeroCounts.pdf"), 6, 6 * widthToHeightRatio)
plotCountsArray(counts, which(zerosInMiddle))
dev.off()

writeLines(as.character(numZerosInMiddle), txtFiles[2L])


shortJurisdictions <- largeJurisdictions & !zerosInMiddle & runLengths < numYears %/% 3L

numShortJurisdictions <- sum(shortJurisdictions)

fitJurisdictions <- largeJurisdictions & !zerosInMiddle & !shortJurisdictions

counts.fit <- t(sapply(which(fitJurisdictions), function(index)
  c(counts[index, seq.int(longestRuns[index,1L], longestRuns[index,2L])], rep(NA_integer_, numYears - runLengths[index]))))


data <- list(
  numGroups = nrow(counts.fit),
  numYears  = ncol(counts.fit),
  
  numNonZeroYears = apply(counts.fit, 1L, function(row) sum(!is.na(row))),
  
  logCounts = apply(counts.fit, 1L, function(row) {
    keep <- !is.na(row)
    y <- ifelse(keep, log(row), 0.0)
    y[keep] <- (y[keep] - y[1L]) / sd(y[keep])
    y[!keep] <- 0.0
    y
  }),
  medianCounts = apply(counts.fit, 1L, function(row) median(row[!is.na(row)])))
data$medianCounts <- with(data, (medianCounts - mean(medianCounts)) / sd(medianCounts))

require(rstan)
model <- stan_model(file.path(srcPath, "numArrests.stan"))

samples <- sampling(model, data = data)

pars <- extract(samples)
