txtFiles <- c(
  file.path("..", txtPath, "analysis_arrestNum_1.txt"),
  file.path("..", txtPath, "analysis_arrestNum_2.txt"))
imgFiles <- c(
  file.path("..", imgPath, "analysis_arrestNumHistogram.pdf"),
  file.path("..", imgPath, "analysis_numArrestsZeroCounts.pdf"))

if (all(file.exists(txtFiles)) && all(file.exists(imgFiles))) stop("all files exist")

macr <- loadData()

counts <- table(macr[,c("ncic_jurisdiction", "arrest_year")])
  
plotCountsArray <- function(counts, indices, plotNames = TRUE, plotAxes = TRUE) {
  outputFormat <- rmdGetOutputFormat()
  
  ratio <- if (outputFormat == "latex") 8.5 / 11 else 1.6
  
  years <- as.integer(colnames(counts))
  
  mar <- if (plotAxes) c(1.05, 1.05, 0, 0) else c(0.05, 0.05, 0, 0)
  if (plotNames) mar[3L] <- 1.2
  
  par(mfrow = getGridDim(ratio, length(indices)),
      mar = mar,
      mgp = c(0.8, 0.2, 0))
  
  for (i in seq_along(indices)) {
    plot(NULL, type = "n", xlim = range(years), ylim = range(counts[indices[i],]),
         xlab = "", ylab = "",
         bty = if (plotAxes) "L" else "n",
         xaxt = if (plotAxes) "s" else "n",
         yaxt = if (plotAxes) "s" else "n")
    if (plotAxes) {
      axis(1L, lwd = 0.7)
      axis(2L, lwd = 0.7)
    }
    if (plotNames)
      title(getNameForJurisdiction(rownames(counts)[indices[i]]), line = 0)
    
    lines(years, counts[indices[i],], lwd = 0.8)
  }
  
  invisible(NULL)
}

numJurisdictions <- nlevels(macr$ncic_jurisdiction)
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

pdf(file.path("..", imgPath, "analysis_numArrestsZeroCounts.pdf"), 6, 6 * widthToHeightRatio)
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
    y[keep] <- (y[keep] - mean(y[keep])) / sd(y[keep])
    y[!keep] <- 0.0
    y
  }),
  medianCounts = apply(counts.fit, 1L, function(row) median(row[!is.na(row)])),
  beta_eta_mean = c(7.0, 0.5),
  beta_eta_scale = c(12.0, 7.5),
  eta_scale = 2.0,
  mu_beta_y = c(0, 0),
  Sigma_beta_y = diag(5.0, 2))

data$medianCounts <- with(data, (medianCounts - mean(medianCounts)) / sd(medianCounts))

require(rstan)
model <- stan_model(file.path(srcPath, "numArrests.stan"))

samples <- sampling(model, data = data)
pars <- extract(samples)


postmean <- function(samples, indices) {
  time <- seq_len(36L)
  time <- (time - mean(time)) / sd(time)
  
  #sapply(indices, function(index) {
  #  apply(samples$beta_y[indices[index],1] + samples$beta_y[indices[index],2] * time[seq_len(longestRun)], 2, mean)
}


gamma.mean <- apply(pars$gamma, 2L, mean)
gamma.order <- order(gamma.mean)

pdf(file.path("..", imgPath, "analysis_numArrestsLowAR.pdf"), 6, 6 * widthToHeightRatio)
plotCountsArray(counts, which(fitJurisdictions)[gamma.order[seq_len(8L)]])
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsHighAR.pdf"), 6, 6 * widthToHeightRatio)
plotCountsArray(counts, which(fitJurisdictions)[gamma.order[seq.int(length(gamma.order), length(gamma.order) - 8L + 1L)]])
dev.off()

eta.mean <- apply(pars$eta, 2L, mean)
eta.order <- order(eta.mean)

pdf(file.path("..", imgPath, "analysis_numArrestsLowVar.pdf"), 6, 6 * widthToHeightRatio)
plotCountsArray(counts, which(fitJurisdictions)[eta.order[seq_len(8L)]])
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsHighVar.pdf"), 6, 6 * widthToHeightRatio)
plotCountsArray(counts, which(fitJurisdictions)[eta.order[seq.int(length(eta.order), length(eta.order) - 8L + 1L)]])
dev.off()

macr.sub <- subset(macr, disposition == "released", c("ncic_jurisdiction", "arrest_year"))
macr.sub$ncic_jurisdiction <- droplevels(macr.sub$ncic_jurisdiction)
numArrested <- table(macr.sub)

counts.fit <- t(sapply(which(fitJurisdictions), function(index)
  c(counts[index, seq.int(longestRuns[index,1L], longestRuns[index,2L])], rep(NA_integer_, numYears - runLengths[index]))))


numArrested.fit <- 

data <- list(
  numGroups = nrow(counts.fit),
  numYears  = ncol(counts.fit),
  numNonZeroYears = apply(counts.fit, 1L, function(row) sum(!is.na(row))),
  numReleased = table(macr.sub),
  numArrested = t(counts[fitJurisdictions,]))

model <- stan_model(file.path(srcPath, "releasedProportions.stan"))

samples <- sampling(model, data = data)
  
