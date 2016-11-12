txtFiles <- c(
  file.path("..", txtPath, "analysis_arrestNum_1.txt"),
  file.path("..", txtPath, "analysis_arrestNum_2.txt"))
imgFiles <- c(
  file.path("..", imgPath, "analysis_arrestNumHistogram.pdf"),
  file.path("..", imgPath, "analysis_numArrestsZeroCounts.pdf"))

if (all(file.exists(txtFiles)) && all(file.exists(imgFiles))) stop("all files exist")

macr <- loadData(path = "..")

counts <- table(macr[,c("ncic_jurisdiction", "arrest_year")])
  
plotCountsArray <- function(counts, indices, posteriorPredictions, plotNames = TRUE, plotAxes = TRUE) {
  posteriorPredictionsAreMissing <- missing(posteriorPredictions)
  
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
    
    if (!posteriorPredictionsAreMissing) {
      postPred <- posteriorPredictions[[i]]
      postMean <- apply(postPred, 1L, mean)
      
      limits <- apply(postPred, 1L, quantile, c(0.025, 0.975))
      polygon(c(years, rev(years)), c(limits[1,], rev(limits[2,])), col = rgb(0.95, 0.95, 0.95), border = "NA")
      lines(years, postMean, col = "gray", lwd = 1.5)
    }
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
  
minLargeJurisdictions <- totalCounts[counts.order[which.max(percentage >= 0.05) - 1L]]

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

counts.fit <- t(sapply(which(fitJurisdictions), function(index) {
  row <- counts[index,]
  if (longestRuns[index,1L] > 1L)       row[seq.int(1L, longestRuns[index,1L])]       <- NA_integer_
  if (longestRuns[index,2L] < numYears) row[seq.int(longestRuns[index,2L], numYears)] <- NA_integer_
  row
}))

standardize <- function(x) (x - mean(x)) / sd(x)

getTransformationsForMatrix <- function(x) {
  pars <- sapply(seq_len(ncol(x)), function(j) {
    u <- unique(x[,j])
    if (length(u) == 1L) return(c(0, 1))
    c(mean(x[,j], na.rm = TRUE), sd(x[,j], na.rm = TRUE))
  })
  forward <- function(x)
    sapply(seq_len(ncol(x)), function(j) (x[,j] - mu[j]) / sigma[j])
  backward <- function(x)
    sapply(seq_len(ncol(x)), function(j) sigma[j] * x[,j] + mu[j])
  
  env <- new.env(parent = baseenv())
  env$mu <- pars[1,]
  env$sigma <- pars[2,]
  
  environment(forward) <- env
  environment(backward) <- env
  
  list(forward = forward, backward = backward)
}

x_y <- cbind(1, seq_len(numYears), seq_len(numYears)^2)
x_j <- cbind(1, apply(counts.fit, 1L, function(row) median(log(row[!is.na(row)]))))

x_y.trans <- getTransformationsForMatrix(x_y)
x_j.trans <- getTransformationsForMatrix(x_j)

x_y.z <- x_y.trans$forward(x_y)
x_j.z <- x_j.trans$forward(x_j)

y.log <- log(counts.fit)
y.pars <- t(sapply(seq_len(nrow(y.log)), function(i) {
    c(mean(y.log[i,], na.rm = TRUE), sd(y.log[i,], na.rm = TRUE))
}))
colnames(y.pars) <- c("mu", "sigma")

data <- list(
  J = nrow(counts.fit),
  T = ncol(counts.fit),
  
  start_j = longestRuns[fitJurisdictions,1L],
  end_j   = longestRuns[fitJurisdictions,2L],
  
  P_y = ncol(x_y),
  P_j = ncol(x_j),
  
  y = apply(counts.fit, 1L, function(row) {
    keep <- !is.na(row)
    y <- ifelse(keep, log(row), 0.0)
    y[keep] <- standardize(y[keep])
    y
  }),
  x_y = x_y.z,
  x_j = x_j.z,
  
  nu_theta = 3.0,
  mu_theta = rep(0.0, 4L),
  Sigma_theta = diag(c(5, rep(2.5, 3L))))

require(rstan)
model <- stan_model(file.path(srcPath, "numArrests.stan"))

samples <- sampling(model, data = data)
pars <- extract(samples)

## 384 x 4000
sigma <- exp(x_j.z %*% t(pars$beta_j) + t(pars$theta[,,4L]))

getPosteriorPredictions <- function(samples, indices) {
  lapply(indices, function(index) {
    exp(y.pars[index,"mu"] + y.pars[index,"sigma"] * x_y.z %*% t(pars$theta[,index,seq_len(3L)]))
  })
}

eta.mean <- apply(pars$theta[,,4], 2L, mean)
eta.order <- order(eta.mean)

pdf(file.path("..", imgPath, "analysis_numArrestsLowVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsHighVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq.int(length(eta.order), length(eta.order) - 8L + 1L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))
dev.off()

## fitted on standardized log scale
residuals <- t(sapply(seq_len(nrow(counts.fit)), function(j) {
  fitted <- x_y.z %*% t(pars$theta[,j,seq_len(3L)])
  ## fitted is 36 x 4000, transposed makes each column 4000 long, so sigma will get recycled in the
  ## division
  result <- t(t(data$y[,j] - fitted) / sigma[j,])
  result[is.na(counts.fit[j,]),] <- NA
  
  apply(result, 1L, mean, na.rm = TRUE)
}))

maxResiduals <- apply(residuals, 1L, function(col) max(abs(col), na.rm = TRUE))
residual.order <- order(maxResiduals)

# smallest residuals not interesting, for reasons explained in Rmd
#plotIndices <- residual.order[seq_len(8L)]
#plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))

pdf(file.path("..", imgPath, "analysis_numArrestsMaxResiduals.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- residual.order[seq.int(length(residual.order), length(residual.order) - 8L + 1L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))
dev.off()


largestJumps <- t(sapply(seq_len(nrow(counts.fit)), function(i) {
  y <- counts.fit[i,]
  y <- y[!is.na(y)]
  m <- median(y)
  
  pct <- y / m
  
  c(min(pct), max(pct))
}))
down.order <- order(largestJumps[,1L])
up.order  <- order(largestJumps[,2L], decreasing = TRUE)

pdf(file.path("..", imgPath, "analysis_numArrestsDownJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- down.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsUpJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- up.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(samples, plotIndices))
dev.off()

macr.sub <- subset(macr, disposition == "released", c("ncic_jurisdiction", "arrest_year"))
macr.sub$ncic_jurisdiction <- droplevels(macr.sub$ncic_jurisdiction)
numArrested <- table(macr.sub)

counts.fit <- t(sapply(which(fitJurisdictions), function(index)
  c(counts[index, seq.int(longestRuns[index,1L], longestRuns[index,2L])], rep(NA_integer_, numYears - runLengths[index]))))

data <- list(
  numGroups = nrow(counts.fit),
  numYears  = ncol(counts.fit),
  numNonZeroYears = apply(counts.fit, 1L, function(row) sum(!is.na(row))),
  numReleased = table(macr.sub),
  numArrested = t(counts[fitJurisdictions,]))

#model <- stan_model(file.path(srcPath, "releasedProportions.stan"))

#samples <- sampling(model, data = data)
  
