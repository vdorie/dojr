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
  
  for (j in seq_along(indices)) {
    y_j <- counts[indices[j],]
    plotYears <- y_j > 0
    
    if (!posteriorPredictionsAreMissing) {
      postPred <- posteriorPredictions[[j]]
      postMean <- apply(postPred, 1L, mean)
      
      limits <- apply(postPred, 1L, quantile, c(0.025, 0.975))
      ylim <- range(limits[,plotYears], y_j)
    } else {
      ylim <- range(y_j)
    }
    plot(NULL, type = "n", xlim = range(years), ylim = ylim,
         xlab = "", ylab = "",
         bty = if (plotAxes) "L" else "n",
         xaxt = if (plotAxes) "s" else "n",
         yaxt = if (plotAxes) "s" else "n")
    
    if (!posteriorPredictionsAreMissing) {
      polygon(c(years, rev(years)), c(limits[1L,], rev(limits[2L,])), col = rgb(0.95, 0.95, 0.95), border = "NA")
      lines(years, postMean, col = "gray", lwd = 1.5)
    }
    if (plotAxes) {
      axis(1L, lwd = 0.7)
      axis(2L, lwd = 0.7)
    }
    if (plotNames)
      title(getNameForJurisdiction(rownames(counts)[indices[j]]), line = 0)
    
    lines(years, y_j, lwd = 0.8)
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

if (FALSE) counts.fit <- t(sapply(which(fitJurisdictions), function(index) {
  row <- counts[index,]
  if (longestRuns[index,1L] > 1L)       row[seq.int(1L, longestRuns[index,1L])]       <- NA_integer_
  if (longestRuns[index,2L] < numYears) row[seq.int(longestRuns[index,2L], numYears)] <- NA_integer_
  row
}))

counts.fit <- t(sapply(which(fitJurisdictions), function(index) {
  row <- counts[index,]
  row[row == 0] <- NA_integer_
  row
}))

indices.fit <- t(sapply(seq_len(nrow(counts.fit)), function(j) {
  res <- which(!is.na(counts.fit[j,]))
  res <- c(res, integer(ncol(counts.fit) - length(res)))
  res
}))
numIndices.fit <- sapply(seq_len(nrow(counts.fit)), function(j) {
  sum(indices.fit[j,] > 0L)
})


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

x <- cbind(1, seq_len(numYears), seq_len(numYears)^2, seq_len(numYears)^3)
x_j <- x[,1:3]
x_sigma <- cbind(1, apply(counts.fit, 1L, function(row) median(log(row[!is.na(row)]))))

x.trans <- getTransformationsForMatrix(x)
x_j.trans <- getTransformationsForMatrix(x_j)
x_sigma.trans <- getTransformationsForMatrix(x_sigma)

x.z <- x.trans$forward(x)
x_j.z <- x_j.trans$forward(x_j)
x_sigma.z <- x_sigma.trans$forward(x_sigma)

y.log <- log(counts.fit)
y.pars <- t(sapply(seq_len(nrow(y.log)), function(i) {
    c(mean(y.log[i,], na.rm = TRUE), sd(y.log[i,], na.rm = TRUE))
}))
colnames(y.pars) <- c("mu", "sigma")

data <- list(
  J = nrow(counts.fit),
  T = ncol(counts.fit),
  
  indices = indices.fit,
  numIndices = numIndices.fit,
  
  P = ncol(x),
  P_j = ncol(x_j),
  P_sigma = ncol(x_sigma),
  
  y = apply(counts.fit, 1L, function(row) {
    keep <- !is.na(row)
    y <- ifelse(keep, log(row), 0.0)
    y[keep] <- standardize(y[keep])
    y
  }),
  x = x.z,
  x_j = x_j.z,
  x_sigma = x_sigma.z,
  w = sqrt(exp(x_sigma[,2]) / sum(exp(x_sigma[,2]))),
  
  # prior on population fixed effects
  nu_beta = 3.0,
  mu_beta = rep(0.0, ncol(x)),
  sigma_beta = c(5, rep(2.5, ncol(x) - 1L)),
  
  # prior on residual variance fixed effects
  nu_beta_sigma = 3.0,
  mu_beta_sigma = rep(0.0, ncol(x_sigma)),
  sigma_beta_sigma = c(5, rep(2.5, ncol(x_sigma) - 1L)),
  
  # prior on random effects covariance
  nu_L_sigma_theta = 3.0,
  sigma_L_sigma_theta = 2.5,
  nu_L_Omega_theta = 4.0
)

require(rstan)
model <- stan_model(file.path(srcPath, "numArrests.stan"))

samples <- sampling(model, data = data)
pars <- extract(samples)

## 4000 x 384
pars$sigma <- exp(tcrossprod(pars$beta_sigma, x_sigma.z) + pars$theta[,,ncol(x_j) + 1L])

getPosteriorLinearPredictor <- function(samples, index)
  tcrossprod(x.z, pars$beta) / data$w[index] + tcrossprod(x_j.z, pars$theta[,index,seq_len(ncol(x_j))])

getPosteriorMeans <- function(samples, indices) {
  lapply(indices, function(index) {
    mu <- getPosteriorLinearPredictor(samples, index)

    exp(y.pars[index,"mu"] + y.pars[index,"sigma"] * mu)
  })
}
getPosteriorPredictions <- function(pars, indices) {
  lapply(indices, function(index) {
    mu <- getPosteriorLinearPredictor(samples, index)
    sigma <- pars$sigma[,j]
    
    exp(y.pars[index,"mu"] + y.pars[index,"sigma"] * matrix(rnorm(length(mu), mu, sigma), nrow(mu)))
  })
}

eta.mean <- apply(pars$theta[,,ncol(x_j) + 1L], 2L, mean)
eta.order <- order(eta.mean)

pdf(file.path("..", imgPath, "analysis_numArrestsLowVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsHighVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq.int(length(eta.order), length(eta.order) - 8L + 1L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))
dev.off()

## fitted on standardized log scale
residuals <- t(sapply(seq_len(nrow(counts.fit)), function(j) {
  mu <- getPosteriorLinearPredictor(samples, j)
  ## fitted is 36 x 4000, transposed makes each column 4000 long, so sigma will get recycled in the
  ## division
  result <- t(t(data$y[,j] - mu) / pars$sigma[,j])
  result[is.na(counts.fit[j,]),] <- NA
  
  apply(result, 1L, mean, na.rm = TRUE)
}))

maxResiduals <- apply(residuals, 1L, function(col) max(abs(col), na.rm = TRUE))
residual.order <- order(maxResiduals)

# smallest residuals not interesting, for reasons explained in Rmd
#plotIndices <- residual.order[seq_len(8L)]
#plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))

pdf(file.path("..", imgPath, "analysis_numArrestsMaxResiduals.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- residual.order[seq.int(length(residual.order), length(residual.order) - 8L + 1L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))
dev.off()


largestJumps <- t(sapply(seq_len(nrow(counts.fit)), function(j) {
  y <- counts.fit[j,]
  y <- y[!is.na(y)]
  
  pct <- y[-1L] / y[-length(y)]
  
  c(min(pct), max(pct))
}))
down.order <- order(largestJumps[,1L])
up.order  <- order(largestJumps[,2L], decreasing = TRUE)

pdf(file.path("..", imgPath, "analysis_numArrestsDownJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- down.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsUpJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- up.order[seq_len(8L)]
plotCountsArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))
dev.off()

#macr.sub <- subset(macr, c("ncic_jurisdiction", "arrest_year", "disposisition"))
#macr.sub$ncic_jurisdiction <- droplevels(macr.sub$ncic_jurisdiction)
#numArrested <- table(macr.sub)
#rm(macr.sub)

tab <- table(macr[,c("ncic_jurisdiction", "arrest_year", "disposition")])

totals.fit <- apply(tab[fitJurisdictions,,], c(1L, 2L), sum)
numReleased.fit <- apply(tab[fitJurisdictions,,], c(1L, 2L), function(x) x["released"])

indices.fit <- t(sapply(seq_len(nrow(totals.fit)), function(j) {
  res <- which(totals.fit[j,] > 0L)
  res <- c(res, integer(ncol(totals.fit) - length(res)))
  res
}))
numIndices.fit <- sapply(seq_len(nrow(totals.fit)), function(j) {
  sum(indices.fit[j,] > 0L)
})


data <- list(
  J = nrow(numReleased.fit),
  T = ncol(numReleased.fit),
  
  P = ncol(x),
  P_j = ncol(x_j),
  
  indices = indices.fit,
  numIndices = numIndices.fit,
  
  
  n = t(totals.fit),
  y = t(numReleased.fit),
  
  x = x.z,
  x_j = x_j.z,
  
  
  # prior on population fixed effects
  nu_beta = 3.0,
  mu_beta = rep(0.0, ncol(x)),
  sigma_beta = c(5, rep(2.5, ncol(x) - 1L)),
  
  # prior on random effects covariance
  nu_L_sigma_beta_j = 3.0,
  sigma_L_sigma_beta_j = 2.5,
  nu_L_Omega_beta_j = 4.0)

model2 <- stan_model(file.path(srcPath, "releasedProportions.stan"))

samples2 <- sampling(model2, data = data)


getPosteriorLinearPredictor <- function(samples, index)
  tcrossprod(x.z, pars$beta) + tcrossprod(x_j.z, pars$theta[,index,seq_len(ncol(x_j))])
