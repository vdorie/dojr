txtFiles <- c(
  file.path("..", txtPath, "analysis_arrestNum_1.txt"),
  file.path("..", txtPath, "analysis_arrestNum_2.txt"))
imgFiles <- c(
  file.path("..", imgPath, "analysis_arrestNumHistogram.pdf"),
  file.path("..", imgPath, "analysis_numArrestsZeroCounts.pdf"))

if (all(file.exists(txtFiles)) && all(file.exists(imgFiles))) {
  stop(simpleMessage("all files exist"))

if (require(rstan, quietly = TRUE) == FALSE) {
  repos <- getOption("repos", default = "https://cran.cnr.berkeley.edu/")
  install.packages("rstan", repos = repos, dependencies = TRUE)
  
  if (require(rstan, quietly = TRUE) == FALSE) stop("cannot load rstan")
}

source(file.path("..", dataSrcPath, "analysis_util.R"))
source(file.path("..", dataSrcPath, "analysis_numArrests_plot.R"))


macr <- loadData(path = "..")
invisible(loadData("jurisdictions", path = ".."))

numArrestsTable <- table(macr[,c("ncic_jurisdiction", "arrest_year", "disposition")])

numArrestsByJurisYear  <- apply(numArrestsTable, c(1L, 2L), sum)
numReleasedByJurisYear <- apply(numArrestsTable, c(1L, 2L), function(x) x["released"])



numJurisdictions <- nlevels(macr$ncic_jurisdiction)
numYears <- dim(numArrestsTable)[2L]
numArrestsByJuris <- apply(numArrestsByJurisYear, 1L, sum)
minNumArrests <- min(numArrestsByJuris)
maxNumArrests <- max(numArrestsByJuris)

numArrestsByJuris.order <- order(numArrestsByJuris)
numArrestsByJuris.order.inv <- numArrestsByJuris.order
numArrestsByJuris.order.inv[numArrestsByJuris.order] <- seq_along(numArrestsByJuris.order)

percentage <- cumsum(numArrestsByJuris[numArrestsByJuris.order]) / sum(numArrestsByJuris)
  
minLargeJurisdictions <- numArrestsByJuris[numArrestsByJuris.order[which.max(percentage >= 0.05) - 1L]]

largeJurisdictions <- numArrestsByJuris >= minLargeJurisdictions
numLargeJurisdictions <- sum(largeJurisdictions)


pdf(imgFiles[1L], 6, 3)
par(mfrow = c(1L, 2L))
hist(numArrestsByJuris, breaks = 20, xlab = "count", main = "Num Arrests by Jurisdiction", freq = TRUE)
barplot(apply(numArrestsByJurisYear, 2L, sum), border = "NA", col = "gray", main = "Num Arrests by Year",
        xlab = "Year", ylab = "Num Arrests")
dev.off()

writeLines(as.character(c(numJurisdictions, minNumArrests, maxNumArrests, numYears, minLargeJurisdictions, numLargeJurisdictions)),
           txtFiles[1L])

longestRunIndices <- t(apply(numArrestsByJurisYear, 1L, function(row) {
  if (all(row > 0L)) return(c(1L, length(row)))
  
  zeroIndices <- which(row == 0L)
  if (row[length(row)] > 0L) zeroIndices <- c(zeroIndices, length(row) + 1L)
  runLengths  <- c(zeroIndices[1L], diff(zeroIndices)) - 1L
  
  longestRun <- which.max(runLengths)
  runStart <- zeroIndices[longestRun] - runLengths[longestRun]
  runEnd   <- runStart + runLengths[longestRun] - 1L
  
  c(runStart, runEnd)
}))
longestRunLengths <- longestRunIndices[,2L] - longestRunIndices[,1L] + 1L

numRuns <- apply(numArrestsByJurisYear, 1L, function(row) {
  if (all(row > 0L)) return(1L)
  
  zeroIndices <- which(row == 0L)
  if (row[length(row)] > 0L) zeroIndices <- c(zeroIndices, length(row) + 1L)
  runLengths  <- c(zeroIndices[1L], diff(zeroIndices)) - 1L
  
  length(runLengths[runLengths > 0L])
})

longestZeroRunIndices <- t(apply(numArrestsByJurisYear, 1L, function(row) {
  if (all(row > 0L)) return(c(1L, 0L))
  
  nonZeroIndices <- which(row != 0L)
  if (row[length(row)] == 0L) nonZeroIndices <- c(nonZeroIndices, length(row) + 1L)
  runLengths  <- c(nonZeroIndices[1L], diff(nonZeroIndices)) - 1L
  
  longestRun <- which.max(runLengths)
  runStart <- nonZeroIndices[longestRun] - runLengths[longestRun]
  runEnd   <- runStart + runLengths[longestRun] - 1L
  
  c(runStart, runEnd)
}))
longestZeroRunLengths <- longestZeroRunIndices[,2L] - longestZeroRunIndices[,1L] + 1L

shortJurisdictions <- largeJurisdictions & longestRunLengths < numYears %/% 3L

numShortJurisdictions <- sum(shortJurisdictions)

# plotArray(numArrestsByJurisYear, which(shortJurisdictions))


zerosInMiddle <- largeJurisdictions & numRuns >= 2L

widthToHeightRatio <- if (rmdGetOutputFormat() == "latex") 8.5 / 11 else 1.6

numZerosInMiddle <- sum(zerosInMiddle)

pdf(file.path("..", imgPath, "analysis_numArrestsZeroCounts.pdf"), 6, 6 * widthToHeightRatio)
plotArray(numArrestsByJurisYear, which(zerosInMiddle))
dev.off()

writeLines(as.character(numZerosInMiddle), txtFiles[2L])

## pull out any that have a gap in the middle for whatever reason
missingMiddleJurisdictions <- largeJurisdictions & !shortJurisdictions & zerosInMiddle & longestZeroRunLengths >= 2L

fitJurisdictions <- largeJurisdictions & !shortJurisdictions & !missingMiddleJurisdictions


y.fit <- numArrestsByJurisYear[fitJurisdictions,]

y.log <- log(y.fit)

indices.fit <- t(sapply(seq_len(nrow(y.fit)), y.fit, FUN = function(j, y) {
  res <- which(y[j,] > 0)
  res <- c(res, integer(ncol(y) - length(res)))
  res
}))
numIndices.fit <- sapply(seq_len(nrow(y.fit)), indices.fit, FUN = function(j, indices) {
  sum(indices[j,] > 0L)
})

x <- cbind(1, seq_len(numYears), seq_len(numYears)^2, seq_len(numYears)^3, seq_len(numYears)^4)
x_j <- x[,1:3]
x_sigma <- apply(y.fit, 1L, function(row) median(log(row[row > 0L])))
x_sigma <- cbind(1, x_sigma, x_sigma^2, x_sigma^3)

x.trans       <- getTransformationsForMatrix(x)
x_j.trans     <- getTransformationsForMatrix(x_j)
x_sigma.trans <- getTransformationsForMatrix(x_sigma)

x.z       <- x.trans$forward(x)
x_j.z     <- x_j.trans$forward(x_j)
x_sigma.z <- x_sigma.trans$forward(x_sigma)

w <- sqrt(exp(x_sigma[,2L]) / sum(exp(x_sigma[,2L])))

## individual fits
if (FALSE) {

df.global <- data.frame(y = log(apply(y.fit, 2L, sum)), t = seq_len(numYears))
df.global$t_sq <- df.global$t^2
df.global$t_cb <- df.global$t^3
df.global$t_qd <- df.global$t^4

df.global.trans <- getTransformationsForMatrix(df.global)

fit.global <- lm(y ~ t + t_sq + t_cb + t_qd, data = df.global.trans$forward(df.global))


sigma <- numeric(nrow(y.log))
pred  <- array(NA, c(nrow(y.log), numYears, 3L))
maxResiduals <- numeric(nrow(y.log))
for (j in seq_len(nrow(y.log))) {
  i_j <- indices.fit[j,]
  y_j <- standardize(standardize(y.log[j,i_j]) - fit.global$fitted[i_j] / w[j] / 350)
  t_j <- seq_len(numYears)[i_j]
  
  fit <- lm(y_j ~ 1 + I(standardize(t_j)) + I(standardize(t_j^2)) + I(standardize(t_j^3)) + I(standardize(t_j^4)), x = TRUE)
  
  sigma[j] <- summary(fit)$sigma / sd(y_j)
  
  vars <- diag(fit$x %*% tcrossprod(vcov(fit), fit$x))
  
  pred[j,i_j,1L] <- exp(fit$fitted) * (1 + 0.5 * vars)
  pred[j,i_j,2L] <- pred[j,i_j,1L] + qnorm(0.025) * abs(pred[j,i_j,1L]) * sqrt(vars)
  pred[j,i_j,3L] <- pred[j,i_j,1L] + qnorm(0.975) * abs(pred[j,i_j,1L]) * sqrt(vars)
  
  maxResiduals[j] <- max(abs(y_j - fit$fitted) / sigma[j])
}

plot(x_sigma[,2], sigma)

sigma.order <- order(sigma)
plotIndices <- sigma.order[seq_len(8L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], pred[plotIndices,,])

residual.order <- order(maxResiduals)
plotIndices <- residual.order[seq_len(8L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], pred[plotIndices,,])

plotIndices <- residual.order[seq.int(length(residual.order), length(residual.order) - 8L + 1L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], pred[plotIndices,,])

}


y.pars <- t(sapply(seq_len(nrow(y.log)), function(j) {
  y_j <- y.log[j,]
  c(mean(y_j[is.finite(y_j)]), sd(y_j[is.finite(y_j)]))
}))
colnames(y.pars) <- c("mu", "sigma")

data <- list(
  J = nrow(y.fit),
  T = ncol(y.fit),
  
  indices = t(indices.fit),
  numIndices = numIndices.fit,
  
  P = ncol(x),
  P_j = ncol(x_j),
  P_sigma = ncol(x_sigma),
  
  y = apply(y.fit, 1L, function(row) {
    zeroIndices <- row == 0L
    row <- as.double(row)
    row[!zeroIndices] <- standardize(log(row[!zeroIndices]))
    row
  }),
  x = x.z,
  x_j = x_j.z,
  x_sigma = x_sigma.z,
  w = w,
  
  # prior on population fixed effects
  nu_beta = 3.0,
  mu_beta = rep(0.0, ncol(x)),
  sigma_beta = c(0.25, rep(2.5, ncol(x) - 1L)),
  
  # prior on residual variance fixed effects
  nu_beta_sigma = 3.0,
  mu_beta_sigma = rep(0.0, ncol(x_sigma)),
  sigma_beta_sigma = c(5, rep(2.5, ncol(x_sigma) - 1L)),
  
  # prior on random effects covariance
  nu_L_sigma_theta = 3.0,
  sigma_L_sigma_theta = c(0.25, rep(2.5, ncol(x_j))),
  nu_L_Omega_theta = 4.0
)


model <- stan_model(file.path(srcPath, "numArrests.stan"))

samples <- sampling(model, data = data)
pars <- extract(samples)

data$y.pars <- y.pars

pars$theta <- aperm(sapply(seq_len(nrow(pars$L_sigma_theta)), simplify = "array",
  function(i) t(diag(pars$L_sigma_theta[i,]) %*% pars$L_Omega_theta[i,,] %*% pars$z[i,,])), c(3L, 1L, 2L))
## 4000 x 384
pars$sigma <- exp(tcrossprod(pars$beta_sigma, data$x_sigma) + pars$theta[,,data$P_j + 1L])

getPosteriorLinearPredictor <- function(data, pars, j)
  tcrossprod(data$x, pars$beta) / data$w[j] + tcrossprod(data$x_j, pars$theta[,j,seq_len(data$P_j)])

getPosteriorMeans <- function(data, pars, indices) {
  lapply(indices, function(j) {
    mu <- getPosteriorLinearPredictor(data, pars, j)

    exp(data$y.pars[j,"mu"] + data$y.pars[j,"sigma"] * mu)
  })
}
getPosteriorPredictions <- function(data, pars, indices) {
  lapply(indices, function(j) {
    mu <- getPosteriorLinearPredictor(data, pars, j)
    sigma <- pars$sigma[,j]
    
    exp(data$y.pars[j,"mu"] + data$y.pars[j,"sigma"] * matrix(rnorm(length(mu), mu, sigma), nrow(mu)))
  })
}

eta.mean <- apply(pars$theta[,,data$P_j + 1L], 2L, mean)
eta.order <- order(eta.mean)

pdf(file.path("..", imgPath, "analysis_numArrestsLowVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq_len(8L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(data, pars, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsHighVar.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- eta.order[seq.int(length(eta.order), length(eta.order) - 8L + 1L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(data, pars, plotIndices))
dev.off()

## fitted on standardized log scale
residuals <- t(sapply(seq_len(nrow(y.fit)), data, pars, FUN = function(j, data, pars) {
  mu <- getPosteriorLinearPredictor(data, pars, j)
  ## mu is 36 x 4000, transposed makes each column 4000 long, so sigma will get recycled in the
  ## division
  result <- t(t(data$y[,j] - mu) / pars$sigma[,j])
  result[data$y[,j] == 0,] <- NA
  
  apply(result, 1L, mean, na.rm = TRUE)
}))

maxResiduals <- apply(residuals, 1L, function(col) max(abs(col), na.rm = TRUE))
residual.order <- order(maxResiduals)

# smallest residuals not interesting, for reasons explained in Rmd
#plotIndices <- residual.order[seq_len(8L)]
#plotArray(counts, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(pars, plotIndices))

pdf(file.path("..", imgPath, "analysis_numArrestsMaxResiduals.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- residual.order[seq.int(length(residual.order), length(residual.order) - 8L + 1L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(data, pars, plotIndices))
dev.off()


largestJumps <- t(sapply(seq_len(nrow(y.fit)), y.fit, FUN = function(j, y) {
  y_j <- y[j,]
  y_j <- y_j[y_j > 0L]
  
  pct <- y_j[-1L] / y_j[-length(y_j)]
  
  c(min(pct), max(pct))
}))
down.order <- order(largestJumps[,1L])
up.order  <- order(largestJumps[,2L], decreasing = TRUE)

pdf(file.path("..", imgPath, "analysis_numArrestsDownJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- down.order[seq_len(8L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(data, pars, plotIndices))
dev.off()

pdf(file.path("..", imgPath, "analysis_numArrestsUpJumps.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- up.order[seq_len(8L)]
plotArray(numArrestsByJurisYear, which(fitJurisdictions)[plotIndices], getPosteriorPredictions(data, pars, plotIndices))
dev.off()



## released percentage

#numArrestsByJurisYear  <- apply(numArrestsTable, c(1L, 2L), sum)
#numReleasedByJurisYear <- apply(numArrestsTable, c(1L, 2L), function(x) x["released"])

y.fit <- numReleasedByJurisYear[fitJurisdictions,]
n.fit <- numArrestsByJurisYear[fitJurisdictions,]

indices.fit <- t(sapply(seq_len(nrow(n.fit)), n.fit, FUN = function(j, n) {
  res <- which(n[j,] > 0L)
  res <- c(res, integer(ncol(n) - length(res)))
  res
}))
numIndices.fit <- sapply(seq_len(nrow(n.fit)), indices.fit, FUN = function(j, indices) {
  sum(indices[j,] > 0L)
})



data <- list(
  J = nrow(y.fit),
  T = ncol(y.fit),
  
  P = ncol(x),
  P_j = ncol(x_j),
  
  indices = t(indices.fit),
  numIndices = numIndices.fit,
  
  
  n = t(n.fit),
  y = t(y.fit),
  
  x = x.z,
  x_j = x_j.z,
  
  
  # prior on population fixed effects
  nu_beta = 3.0,
  mu_beta = rep(0.0, ncol(x)),
  sigma_beta = c(5, rep(2.5, ncol(x) - 1L)),
  
  # prior on random effects covariance
  nu_L_sigma_beta_j = 3.0,
  sigma_L_sigma_beta_j = c(10, rep(5, ncol(x_j) - 1L)),
  nu_L_Omega_beta_j = 4.0)

model2 <- stan_model(file.path(srcPath, "releasedProportions.stan"))

## lmer fit
if (FALSE) {

totalNumRows <- sum(sapply(seq_len(nrow(n.fit)), function(j) sum(n.fit[j,] > 0L)))
n.tot <- integer(totalNumRows)
y.tot <- integer(totalNumRows)
t.tot <- numeric(totalNumRows)
g.tot <- integer(totalNumRows)

offset <- 0L
for (j in seq_len(nrow(n.fit))) {
  keep_j <- which(n.fit[j,] > 0L)
  n_j <- length(keep_j)
  
  range <- offset + seq_len(n_j)
  n.tot[range] <- n.fit[j,keep_j]
  y.tot[range] <- y.fit[j,keep_j]
  t.tot[range] <- seq_len(numYears)[keep_j]
  g.tot[range] <- j
  
  offset <- offset + n_j
}
rm(j, keep_j, n_j, range, offset)


releasedData <- data.frame(n = n.tot, y = y.tot, t = t.tot, g = g.tot)

z <- standardize

glmerFit <- glmer(cbind(y, n - y) ~ 1 + I(z(t)) + I(z(t^2)) + I(z(t^3)) + (1 + I(z(t)) + I(z(t^2)) | g), releasedData, family = binomial())


par(mar = c(2, 2, 0.1, 0.1), mgp = c(1.2, 0.2, 0))
j <- 10L
plot(NULL, type = "n", ylim = c(0, 1), xlim = c(1L, numYears))
lines(seq_len(numYears), y.fit[j,] / n.fit[j,])
releasedData_j <- subset(releasedData, g == j)
fits <- predict(glmerFit, releasedData_j)
lines(seq_len(numYears)[releasedData_j$t], plogis(fits), col = "gray")

rm(z)

}

samples2 <- sampling(model2, data = data)
pars2 <- extract(samples2)

getPosteriorLinearPredictor <- function(data, pars, index)
  plogis(tcrossprod(data$x, pars$beta) + tcrossprod(data$x_j, pars$beta_j[,index,]))
  
getPosteriorMeans <- function(data, pars, indices) {
  lapply(indices, function(j)
    getPosteriorLinearPredictor(data, pars, j))
}

getPosteriorPredictions <- function(data, pars, indices) {
  lapply(indices, function(j) {
    mu <- getPosteriorLinearPredictor(data, pars, j)
    ## mu is 36 x 4000, so mu as vector are the 36 years stacked on top of each other, 4000 times
    ## recycling the 36 year-long n gives the correct size
    result <- matrix(rbinom(length(mu), data$n[,j], mu), nrow(mu)) / data$n[,j]
    result[data$n[,j] == 0L,] <- NA
    result
  })
}

residuals <- t(sapply(seq_len(nrow(n.fit)), data, pars2, FUN = function(j, data, pars) {
  mu <- getPosteriorLinearPredictor(data, pars, j)
  ## mu is 36 x 4000, transposed makes each column 4000 long
  sigma <- sqrt((mu * (1 - mu)) / data$n[,j])
  p <- data$y[,j] / data$n[,j]
  result <- (p - mu) / sigma
  result[data$n[,j] == 0L,] <- NA
  
  apply(result, 1L, mean, na.rm = TRUE)
}))

maxResiduals <- apply(residuals, 1L, function(col) max(abs(col), na.rm = TRUE))
residual.order <- order(maxResiduals)

#pdf(file.path("..", imgPath, "analysis_numArrestsMaxResiduals.pdf"), 6, 6 * widthToHeightRatio)
plotIndices <- residual.order[seq.int(length(residual.order), length(residual.order) - 8L + 1L)]
plotArray(numReleasedByJurisYear / numArrestsByJurisYear, which(fitJurisdictions)[plotIndices],
          getPosteriorPredictions(data, pars2, plotIndices), totals = numArrestsByJurisYear, ylim = c(0, 1))
#dev.off()

numToPlot <- 8L
offset <- 8L
plotIndices <- residual.order[seq.int(length(residual.order) - offset, length(residual.order) - offset - numToPlot + 1L)]
plotArray(numReleasedByJurisYear / numArrestsByJurisYear, which(fitJurisdictions)[plotIndices],
          getPosteriorPredictions(data, pars2, plotIndices), totals = numArrestsByJurisYear, ylim = c(0, 1))

linPreds <- getPosteriorLinearPredictor(data, pars2, plotIndices[1L])
