load("~/Repositories/dojr/common/data/macr_pii.Rdata")
macr.sub <- macr[,c("ncic_jurisdiction", "arrest_year", "disposition")]


#rm(macr)
#invisible(gc(FALSE))

jurisdictions <- levels(macr.sub$ncic_jurisdiction)
years <- sort(unique(macr.sub$arrest_year))

dispositions <- table(macr.sub)
plotOrder <- order(apply(dispositions, 1L, function(x) median(rowSums(x), na.rm = TRUE)), decreasing = TRUE)

source("~/Repositories/dojr/bookingRates/src/plotFunctions.R")

numToPlot <- 84L
plotWidth <- 8
plotHeight <- 8 / 1.6

cex.text <- 0.55

pdf("~/Desktop/disposition_averages.pdf", plotWidth, plotHeight)
par(mfrow = getGridDim(1.6, numToPlot), mar = c(0.05, 0.05, 0.05, 0.05))
for (i in seq_len(numToPlot)) {
  dispositions.i <- dispositions[plotOrder[i],,]
  totalDispositions <- rowSums(dispositions.i)
  dispositions.i <- dispositions.i / totalDispositions
  plot(NULL, type = "n", xlim = range(years), ylim = c(0, 1), yaxt = "n", xaxt= "n", bty = "n", xaxs = "i")
  for (j in seq_len(nlevels(macr$disposition))) {
    lines(years, dispositions.i[,j], col = j, lwd = 0.75)
  }
  plotRegion <- par()$usr
  xVal <- plotRegion[1] + 0.015 * (plotRegion[2] - plotRegion[1])
  yVal <- plotRegion[4] - 0.01 * (plotRegion[4] - plotRegion[3])
  text(xVal, yVal, paste0("juris ", jurisdictions[plotOrder[i]]), adj = c(0, 1), cex = cex.text)
  
  xVal <- plotRegion[2] - 0.015 * (plotRegion[2] - plotRegion[1])
  aveDispositions <- median(totalDispositions)
  textVal <- if (aveDispositions > 1e3) {
    paste0(round(aveDispositions / 1000, if (aveDispositions > 1e4) 0 else 1), "k")
  } else {
    as.character(signif(aveDispositions, 1))
  }
  text(xVal, yVal, paste0("n ~ ", textVal), adj = c(1, 1), cex = cex.text)
}
dev.off()

## now plot # dispositions per year as percentage of max over all years
pdf("~/Desktop/disposition_totals.pdf", plotWidth, plotHeight)
par(mfrow = getGridDim(1.6, numToPlot), mar = c(0.05, 0.05, 0.05, 0.05))
for (i in seq_len(numToPlot)) {
  dispositions.i <- dispositions[plotOrder[i],,]
  totalDispositions <- rowSums(dispositions.i)
  maxDispositions <- max(totalDispositions, na.rm = TRUE)
  
  plot(NULL, type = "n", xlim = range(years), ylim = c(0, 1), yaxt = "n", xaxt= "n")
  lines(years, totalDispositions / maxDispositions, lwd = 0.75)
  
  plotRegion <- par()$usr
  xVal <- plotRegion[1] + 0.015 * (plotRegion[2] - plotRegion[1])
  yVal <- plotRegion[3] + 0.015 * (plotRegion[4] - plotRegion[3])
  text(xVal, yVal, paste0("juris ", jurisdictions[plotOrder[i]]), adj = c(0, 0), cex = cex.text)
  
  xVal <- plotRegion[2] - 0.015 * (plotRegion[2] - plotRegion[1])
  textVal <- if (maxDispositions > 1e3) {
    paste0(round(maxDispositions / 1000, if (maxDispositions > 1e4) 0 else 1), "k")
  } else {
    as.character(signif(maxDispositions, 1))
  }
  text(xVal, yVal, paste0("n = ", textVal), adj = c(1, 0), cex = cex.text)
}
dev.off()


truancyCodes <- 1
immigrationCodes <- 90
macr.sub <- macr[macr$age < 18,c("arrest_year", "bcs_offense_code")]

sum(macr.sub %in% truancyCodes, na.rm = TRUE)

truancyTable     <- table(macr.sub[macr.sub$bcs_offense_code %in% truancyCodes,"arrest_year"])
immigrationTable <- table(macr.sub[macr.sub$bcs_offense_code %in% immigrationCodes,"arrest_year"])


pdf("~/Desktop/truancyImmigration.pdf", 6, 3)
par(mfrow = c(1L, 2L))
plot(as.numeric(names(truancyTable)), as.numeric(truancyTable), type = "l", lwd = 0.75,
     yaxs = "i", ylim = c(1000, max(truancyTable)),
     xlab = "Year", ylab = "Num Arrests", main = "Truancy Arrests Over Time")
plot(as.numeric(names(immigrationTable)), as.numeric(immigrationTable), type = "l", lwd = 0.75,
     yaxs = "i", ylim = c(0, max(immigrationTable)),
     xlab = "Year", ylab = "Num Arrests", main = "Immigration Arrests Over Time")
dev.off()
