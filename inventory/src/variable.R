summarizeVariable <- function(variableName) {
  cleanVariableName <- gsub("\\.", "_", variableName)
  summaryFile <- file.path(txtPath, paste0(cleanVariableName, "_summary.txt"))
  
  if (file.exists(summaryFile)) return(readLines(summaryFile))
  
  data <- loadData()
  x <- data[[variableName]]
  
  sourceFile <- file.path(dataSrcPath, paste0(cleanVariableName, "_summary.R"))
  if (file.exists(sourceFile)) {
    source(sourceFile, local = TRUE)
  } else if (is.factor(x)) {
    tab <- table(x, useNA = "ifany")
    tab <- tab[order(tab, decreasing = TRUE)]
    
    variableSummary <- rmdFormat(tab)
  } else if (is.integer(x)) {
    imgFile <- file.path(imgPath, paste0(cleanVariableName, "_summary.pdf"))
    if (!file.exists(imgFile)) {
      pdf(imgFile, width = 4, height = 4)
      discrete.histogram(x, main = variableName, xlab = variableName, col = "gray", border = NA)
      dev.off()
    }
    
    summaryStatistics <- summary(x)
    variableSummary <- c(rmdFormat(summaryStatistics), "", rmdImageInline(imgFile), "")
  } else if (is.numeric(x)) {
    imgFile <- file.path(imgPath, paste0(cleanVariableName, "_summary.pdf"))
    if (!file.exists(imgFile)) {
      pdf(imgFile, width = 4, height = 4)
      hist(x, breaks = 20, col = "gray", main = variableName, border = NA)
      dev.off()
    }
    
    summaryStatistics <- summary(x)
    variableSummary <- c(rmdFormat(summaryStatistics), "", rmdImageInline(imgFile), "")
  } else {
    variableSummary <- ""
  }
  
  writeLines(variableSummary, summaryFile)
  variableSummary
}
