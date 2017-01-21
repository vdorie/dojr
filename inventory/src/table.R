getColumnType <- function(x) { cl <- class(x); if (cl[1L] %in% c("numeric", "integer") && length(unique(x)) == 2L) "binary" else cl }
getColumnValues <- function(x) {
  if (is.factor(x)) truncateString(paste0(truncateString(levels(x), 7L), collapse = "/"), 18L)
  else if (is.numeric(x)) paste0(range(x, na.rm = TRUE), collapse = "-")
  else if (is.character(x)) truncateString(paste0(truncateString(unique(x), 7L), collapse = "/"), 18L)
}

formatFirstFiveRows <- function(tableName) {
  firstFiveFile <- file.path(txtPath, paste0(tableName, "_firstFive.txt"))
  if (file.exists(firstFiveFile)) return(readLines(firstFiveFile))
  
  firstFive <- rmdDisplayDataFrameHead(loadData(tableName), maxRows = 6L, maxCols = 7L, maxColWidth = 14L)
  writeLines(firstFive, firstFiveFile)
  firstFive
}

## truncates a data frame to just its first 5 lines and also trims the column text
rmdDisplayDataFrameHead <- function(df, maxRows = 6L, maxCols = 7L, maxColWidth = 14L)
{
  if (nrow(df) > maxRows) df <- df[seq_len(maxRows + 1L),]
  if (ncol(df) > maxCols) df <- cbind(df[,seq_len(maxCols - 1L)], "..." = rep("...", nrow(df)))
  
  for (j in seq_along(df)) {
    colnames(df)[j] <- truncateString(colnames(df[j]), maxColWidth)
    if (is.factor(df[[j]]) && !all(is.na(df[[j]]))) {
      df[[j]] <- droplevels(df[[j]])
      levels(df[[j]]) <- truncateString(levels(df[[j]]), maxColWidth)
    } else if (is.character(df[[j]]))
      df[[j]] <- truncateString(df[[j]], maxColWidth)
  }
  rmdFormat(df, maxRows = maxRows)
}

summarizeVariables <- function(tableName) {
  summaryFile <- file.path(txtPath, paste0(tableName, "_variableSummary.csv"))
  if (file.exists(summaryFile)) return(read.csv(summaryFile, stringsAsFactors = FALSE))
  
  variablesFile <- file.path(datasetDir, paste0(tableName, "_variables.csv"))
  if (!file.exists(variablesFile)) return("") 
  variables <- read.csv(variablesFile, stringsAsFactors = FALSE)
  
  
  data <- loadData(tableName)
  types <- sapply(data, getColumnType)
  types <- unname(types[match(variables$name, names(types))])
  values <- sapply(names(data), function(variableName) {
    variableRow <- which.max(variables$name == variableName)
    if (length(variableRow) == 0L) return("NA")
    if (!is.null(variables$contains_pii) && variables$contains_pii[variableRow] == 1L) return("pii")
    getColumnValues(data[[variableName]])
  })
  values <- as.character(values[match(variables$name, names(values))])
  variablesSummary <- data.frame(name = truncateString(variables$name, 24L),
                                 type = types,
                                 value = values,
                                 description = variables$short_description)
  write.csv(variablesSummary, file = summaryFile, row.names = FALSE)
  
  variablesSummary
}
