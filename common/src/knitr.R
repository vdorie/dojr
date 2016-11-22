rmdGetOutputFormat <- function() {
  outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat)) {
    tryResult <- tryCatch(outputFormat <- rmarkdown::all_output_formats(knitr::current_input())[1L], error = function(e) e)
    if (is(tryResult, "error")) outputFormat <- "latex"
  }
  outputFormat
}


rmdColFmt <- function(color, x) {
  outputFormat <- rmdGetOutputFormat()
  
  switch(outputFormat, 
         latex = paste0("\\textcolor{", color, "}{", x, "}"),
         html  = paste0("<font color='", color, "'>", x, "</font>"),
         x)
}

rmdImageInline <- function(path) {
  outputFormat <- rmdGetOutputFormat()
    
  switch(outputFormat,
         latex = paste0("\\begin{center}\n  \\includegraphics{", path, "}\n\\end{center}"),
         html  = paste0("<img src=\"", path, "\"/>"),
         paste0("![](", path, ")"))
}

rmdPageBreak <- function() {
  outputFormat <- rmdGetOutputFormat()
  
  switch(outputFormat,
         latex = "\n\\newpage\n\n",
         "")
}

## appends a ... if the string is too long
truncateString <- function(x, length)
  unname(sapply(x, function(x.i) if (nchar(x.i) > length) paste0(substr(x.i, 1L, length - 1L), "...") else x.i))

rmdFormat <- function(x, ...) { UseMethod("rmdFormat", x) }

rmdFormat.data.frame <- function(x, colWidths = NULL, maxRows = 25L, ...)
{
  x.char <- sapply(x, function(x.i) format(x.i, ...))
  if (nrow(x) > maxRows)
    x.char[maxRows,] <- "..."
  printLimit <- if (maxRows < nrow(x)) maxRowsL else nrow(x)
  
  rmdFormat.matrix(x.char[seq.int(1L, printLimit),], colWidths, ...)
}

rmdFormat.matrix <- function(x, colWidths = NULL, ...)
{
  catRow <- function(x, maxLengths, printLengths) {
    x.str <- lapply(seq_along(x), function(col) strwrap(x[col], maxLengths[col] + 1L))
    numLines <- max(sapply(x.str, length))
    
    for (i in seq_len(numLines)) {
      cat("| ")
      for (col in seq_along(x)) {
        if (col > 1L) cat(" | ")
        x.j <- if (length(x.str[[col]]) > i) paste0(x.str[[col]][i], "\\") else { if (length(x.str[[col]]) < i) "" else x.str[[col]][i] }
        cat(sprintf("%-*s", printLengths[col], x.j))
      }
      cat(" |\n")
    }
  }
  catHeaderSeparator <- function(printLengths) {
    cat("+=")
    for (col in seq_along(printLengths)) {
      if (col > 1L) cat("=+=")
      for (j in seq_len(printLengths[col])) cat("=")
    }
    cat("=+\n")
  }
  catRowSeparator <- function(printLengths) {
    cat("+-")
    for (col in seq_along(printLengths)) {
      if (col > 1L) cat("-+-")
      for (j in seq_len(printLengths[col])) cat("-")
    }
    cat("-+\n")
  }
  
  
  stringResult <- NULL
  stringConnection <- textConnection("stringResult", "w", local = TRUE)
  sink(stringConnection)
  
  x.char  <- sapply(seq_len(ncol(x)), function(col) format(x[,col], ...))
  maxLengths <- sapply(seq_len(ncol(x)), function(col) max(nchar(x.char[1L, col]), nchar(colnames(x))[col]))
  colnames(x.char) <- colnames(x)
  rownames(x.char) <- NULL
  
  if (!is.null(rownames(x))) {
    x.char <- cbind(rownames(x), x.char)
    maxLengths <- c(max(nchar(rownames(x))), maxLengths)
  }
  
  hasExtraHeaders <- if (!is.null(dimnames(x))) any(!is.null(names(dimnames(x)))) else FALSE
  extraHeaders <- if (hasExtraHeaders) names(dimnames(x)) else NULL
  
  if (hasExtraHeaders) {
    ## print row header over row names if row names exist, otherwise over top row in special header row
    if (!is.null(extraHeaders[1L])) {
      if (!is.null(rownames(x))) {
        colnames(x.char)[1L] <- extraHeaders[1L]
      } else {
        extraHeaders[1L] <- paste0(extraHeaders[1L], "\\")
      }
      maxLengths[1L] <- max(nchar(extraHeaders[1L]), maxLengths[1L])
    }
    if (ncol(x) > 1L && !is.null(extraHeaders[2L])) {
      extraHeaders[2L] <- paste0(extraHeaders[2L], "\\")
      maxLengths[2L] <- max(nchar(extraHeaders[2L]), maxLengths[2L])
    }
  }
    
  if (!is.null(colWidths)) {
    if (length(colWidths) == 1L) colWidths <- rep(colWidths, ncol(x.char))
    
    if (length(colWidths) == ncol(x)) {
      replaceRange <- seq.int(length(maxLengths) - ncol(x) + 1L, length(maxLengths))
      printLengths <- ifelse(colWidths < maxLengths[replaceRange], colWidths + 1L, maxLengths[replaceRange])
      if (length(printLengths) < length(maxLengths)) printLengths <- c(maxLengths[1L], printLengths)
      maxLengths[replaceRange] <- pmin(maxLengths[replaceRange], colWidths)
      
      if (hasExtraHeaders) browser()
    } else {
      browser()
    }
    
    
  } else {
    printLengths <- maxLengths
  }
  
  cat("+-")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat("-+-")
    for (row in seq_len(printLengths[col])) cat("-")
  }
  cat("-+\n")
  if (hasExtraHeaders) {
    cat("| ")
    if (!is.null(extraHeaders[1L]) && is.null(rownames(x))) cat(sprintf("%-*s", printLengths[1L], extraHeaders[1L]))
    else for (j in seq_len(printLengths[1L])) cat(" ")
    if (ncol(x.char) > 1L) {
      cat(" | ")
      if (!is.null(extraHeaders[2L])) cat(sprintf("%-*s", printLengths[2L], extraHeaders[2L]))
      if (ncol(x) > 2L) for (i in seq.int(3L, ncol(x.char))) {
        cat(" | ")
        for (j in seq_len(printLengths[i])) cat(" ")
      }
    }
    cat(" |\n")
  }
  
  ## can't name dimensions with a '.', so replace those that are interstital
  headerNames <- gsub("([^.])\\.([^.])", "\\1 \\2", colnames(x.char), perl = TRUE)
  
  catRow(headerNames, maxLengths, printLengths)
  
  catHeaderSeparator(printLengths)
  
  for (row in seq_len(nrow(x.char))) {
    catRow(x.char[row,], maxLengths, printLengths)
    if (row != nrow(x.char))
      catRowSeparator(printLengths)
  }
  cat("+-")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat("-+-")
    for (j in seq_len(printLengths[col])) cat("-")
  }
  cat("-+\n")
  
  sink()
  close(stringConnection)

  stringResult
}

rmdFormat.summaryDefault <- function(x, ...)
{
  df <- data.frame(Name = names(x), Value = as.numeric(x))
  rmdFormat(df, ...)
}

rmdFormat.table <- function(x, ...)
{
  if (length(dim(x)) == 1L) {
    df <- data.frame(Name = names(x), Freq = as.numeric(x))
    rmdFormat(df, ...)
  } else if (length(dim(x)) == 2L) {
    x.m <- x
    class(x.m) <- NULL
    rmdFormat(x.m, ...)
  } else {
    stop("rmdFormat for multidimensional table not yet implemented")
  }
}
