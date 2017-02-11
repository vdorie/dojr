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

.latexFontSizes <- c(tiny = 5, scriptsize = 7, footnotesize = 8, small = 9,
                     normalsize = 10, large = 12, Large = 14.4, LARGE = 17.28,
                     huge = 17.28, huge = 20.74, Huge = 24.88)

.latexFormat.matrix <- function(x, type, style, alignment, width, colWidths, rowHeight, fontSize, color, ...)
{
  stringResult <- NULL
  stringConnection <- textConnection("stringResult", "w", local = TRUE)
  sink(stringConnection)
  
  if (!is.null(fontSize)) {
    if (is.character(fontSize) && fontSize %not_in% names(.latexFontSizes)) fontSize <- as.numeric(fontSize)
    if (is.numeric(fontSize)) {
      if (0 < fontSize && fontSize < 1) fontSize <- fontSize * 10
      fontSize <- names(.latexFontSizes)[which.min(abs(fontSize - .latexFontSizes))]
    }
    if (fontSize %not_in% names(.latexFontSizes)) error("unrecognized font size: ", fontSize)
  }
  
  if (!is.null(alignment) && alignment != "center") error("alignment must be 'center' or NULL")
  
  if (0 < width && width <= 1 && style != "Rmd") width <- (8.5 - 2) * width
  
  if (is.null(colWidths)) colWidths <- 1
  colWidths <- rep_len(colWidths, ncol(x))
  ## page width, minus margins, converted into points and subtracted out space between columns
  if (style == "Rmd")
    colWidths <- 0.8 * width * colWidths / sum(colWidths)
  else
    colWidths <- 0.95 * (width * 72.27 - (ncol(x) - 1) * 12) * colWidths / sum(colWidths)
  colWidths <- round(colWidths, 3)
  
  indentation <- 0L
  indentationStr <- ""
  
  if (!is.null(alignment)) {
    cat("\\begin{center}")
    indentation <- 2L
    indentationStr <- sprintf("%-*s", indentation, "")
    if (!is.null(fontSize)) cat("\\", fontSize, sep = "")
    cat("\n")
  } else {
    if (!is.null(fontSize)) cat("\\", fontSize, "\n", sep = "")
  }
  if (!is.null(rowHeight))
    cat(indentationStr, "\\renewcommand{\\arraystretch}{1.25}\n", sep = "")
  if (!is.null(color))
    cat(indentationStr, "\\rowcolors{2}{", color[1L], "}{", if (length(color) > 1L) color[2L] else "", "}\n", sep = "")
  
  if (style == "Rmd") {
    cat(indentationStr, "\\begin{longtable}[]{@{}", rep_len("l", ncol(x)), "@{}}\n", sep = "")
  } else {
    colWidths <- paste0("p{", colWidths, "pt}")
    cat(indentationStr, "\\begin{tabular}{")
    cat(colWidths, "}\n", sep = "")
  }
  
  indentation <- indentation + 2L
  indentationStr <- sprintf("%-*s", indentation, "")
  
  if (style == "Rmd") cat(indentationStr, "\\toprule\n", sep = "")
  
  if (!is.null(colnames(x))) {
    ## replace dots.in.variables names with spaces
    headerNames <- gsub("([^.])\\.([^.])", "\\1 \\2", colnames(x), perl = TRUE)
    if (style == "Rmd")
      cat(paste0(indentationStr, "\\begin{minipage}[b]{", colWidths, "\\columnwidth}\\raggedright\\strut\n",
                        indentationStr, headerNames, "\\strut\n\\end{minipage}", collapse = " & "),
          "\\tabularnewline\n\\midrule\n\\endhead\n", sep = "")
    else
      cat(indentationStr, paste0(paste0("\\textbf{", headerNames, "}"), collapse = " & "), " \\\\ \\hline\n", sep = "")
  }
  for (i in seq_len(nrow(x))) {
    if (style == "Rmd")
      cat(paste0(indentationStr, "\\begin{minipage}[t]{", colWidths, "\\columnwidth}\\raggedright\\strut\n",
                 indentationStr, x[i,], "\\strut\n\\end{minipage}", collapse = " & "),
          "\\tabularnewline\n", sep = "")
    else
      cat(indentationStr, paste0(x[i,], collapse = " & "), " \\\\\n", sep = "")
  }
  if (style == "Rmd") cat(indentationStr, "\\bottomrule\n", sep = "")
  
  indentation <- indentation - 2L
  indentationStr <- sprintf("%-*s", indentation, "")
  
  if (style == "Rmd")
    cat(indentationStr, "\\end{longtable}\n", sep = "")
  else
    cat(indentationStr, "\\end{tabular}\n", sep = "")
  
  if (!is.null(rowHeight))
    cat(indentationStr, "\\renewcommand{\\arraystretch}{1.0}\n", sep = "")
  
  if (!is.null(alignment))
    cat("\\end{center}\n")
  
  sink()
  close(stringConnection)

  stringResult
}

nativeFormat <- function(x, type = rmdGetOutputFormat(), ...) { UseMethod("nativeFormat", x) }

nativeFormat.data.frame <- function(x, type = rmdGetOutputFormat(), style = "classic", alignment = "center", width = 1, colWidths = NULL, rowHeight = NULL, fontSize = NULL, color = NULL, ...)
{
  x.char <- sapply(x, function(x.i) format(x.i, ...))
  if (!is.matrix(x.char)) x.char <- matrix(x.char, ncol = length(x), dimnames = dimnames(x))
  
  nativeFormat.matrix(x.char, type, style, alignment, width, colWidths, rowHeight, fontSize, color, ...)
}

nativeFormat.matrix <- function(x, type = rmdGetOutputFormat(), style = "classic", alignment = "center", width = 1, colWidths = NULL, rowHeight = NULL, fontSize = NULL, color = NULL, ...)
{
  if (type == "latex") {
    result <- .latexFormat.matrix(x, type, style, alignment, width, colWidths, rowHeight, fontSize, color, ...)
  } else {
    warning("native format for types other than latex not yet implemented")
    result <- rmdFormat.matrix(x, colWidths, ...)
  }
  result
}

## appends a ... if the string is too long
truncateString <- function(x, length)
  unname(sapply(x, function(x.i) if (nchar(x.i, keepNA = FALSE) > length) paste0(substr(x.i, 1L, length - 1L), "...") else x.i))

rmdFormat <- function(x, ...) { UseMethod("rmdFormat", x) }

rmdFormat.data.frame <- function(x, colWidths = NULL, maxRows = 25L, ...)
{
  x.char <- sapply(x, function(x.i) format(x.i, ...))
  if (!is.matrix(x.char)) x.char <- t(as.matrix(x.char))
  if (!is.na(maxRows) && nrow(x) > maxRows)
    x.char[maxRows,] <- "..."
  printLimit <- if (!is.na(maxRows) && maxRows < nrow(x)) maxRows else nrow(x)
  rmdFormat.matrix(x.char[seq.int(1L, printLimit),, drop = FALSE], colWidths, ...)
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
  if (!is.matrix(x.char)) x.char <- t(as.matrix(x.char))
  
  colnames(x.char) <- colnames(x)
  if (anyNA(colnames(x.char))) colnames(x.char)[is.na(colnames(x.char))] <- "NA"
  
  maxLengths <- sapply(seq_len(ncol(x)), function(col) max(nchar(x.char[1L, col]), nchar(colnames(x.char))[col]))
  rownames(x.char) <- NULL
  
  if (!is.null(rownames(x))) {
    x.char <- cbind(rownames(x), x.char)
    if (anyNA(x.char[,1L])) x.char[is.na(x.char[,1L]),1L] <- "NA"
    maxLengths <- c(max(nchar(x.char[,1L])), maxLengths)
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
