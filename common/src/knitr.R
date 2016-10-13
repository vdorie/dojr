colFmt <- function(color, x) {
  outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat)) outputFormat <- rmarkdown::all_output_formats(knitr::current_input())[1L]
  
  switch(outputFormat, 
         latex = paste0("\\textcolor{", color, "}{", x, "}"),
         html  = paste0("<font color='", color, "'>", x, "</font>"),
         x)
}

## appends a ... if the string is too long
truncateString <- function(x, length)
  unname(sapply(x, function(x.i) if (nchar(x.i) > length) paste0(substr(x.i, 1L, length - 1L), "...") else x.i))

rmdFormat <- function(x, ...) { UseMethod("rmdFormat", x) }

rmdFormat.data.frame <- function(x, maxRows = 25L, ...)
{
  stringResult <- NULL
  stringConnection <- textConnection("stringResult", "w", local = TRUE)
  sink(stringConnection)
  
  x.char <- sapply(x, function(x.i) format(x.i, ...))
  lengths <- sapply(seq_along(x), function(i) max(nchar(x.char[1L, i]), nchar(names(x))[i]))
  
  for (i in seq_along(x)) {
    if (i > 1L) cat(" | ")
    cat(sprintf("%-*s", lengths[i], names(x)[i]))
  }
  cat("\n")
  for (i in seq_along(x)) {
    if (i > 1L) cat(" | ")
    for (j in seq_len(lengths[i])) cat("-")
  }
  cat("\n")
  printLimit <- if (nrow(x) > maxRows) maxRows - 1L else nrow(x)
  for (j in seq_len(printLimit)) {
    for (i in seq_along(x)) {
      if (i > 1L) cat(" | ")
      cat(sprintf("%-*s", lengths[i], x.char[j,i]))
    }
    cat("\n") 
  }
  if (printLimit != nrow(x)) {
    for (i in seq_along(x)) {
      if (i > 1L) cat(" | ")
      numDots <- min(lengths[i], 3L)
      for (j in seq_len(numDots)) cat(".")
      if (numDots < lengths[i]) for (j in seq.int(numDots + 1L, lengths[i])) cat(" ")
    }
    cat("\n")
  }
  sink()
  close(stringConnection)

  stringResult
}

rmdFormat.matrix <- function(x, ...)
{
  stringResult <- NULL
  stringConnection <- textConnection("stringResult", "w", local = TRUE)
  sink(stringConnection)
  
  x.char  <- sapply(seq_len(ncol(x)), function(col) format(x[,col], ...))
  lengths <- sapply(seq_len(ncol(x)), function(col) max(nchar(x.char[1L, col]), nchar(colnames(x))[col]))
  colnames(x.char) <- colnames(x)
  rownames(x.char) <- NULL
  
  if (!is.null(rownames(x))) {
    x.char <- cbind(rownames(x), x.char)
    lengths <- c(max(nchar(rownames(x))), lengths)
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
      lengths[1L] <- max(nchar(extraHeaders[1L]), lengths[1L])
    }
    if (ncol(x) > 1L && !is.null(extraHeaders[2L])) {
      extraHeaders[2L] <- paste0(extraHeaders[2L], "\\")
      lengths[2L] <- max(nchar(extraHeaders[2L]), lengths[2L])
    }
  }
  
  cat("+-")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat("-+-")
    for (row in seq_len(lengths[col])) cat("-")
  }
  cat("-+\n")
  if (hasExtraHeaders) {
    cat("| ")
    if (!is.null(extraHeaders[1L]) && is.null(rownames(x))) cat(sprintf("%-*s", lengths[1L], extraHeaders[1L]))
    else for (j in seq_len(lengths[1L])) cat(" ")
    if (ncol(x.char) > 1L) {
      cat(" | ")
      if (!is.null(extraHeaders[2L])) cat(sprintf("%-*s", lengths[2L], extraHeaders[2L]))
      if (ncol(x) > 2L) for (i in seq.int(3L, ncol(x.char))) {
        cat(" | ")
        for (j in seq_len(lengths[i])) cat(" ")
      }
    }
    cat(" |\n")
  }
  cat("| ")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat(" | ")
    cat(sprintf("%-*s", lengths[col], colnames(x.char)[col]))
  }
  cat(" |\n")
  cat("+=")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat("=+=")
    for (j in seq_len(lengths[col])) cat("=")
  }
  cat("=+\n")
  for (row in seq_len(nrow(x.char))) {
    cat("| ")
    for (col in seq_len(ncol(x.char))) {
      if (col > 1L) cat(" | ")
      cat(sprintf("%-*s", lengths[col], x.char[row,col]))
    }
    cat(" |\n")
    if (row != nrow(x.char)) {
      cat("+-")
      for (col in seq_len(ncol(x.char))) {
        if (col > 1L) cat("-+-")
        for (j in seq_len(lengths[col])) cat("-")
      }
      cat("-+\n")
    }
  }
  cat("+-")
  for (col in seq_len(ncol(x.char))) {
    if (col > 1L) cat("-+-")
    for (j in seq_len(lengths[col])) cat("-")
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
  } else if (length(dim(tab)) == 2L) {
    x.m <- x
    class(x.m) <- NULL
    rmdFormat(x.m, ...)
  } else {
    stop("rmdFormat for multidimensional table not yet implemented")
  }
}
