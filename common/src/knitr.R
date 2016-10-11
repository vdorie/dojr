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

rmdFormat.summaryDefault <- function(x, ...)
{
  df <- data.frame(Name = names(x), Value = as.numeric(x))
  rmdFormat(df, ...)
}

rmdFormat.table <- function(x, ...)
{
  df <- data.frame(Name = names(x), Freq = as.numeric(x))
  rmdFormat(df, ...)
}
