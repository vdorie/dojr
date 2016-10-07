colFmt <- function(color, x) {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat)) outputFormat <- rmarkdown::all_output_formats(knitr::current_input())[1L]
  switch(outputFormat, 
         latex = paste0("\\textcolor{", color, "}{", x, "}"),
         html  = paste0("<font color='", color, "'>", x, "</font>"),
         x)
}
