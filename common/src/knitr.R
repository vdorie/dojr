colFmt <- function(x, color) {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  switch(outputFormat, 
         latex = paste0("\\textcolor{", color, "}{", x, "}"),
         html  = paste0("<font color='", color, "'>", x, "</font>"),
         x)
}
