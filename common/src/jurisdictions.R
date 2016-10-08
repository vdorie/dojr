## Contains helper functions to map the jurisdictions to their names
##
## Use it by first loading the jurisdictions csv and then `source`ing the file.

getNameForJurisdiction <- function(x, jurisdictions = .GlobalEnv$jurisdictions)
{
  if (is.null(jurisdictions)) stop("cannot find 'jurisdictions'")
  
  with(jurisdictions, Agency[match(x, Code)])
}
