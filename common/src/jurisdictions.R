## Contains helper functions to map the jurisdictions to their names
##
## Use it by first loading the jurisdictions csv and then `source`ing the file.

getNameForJurisdiction <- function(x, jurisdictions = .GlobalEnv$jurisdictions)
{
  if (is.null(jurisdictions)) stop("cannot find variable 'jurisdictions', load jurisdictions.csv first")
  
  with(jurisdictions, Agency[match(x, Code)])
}

getCountyNameForJurisdiction <- function(x, jurisdictions = .GlobalEnv$jurisdictions)
{
  if (is.null(jurisdictions)) stop("cannot find variable 'jurisdictions', load jurisdictions.csv first")
  
  with(jurisdictions, County[match(x, Code)])
}

getCountyNameForCode <- function(x, jurisdictions = .GlobalEnv$jurisdictions)
{
  if (is.null(jurisdictions)) stop("cannot find variable 'jurisdictions', load jurisdictions.csv first")
  
  with(jurisdictions, County[match(x, CntyCode)[1L]])
}
