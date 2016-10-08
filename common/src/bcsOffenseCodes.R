## Contains helper functions to map the offense codes to their names
## also summarizes the offense code table to get summary offense code names
##
## Use it by first loading the bcs_offense_code csv and then `source`ing the file.

getNameForOffenseCode <- function(x, arrest_year = NULL, bcsOffenseCodes = .GlobalEnv$bcsOffenseCodes)
{
  if (is.null(bcsOffenseCodes)) stop("cannot find 'bcsOffenseCodes'")
  
  if (!is.null(arrest_year)) arrest_year <- rep_len(arrest_year, length(x))
  
  sapply(seq_along(x), function(i) {
    matchingRows <- which(x[i] == bcsOffenseCodes$offense_code)
    if (length(matchingRows) == 1L) return(bcsOffenseCodes$summary_offense_type[matchingRows])
    
    bcsOffenseCodes.sub <- bcsOffenseCodes[matchingRows,]
    result <- if (is.null(arrest_year)) {
      with(bcsOffenseCodes.sub, summary_offense_type[which.max(new_2013)])
    } else {
      with(bcsOffenseCodes.sub, summary_offense_type[which(new_2013 == if (arrest_year[i] >= 2013) 1 else 0)])
    }
    if (length(result) == 0L) "NA" else result
  })
}

uniqueCodes <- unique(bcsOffenseCodes$summary_code)
uniqueCodeNames <- sapply(uniqueCodes, function(code) {
  tab <- with(bcsOffenseCodes, table(summary_offense_type[which(summary_code == code)]))
  names(tab)[which.max(tab)]
})
bcsSummaryOffenseCodes <- data.frame(summary_code = uniqueCodes, summary_offense_type = uniqueCodeNames, stringsAsFactors = FALSE)
bcsSummaryOffenseCodes <- bcsSummaryOffenseCodes[order(bcsSummaryOffenseCodes$summary_code),]
rownames(bcsSummaryOffenseCodes) <- as.character(seq_len(nrow(bcsSummaryOffenseCodes)))

rm(uniqueCodes, uniqueCodeNames)


getNameForSummaryOffenseCode <- function(x, bcsSummaryOffenseCodes = .GlobalEnv$bcsSummaryOffenseCodes)
{
  if (is.null(bcsOffenseCodes)) stop("cannot find 'bcsSummaryOffenseCodes'")
  
  with(bcsSummaryOffenseCodes, summary_offense_type[match(x, summary_code)])
}
