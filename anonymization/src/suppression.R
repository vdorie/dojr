enforceKAnonymity <- function(x, across, by, inQuasiIdentifiers, suppressing, k)
{
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("k #", "k %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, suppressing)]
    
    sdc <- createSdcObj(x.sub, keyVars = inQuasiIdentifiers)
    
    dropRows <- sdc@risk$individual[,2L] < k & !is.na(x.sub[,suppressing])
    
    numSuppressed[i,"k #"] <- sum(dropRows)
    numSuppressed[i,"k %"] <- 100 * numSuppressed[i,"k #"] / nrow(x.sub)
    
    x[subRows, suppressing][dropRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}

enforceLDiversity <- function(x, across, by, inQuasiIdentifiers, forSensitiveValues, suppressing, l)
{
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("l #", "l %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, suppressing)]
    if (length(forSensitiveValues) == 1L) {
      x.sub[,"sensitive_key"] <- as.factor(x[subRows, forSensitiveValues])
    } else {
      x.sub[,"sensitive_key"] <- as.factor(apply(sapply(x[subRows, forSensitiveValues], as.character), 1L, function(row) paste0(row, collapse = ":")))
    }
    
    sdc <- createSdcObj(x.sub, keyVars = inQuasiIdentifiers)
    sdc <- ldiversity(sdc, ldiv_index = "sensitive_key")
    
    dropRows <- sdc@risk$ldiversity[,"sensitive_key_Distinct_Ldiversity"] < l & !is.na(x.sub[,suppressing])
    
    numSuppressed[i,"l #"] <- sum(dropRows)
    numSuppressed[i,"l %"] <- 100 * numSuppressed[i,"l #"] / nrow(x.sub)
    
    x[subRows, suppressing][dropRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}

## the lesser category should come first in mappedFrom, mappedTo
enforceOrderedDiversity <- function(x, across, by, inQuasiIdentifiers, forSensitiveValue, mappedFrom, mappedTo, suppressing)
{
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("o #", "o %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, c(suppressing, forSensitiveValue))]
    
    x.sub[,"sensitive_key"] <- remapFactor(x.sub[,forSensitiveValue], mappedFrom, mappedTo)
    
    summaryTable <- table(x.sub[,c(inQuasiIdentifiers, "sensitive_key")])
    atRiskTable <- apply(summaryTable, seq_len(length(dim(summaryTable)) - 1L), function(x) {
      ifelse(x[mappedTo[2L]] > 0L & x[mappedTo[1L]] == 0L, TRUE, FALSE)
    })
    
    tableDims <- dim(atRiskTable)
    dimNames <- dimnames(atRiskTable)
   
    indexExpression <- quote(match(x.sub[,inQuasiIdentifiers[1L]], dimNames[[1L]]))
    for (j in seq.int(2L, length(dim(atRiskTable)))) {
      temp <- quote(a + b)
      temp[[2L]] <- indexExpression
      temp[[3L]] <- substitute((match(x.sub[,inQuasiIdentifiers[j]], dimNames[[j]]) - 1L) * prod(head(tableDims, j - 1L)), list2env(list(j = j)))
      indexExpression <- temp
    }
    
    suppressRows <- atRiskTable[eval(indexExpression)]
    suppressRows[is.na(suppressRows)] <- FALSE
    
    numSuppressed[i,"o #"] <- sum(suppressRows)
    numSuppressed[i,"o %"] <- 100 * numSuppressed[i,"o #"] / nrow(x.sub)
    
    x[subRows, suppressing][suppressRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}
