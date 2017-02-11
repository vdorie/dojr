enforceKAnonymity <- function(x, across, by, inQuasiIdentifiers, suppressing, k)
{
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("k #", "k %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, suppressing)]
    
    sdc <- createSdcObj(x.sub, keyVars = inQuasiIdentifiers)
    
    dropRows <- sdc@risk$individual[,2L] <= k & !is.na(x.sub[,suppressing])
    
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
      x.sub[["sensitive_key"]] <- as.factor(x[subRows, forSensitiveValues])
    } else {
      x.sub[,"sensitive_key"] <- as.factor(apply(sapply(x[subRows, forSensitiveValues], as.character), 1L, function(row) paste0(row, collapse = ":")))
    }
    
    sdc <- createSdcObj(x.sub, keyVars = inQuasiIdentifiers)
    sdc <- ldiversity(sdc, ldiv_index = "sensitive_key")
    
    dropRows <- sdc@risk$ldiversity[,"sensitive_key_Distinct_Ldiversity"] <= l & !is.na(x.sub[,suppressing])
    
    numSuppressed[i,"l #"] <- sum(dropRows)
    numSuppressed[i,"l %"] <- 100 * numSuppressed[i,"l #"] / nrow(x.sub)
    
    x[subRows, suppressing][dropRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}
