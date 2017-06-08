## old version uses SDC micro
if (FALSE) enforceKAnonymity <- function(x, across, inQuasiIdentifiers, suppressing, k, matchingCriteria = NULL)
{
  by <- if (is.factor(x[,across])) levels(x[,across]) else sort(unique(x[,across]))
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("k #", "k %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, suppressing)]
    
    sdc <- createSdcObj(x.sub, keyVars = union(inQuasiIdentifiers, suppressing))
    
    suppressRows <- sdc@risk$individual[,2L] < k & !is.na(x.sub[,suppressing])
    if (!is.null(matchingCriteria)) suppressRows <- suppressRows & matchingCriteria[subRows]
    
    numSuppressed[i,"k #"] <- sum(suppressRows)
    numSuppressed[i,"k %"] <- 100 * numSuppressed[i,"k #"] / nrow(x.sub)
    
    x[subRows, suppressing][suppressRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}

if (FALSE) enforceKAnonymity <- function(x, across, inQuasiIdentifiers, suppressing, k, matchingCriteria = NULL)
{
  by <- if (is.factor(x[,across])) levels(x[,across]) else sort(unique(x[,across]))
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("k #", "k %")))
  
  x.sub <- x[,unique(c(across, suppressing, inQuasiIdentifiers))]
  
  summaryTable <- table(x.sub)
  atRiskTable <- summaryTable < k
  
  tableDims <- dim(atRiskTable)
  dimNames <- dimnames(atRiskTable)
  
  indexExpression <- quote(match(x.sub[,across], dimNames[[1L]]) + (match(x.sub[,suppressing], dimNames[[2L]]) - 1L) * head(tableDims, 1L))
  for (j in seq.int(3L, length(dim(atRiskTable)))) {
    temp <- quote(a + b)
    temp[[2L]] <- indexExpression
    temp[[3L]] <- substitute((match(x.sub[,inQuasiIdentifiers[j - 2L]], dimNames[[j]]) - 1L) * prod(head(tableDims, j - 1L)), list2env(list(j = j)))
    indexExpression <- temp
  }
  
  suppressRows <- atRiskTable[eval(indexExpression)]
  if (!is.null(matchingCriteria)) suppressRows <- suppressRows & matchingCriteria
  suppressRows[is.na(suppressRows)] <- FALSE
  
  suppressedTable <- table(data.frame(a = x.sub[,across], x = suppressRows), useNA = "ifany")
  if (ncol(suppressedTable) == 1L) suppressedTable <- if (colnames(suppressedTable) == "FALSE") cbind(suppressedTable, "TRUE" = 0L) else cbind(suppressedTable, "FALSE" = 0L)
  numSuppressed[,1L] <- suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)), "TRUE"]
  numSuppressed[,2L] <- 100 * numSuppressed[,1L] / rowSums(suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)),])
  
  x[suppressRows,suppressing] <- NA
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}

if (FALSE) enforceKAnonymity <- function(x, k, inKeyVars, groupedBy = NULL, suppressionOrder = NULL)
{
  x.sub <- x[,unique(c(groupedBy, inKeyVars))]
  sdc <- createSdcObj(x.sub, keyVars = inKeyVars, strataVar = groupedBy)
  
  suppressionOrder <- if (!is.null(suppressionOrder)) {
    if (is.character(suppressionOrder)) suppressionOrder <- match(suppressionOrder, inKeyVars)
    length(inKeyVars) - suppressionOrder + 1L
  } else {
    NULL
  }
  suppressWarnings(sup <- kAnon(sdc, k = k, importance = suppressionOrder))
  
  if (!is.null(groupedBy)) {
    dt <- data.table(sup@manipKeyVars, sup@origData[,sup@strataVar])
    dt[,fk := as.double(freqCalc(.SD, inKeyVars)$fk),
       by = groupedBy,
       .SDcols = inKeyVars]
    
    # eval(substitute(dt[dt$fk < k,inKeyVars] <- NA, list2env(list(inKeyVars = inKeyVars))))
    dt[fk < k, paste(inKeyVars) := NA]
    
    sup@manipKeyVars <- dt[,inKeyVars,with=FALSE]
  }
  
  list(keyVars = as.data.frame(sup@manipKeyVars[,inKeyVars,with=FALSE]),
       sup = sapply(colnames(sup@origData)[sup@keyVars], function(keyVar) sum(is.na(sup@manipKeyVars[[keyVar]]) & !is.na(sup@origData[[keyVar]]))))
}

enforceKAnonymity <- function(x, k, inKeyVars, groupedBy = NULL, suppressionWeight = NULL)
{
  x.sub <- x[,unique(c(groupedBy, inKeyVars))]
  
  suppressionWeight <- if (!is.null(suppressionWeight)) {
    if (is.character(suppressionWeight)) suppressionWeight <- match(suppressionWeight, inKeyVars)
  } else {
    rep.int(1, length(inKeyVars))
  }
  
  dt <- data.table(x)
  
  dt <- dt[,paste(inKeyVars) := kAnon(data.frame(.SD), theta = suppressionWeight)$x[,inKeyVars],
           by = groupedBy, .SDcols = inKeyVars]

  list(keyVars = data.frame(dt[,inKeyVars,with=FALSE]),
       sup = sapply(inKeyVars, function(keyVar) sum(is.na(dt[[keyVar]]) & !is.na(x[[keyVar]]))))
}

enforceLDiversity <- function(x, l, inKeyVars, forSensitiveValues, groupedBy = NULL, suppressionOrder = NULL)
{
  x.sub <- x[,unique(c(groupedBy, inKeyVars))]
  if (length(forSensitiveValues) == 1L) {
    x.sub[,"_sensitive_key"] <- as.factor(x[,forSensitiveValues])
  } else {
    x.sub[,"_sensitive_key"] <- as.factor(apply(sapply(x[,forSensitiveValues], as.character), 1L, function(row) paste0(row, collapse = ":")))
  }
  
  tab <- table(x.sub, useNA = "ifany")
  tab <- apply(tab, seq_len(length(dim(tab)) - 1L), function(x) sum(x > 0L))
  tableDims <- dim(tab)
  dimNames <- dimnames(tab)
  
  indexExpression <- substitute(match(x.sub[,.A.], dimNames[[.J.]]), list2env(list(.A. = names(dimNames)[1L], .J. = 1)))
  j <- 2L
  for (k in seq.int(2L, length(tableDims))) {
    indexExpression <- substitute(.A. + .B.,
                                  list2env(list(.A. = indexExpression,
                                                .B. = substitute((match(x.sub[,.C.], dimNames[[.J.]]) - 1L) * prod(head(tableDims, .J. - 1L)),
                                                                 list2env(list(.C. = names(dimNames)[k], .J. = j))))))
    j <- j + 1L
  }
    
  x.sub[,"_l"] <- tab[eval(indexExpression)]
  browser()
  
  suppressionOrder <- if (!is.null(suppressionOrder)) {
    if (is.character(suppressionOrder)) suppressionOrder <- match(suppressionOrder, inKeyVars)
    length(inKeyVars) - suppressionOrder + 1L
  } else {
    NULL
  }

  
  suppSubsetLdiv(subset(x.sub, arrest_year == 1980 & ncic_jurisdiction == "0100", c(inKeyVars, "_sensitive_key")),
                 l, inKeyVars, "_sensitive_key", suppressionOrder)
}

if (FALSE) enforceLDiversity <- function(x, across, inQuasiIdentifiers, forSensitiveValues, suppressing, l, matchingCriteria = NULL)
{
  by <- if (is.factor(x[,across])) levels(x[,across]) else sort(unique(x[,across]))
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("l #", "l %")))
  
  for (i in seq_along(by)) {
    subRows <- x[,across] == by[i]
    
    x.sub <- x[subRows, union(inQuasiIdentifiers, suppressing)]
    if (length(forSensitiveValues) == 1L) {
      x.sub[,"sensitive_key"] <- as.factor(x[subRows, forSensitiveValues])
    } else {
      x.sub[,"sensitive_key"] <- as.factor(apply(sapply(x[subRows, forSensitiveValues], as.character), 1L, function(row) paste0(row, collapse = ":")))
    }
    
    sdc <- createSdcObj(x.sub, keyVars = union(inQuasiIdentifiers, suppressing))
    sdc <- ldiversity(sdc, ldiv_index = "sensitive_key")
    
    suppressRows <- sdc@risk$ldiversity[,"sensitive_key_Distinct_Ldiversity"] < l & !is.na(x.sub[,suppressing])
    if (!is.null(matchingCriteria)) suppressRows <- suppressRows & matchingCriteria[subRows]
    
    numSuppressed[i,"l #"] <- sum(suppressRows)
    numSuppressed[i,"l %"] <- 100 * numSuppressed[i,"l #"] / nrow(x.sub)
    
    x[subRows, suppressing][suppressRows] <- NA
  }
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}

if (FALSE) enforceLDiversity <- function(x, across, inQuasiIdentifiers, forSensitiveValues, suppressing, l, matchingCriteria = NULL)
{
  by <- if (is.factor(x[,across])) levels(x[,across]) else sort(unique(x[,across]))
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("l #", "l %")))
  
  x.sub <- x[,unique(c(across, suppressing, inQuasiIdentifiers))]
  if (length(forSensitiveValues) == 1L) {
    x.sub[,"sensitive_key"] <- as.factor(x[,forSensitiveValues])
  } else {
    x.sub[,"sensitive_key"] <- as.factor(apply(sapply(x[,forSensitiveValues], as.character), 1L, function(row) paste0(row, collapse = ":")))
  }
  
  summaryTable <- table(x.sub[,unique(c(across, suppressing, inQuasiIdentifiers, "sensitive_key"))])
  atRiskTable <- apply(summaryTable, seq.int(1L, length(dim(summaryTable)) - 1L), function(x) {
    sum(x > 0L) < l
  })
  
  tableDims <- dim(atRiskTable)
  dimNames <- dimnames(atRiskTable)
  
  indexExpression <- quote(match(x.sub[,across], dimNames[[1L]]) + (match(x.sub[,suppressing], dimNames[[2L]]) - 1L) * head(tableDims, 1L))
  for (j in seq.int(3L, length(dim(atRiskTable)))) {
    temp <- quote(a + b)
    temp[[2L]] <- indexExpression
    temp[[3L]] <- substitute((match(x.sub[,inQuasiIdentifiers[j - 2L]], dimNames[[j]]) - 1L) * prod(head(tableDims, j - 1L)), list2env(list(j = j)))
    indexExpression <- temp
  }
  
  suppressRows <- atRiskTable[eval(indexExpression)]
  if (!is.null(matchingCriteria)) suppressRows <- suppressRows & matchingCriteria
  suppressRows[is.na(suppressRows)] <- FALSE
  
  suppressedTable <- table(data.frame(a = x.sub[,across], x = suppressRows), useNA = "ifany")
  if (ncol(suppressedTable) == 1L) suppressedTable <- if (colnames(suppressedTable) == "FALSE") cbind(suppressedTable, "TRUE" = 0L) else cbind(suppressedTable, "FALSE" = 0L)
  numSuppressed[,1L] <- suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)), "TRUE"]
  numSuppressed[,2L] <- 100 * numSuppressed[,1L] / rowSums(suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)),])
  
  x[suppressRows,suppressing] <- NA
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}


## the lesser category should come first in mappedFrom, mappedTo
enforceOrderedDiversity <- function(x, across, inQuasiIdentifiers, forSensitiveValue, mappedFrom, mappedTo, suppressing)
{
  by <- if (is.factor(x[,across])) levels(x[,across]) else sort(unique(x[,across]))
  numSuppressed <- matrix(NA_real_, length(by), 2L, dimnames = list(as.character(by), c("o #", "o %")))
  
  x.sub <- x[,unique(c(across, inQuasiIdentifiers, forSensitiveValue, suppressing))]
  
  x.sub[,"sensitive_key"] <- remapFactor(x.sub[,forSensitiveValue], mappedFrom, mappedTo)
  
  summaryTable <- table(x.sub[,unique(c(across, suppressing, inQuasiIdentifiers, "sensitive_key"))])
  atRiskTable <- apply(summaryTable, seq.int(1L, length(dim(summaryTable)) - 1L), function(x) {
    x[mappedTo[2L]] > 0L && x[mappedTo[1L]] == 0L
  })
  
  tableDims <- dim(atRiskTable)
  dimNames <- dimnames(atRiskTable)
  
  indexExpression <- quote(match(x.sub[,across], dimNames[[1L]]) + (match(x.sub[,suppressing], dimNames[[2L]]) - 1L) * head(tableDims, 1L))
  for (j in seq.int(3L, length(dim(atRiskTable)))) {
    temp <- quote(a + b)
    temp[[2L]] <- indexExpression
    temp[[3L]] <- substitute((match(x.sub[,inQuasiIdentifiers[j - 2L]], dimNames[[j]]) - 1L) * prod(head(tableDims, j - 1L)), list2env(list(j = j)))
    indexExpression <- temp
  }
  
  suppressRows <- atRiskTable[eval(indexExpression)]
  suppressRows[is.na(suppressRows)] <- FALSE
  
  suppressedTable <- table(data.frame(a = x.sub[,across], x = suppressRows), useNA = "ifany")
  if (ncol(suppressedTable) == 1L) suppressedTable <- if (colnames(suppressedTable) == "FALSE") cbind(suppressedTable, "TRUE" = 0L) else cbind(suppressedTable, "FALSE" = 0L)
  numSuppressed[,1L] <- suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)), "TRUE"]
  numSuppressed[,2L] <- 100 * numSuppressed[,1L] / rowSums(suppressedTable[match(rownames(suppressedTable), rownames(numSuppressed)),])
  
  x[suppressRows,suppressing] <- NA
  
  list(suppressed = x[,suppressing], numSuppressed = numSuppressed)
}
