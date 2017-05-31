## from lme4
namedList <- function(...) {
  result <- list(...)
  substituteNames <- sapply(substitute(list(...)), deparse)[-1L]
  if (is.null(resultNames <- names(result))) resultNames <- substituteNames
  if (any(noNames <- resultNames == "")) resultNames[noNames] <- substituteNames[noNames]
  setNames(result, resultNames)
}

"%not_in%" <- function(x, table) match(x, table, nomatch = 0L) <= 0L

evalx <- function(x, e) {
  mc <- match.call()
  callingEnv <- parent.frame()
  evalEnv <- new.env(parent = callingEnv)
  evalEnv$x <- x
  eval(mc$e, evalEnv)
}
