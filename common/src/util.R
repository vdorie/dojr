"%not_in%" <- function(x, table) match(x, table, nomatch = 0L) <= 0L

remapFactor <- function(x, groups, name, droplevels = TRUE) {
  if (!is.list(groups)) groups <- list(groups)
  if (length(groups) != length(name)) stop("length of groups != length of name")
  for (i in seq_along(groups)) {
    if (length(groups[[i]]) > 1L) x[x %in% groups[[i]][-1L]] <- groups[[i]][1L]
    levels(x)[levels(x) == groups[[i]][1L]] <- name[i]
  }
  if (droplevels) x <- base::droplevels(x)
  x
}

## evaluates the expression 'e' by binding its first argument as 'x'
evalx <- function(x, e) {
  mc <- match.call()
  callingEnv <- parent.frame()
  evalEnv <- new.env(parent = callingEnv)
  evalEnv$x <- x
  eval(mc$e, evalEnv)
}
