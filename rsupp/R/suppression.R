getRunVariables <- function(vars, keyVars, strataVars, divVar)
{
  runVars <- keyVars
  
  if (any(keyVars %not_in% vars))
    stop("keyVars '", paste0(keyVars[keyVars %not_in% vars], collapse = "', '"), "' not found")
  if (!is.null(divVar)) {
    if (length(divVar) > 1L) stop("'divVar' must specify a single variable")
    if (divVar %not_in% vars) stop("div variable '", divVar, "' not found")
    if (divVar %in% keyVars) stop("div variable cannot be in keyVars")
    
    runVars <- c(divVar, runVars)
  }
  nonStrataVars <- runVars
  
  if (!is.null(strataVars)) {
    if (any(strataVars %not_in% vars))
      stop("strata variables '", paste0(strataVars[strataVars %not_in% vars], collapse = "', '"), "' not found")
    if (any(strataVars %in% keyVars))
      stop("strata variables cannot be in keyVars")
    if (!is.null(divVar) && divVar %in% strataVars)
      stop("div variable cannot be in strataVars")
    
    runVars <- c(strataVars, runVars)
  }
  
  namedList(nonStrataVars, runVars)
}

calcRisk <- function(x, keyVars = colnames(x), strataVars = NULL, divVar = NULL, risk.f = NULL)
{
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  massign[nonStrataVars, runVars] <- getRunVariables(vars, keyVars, strataVars, divVar)
  
  x.run <- x[,match(runVars, vars)]
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  if (is.null(strataVars)) {
    .Call(C_calcRisk, x.run, risk.f)
  } else {
    x.run <- data.table(x.run)
    x.run[,list(risk = .Call(C_calcRisk, data.frame(.SD), risk.f)),
          by = strataVars, .SDcols = nonStrataVars]$risk
  }
}

getAtRiskSubset <- function(x, keyVars = colnames(x), divVar = NULL, risk.f = NULL, risk.k = 5)
{
  risk.k <- coerceOrError(risk.k[1L], "double")
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  massign[,runVars] <- getRunVariables(vars, keyVars, NULL, divVar)
  x.run <- x[,match(runVars, vars)]
    
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  res <- .Call(C_getAtRiskSubset, x.run, risk.f, risk.k)
  
  if (any(colnames(x) %not_in% colnames(x.run))) {
    extraCols <- colnames(x)[colnames(x) %not_in% colnames(x.run)]
    res[,extraCols] <- x[,extraCols]
    res <- res[,c(colnames(x), "orig.risk")]
  }
  res
}

rsupp.par <- function(alpha = 15, gamma = 0.8, n.burn = 200L, n.samp = 1000L,
                      n.chain = 8L, rowSwap.prob = 0.45, colSwap.prob = 0.25, na.prob = 0.95)
  namedList(alpha, gamma, n.burn, n.samp, n.chain, rowSwap.prob, colSwap.prob, na.prob)

localSuppression <-
  function(x, keyVars = colnames(x), strataVars = NULL, divVar = NULL, risk.f = NULL, risk.k = 5,
           keyVars.w = NULL, par = rsupp.par(), verbose = FALSE, skip.rinit = FALSE)
{
  par$risk.k  <- coerceOrError(risk.k[1L], "double")
  par$alpha   <- coerceOrError(par$alpha[1L], "double")
  par$gamma   <- coerceOrError(par$gamma[1L], "double")
  par$rowSwap.prob <- coerceOrError(par$rowSwap.prob[1L], "double")
  par$colSwap.prob <- coerceOrError(par$colSwap.prob[1L], "double")
  par$na.prob      <- coerceOrError(par$na.prob[1L], "double")
  par$n.burn  <- coerceOrError(par$n.burn[1L], "integer")
  par$n.samp  <- coerceOrError(par$n.samp[1L], "integer")
  par$verbose <- coerceOrError(verbose[1L], "integer")
  
  skip.rinit <- coerceOrError(skip.rinit[1L], "logical")
  
  n.chain <- coerceOrError(par$n.chain, "integer")
  par$n.chain <- NULL
  if (n.chain <= 0L) stop("n.chain must be a positive integer")
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  massign[nonStrataVars, runVars] <- getRunVariables(vars, keyVars, strataVars, divVar)
  
  x.run <- x[,match(runVars, vars)]
  
  if (is.null(keyVars.w)) {
    par$theta <- rep.int(1, length(keyVars))
  } else {
    if (is.character(keyVars.w)) {
      
      if (any(keyVars %not_in% keyVars.w))
        stop("keyVars '", paste0(keyVars.w[keyVars.w %not_in% keyVars], collapse = "', '"), "' not in keyVar weights")
      
      temp <- seq_along(keyVars.w)
      names(temp) <- keyVars.w
      keyVars.w <- temp
      rm(temp)
    }
    if (is.null(names(keyVars.w))) par$theta <- coerceOrError(keyVars.w, "double")
    else {
      if (any(names(keyVars.w) %not_in% keyVars))
        stop("keyVar weights '", paste0(names(keyVars.w)[names(keyVars.w) %not_in% keyVars], collapse = "', '"), "' not in keyVars")
      par$theta <- coerceOrError(keyVars.w[match(keyVars, names(keyVars.w))], "double")
    }
  }
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
 
  risk <- calcRisk(x.run, keyVars, strataVars, divVar, risk.f)
  if (par$risk.k > 0 && min(risk) >= par$risk.k) {
    x[,"risk"] <- risk
    return(list(x = x, obj = NA_real_, n = NA_real_))
  }
  
  res <- list(x = NULL, obj = -Inf)
  for (i in seq_len(n.chain)) {
    if (is.null(strataVars)) {
      res.i <- .Call(C_localSuppression, x.run, risk.f, par, skip.rinit)
    } else {
      ## mess here is to use data.table to get an efficient subset and update of the data frame within each stratum
      totalObjective <- 0
      x.dt <- data.table(x.run)
      x.dt[,paste(nonStrataVars) := {
        gc(FALSE)
        if (verbose > 0) cat("suppressing subset '", paste(sapply(.BY, as.character), collapse = "/"), "':\n", sep = "")
        x.dt.j <- as.data.frame(.SD)
        risk <- calcRisk(x.dt.j, keyVars, NULL, divVar, risk.f)
        if (par$risk.k > 0 && min(risk) >= par$risk.k) {
          .SD
        } else {
          tryResult <- tryCatch(res.j <- .Call(rsupp:::C_localSuppression, x.dt.j, risk.f, par, skip.rinit), error = function(e) e)
          if (is(tryResult, "error")) browser()
          
          callingEnv <- parent.env(environment())
          if (!is.na(res.j$obj) && is.finite(res.j$obj))
            callingEnv$totalObjective <- callingEnv$totalObjective + res.j$obj
          res.j$x[,nonStrataVars]
        }
      },by = strataVars, .SDcols = nonStrataVars]
      x.dt <- as.data.frame(x.dt)
      ## unfortunately, we can't return a data frame above, at least not with integer and double columns
      ## so stratified risk needs to be recalculated
      x.dt$risk <- calcRisk(x.dt, keyVars, strataVars, divVar, risk.f)
      res.i <- list(x = x.dt, obj = totalObjective)
    }
    
    if (res.i$obj > res$obj) {
      res$x <- res.i$x
      res$obj <- res.i$obj
    } else if (is.null(res$x)) {
      res$x <- res.i$x
    }
  }
  
  if (any(vars %not_in% runVars)) {
    extraCols <- vars[vars %not_in% runVars]
    res$x[,extraCols] <- x[,extraCols] 
  }
  res$x <- res$x[,c(vars, "risk")]
  
  res
}

## alpha - controls how strongly concentrated the penalty term for k is around k itself
## high gives more concentration (and thus permits fewer iterations from being above/below)

## gamma - relative tension between the k term and NA term; 1 is full k term, 0 is full NA

## theta - cost of NAing each variable in df
if (FALSE) kAnon <- function(x, k = 5L, alpha = 15, gamma = 0.8, theta = rep_len(1, ncol(x)),
                  n.burn = 200L, n.samp = 1000L, verbose = FALSE,
                  n.chain = 8L)
{
  k <- as.integer(k[1L])
  alpha <- as.double(alpha[1L])
  gamma <- as.double(gamma[1L])
  theta <- as.double(theta)
  n.burn <- as.integer(n.burn[1L])
  n.samp <- as.integer(n.samp[1L])
  verbose <- as.logical(verbose[1L])
  
  if (nrow(x) < k) {
    for (i in seq_along(x)) x[[i]] <- NA_integer_
    return(list(x = x, obj = NA_real_, n = NA_real_))
  } 
  
  minK <- min(.Call(C_calcK, x))
  if (minK >= k) return(list(x = x, obj = NA_real_, n = NA_real_))
  
  res <- .Call(C_anonymize, x, k, alpha, gamma, theta, n.burn, n.samp, verbose)
  
  for (i in seq_len(n.chain)) {
    res.i <- .Call(C_anonymize, x, k, alpha, gamma, theta, n.burn, n.samp, verbose)
    
    if (res.i$obj > res$obj) res <- res.i
  }
  
  if (res$obj < -1e6 && nrow(x) <= 1.5 * k) {
    for (i in seq_along(x)) x[[i]] <- NA_integer_
    return(list(x = x, obj = NA_real_, n = NA_real_))
  }
  
  i <- 1L
  while (res$obj < -1e6 && i <= 4L) {
    n.burn <- n.burn * 2L
    n.samp <- n.samp * 2L
    
    res <- .Call(C_anonymize, x, k, alpha, gamma, theta, n.burn, n.samp, verbose)
    i <- i + 1L
  }
    
  if (res$obj < -1e6) browser()
  res
}
