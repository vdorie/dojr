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

calcRisk <- function(x, keyVars = colnames(x), strataVars = NULL, divVar = NULL, risk.f = NULL, na.risk.within = FALSE)
{
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  nonStrataVars <- NULL; runVars <- NULL # R CMD check warnings
  massign[nonStrataVars, runVars] <- getRunVariables(vars, keyVars, strataVars, divVar)
  
  x.run <- x[,match(runVars, vars)]
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  if (is.null(strataVars)) {
    .Call(C_calcRisk, x.run, risk.f, na.risk.within)
  } else {
    x.run <- data.table(x.run)
    x.run[,list(risk = .Call(C_calcRisk, data.frame(.SD), risk.f, na.risk.within)),
          by = strataVars, .SDcols = nonStrataVars]$risk
  }
}

getAtRiskSubset <- function(x, keyVars = colnames(x), divVar = NULL, risk.f = NULL, na.risk.within = FALSE, risk.k = 5)
{
  risk.k <- coerceOrError(risk.k[1L], "double")
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  runVars <- NULL # R CMD check warnings
  massign[,runVars] <- getRunVariables(vars, keyVars, NULL, divVar)
  x.run <- x[,match(runVars, vars)]
    
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  res <- .Call(C_getAtRiskSubset, x.run, risk.f, na.risk.within, risk.k)
  
  if (any(colnames(x) %not_in% colnames(x.run))) {
    extraCols <- colnames(x)[colnames(x) %not_in% colnames(x.run)]
    res[,extraCols] <- x[,extraCols]
    res <- res[,c(colnames(x), "orig.risk")]
  }
  res
}

anonymize <- function(x, varTypes, risk.f, na.risk.within, par, skip.rinit, verbose)
{
  keyVars       <- varTypes$keyVars
  strataVars    <- varTypes$strataVars
  nonStrataVars <- varTypes$nonStrataVars
  divVar        <- varTypes$divVar
  
  quoteInNamespace <- function(name, character.only = FALSE) {
    result <- quote(a + b)
    result[[1L]] <- as.symbol(":::")
    result[[2L]] <- as.symbol("rsupp")
    
    result[[3L]] <- if (character.only) name else match.call()[[2]]
    result
  }
  
  C_localSuppression <- eval(quoteInNamespace(C_localSuppression))
  calcRisk <- eval(quoteInNamespace(calcRisk))
  
  if (is.null(strataVars)) {
    result <- .Call(C_localSuppression, x, risk.f, na.risk.within, par, skip.rinit)
  } else {
    ## mess here is to use data.table to get an efficient subset and update of the data frame within each stratum
    totalObjective <- 0
    x.dt <- data.table(x)
    x.dt[,paste(nonStrataVars) := {
      gc(FALSE)
      # if (verbose > 0) cat("suppressing subset '", paste(sapply(.BY, as.character), collapse = "/"), "':\n", sep = "")
      x.dt.j <- as.data.frame(.SD)
      if (all(is.na(x.dt.j[,keyVars]))) {
        x.dt.j
      } else {
        risk <- calcRisk(x.dt.j, keyVars, NULL, divVar, risk.f, na.risk.within)
        if (par$risk.k > 0 && min(risk) >= par$risk.k) {
          .SD
        } else {
          tryResult <- tryCatch(res.j <- .Call(C_localSuppression, x.dt.j, risk.f, na.risk.within, par, skip.rinit), error = function(e) e)
          if (is(tryResult, "error"))
            stop("caught error: ", toString(tryResult), "\n")
          
          callingEnv <- parent.env(environment())
          if (!is.na(res.j$obj) && is.finite(res.j$obj))
            callingEnv$totalObjective <- callingEnv$totalObjective + res.j$obj
          res.j$x[,nonStrataVars]
        }
      }
    },by = strataVars, .SDcols = nonStrataVars]
    x.dt <- as.data.frame(x.dt)        ## unfortunately, we can't return a data frame above, at least not with integer and double columns
    ## so stratified risk needs to be recalculated
    x.dt$risk <- calcRisk(x.dt, keyVars, strataVars, divVar, risk.f)
    result <- list(x = x.dt, obj = totalObjective)
  }
  
  result
}

rsupp.par <- function(alpha = 15, gamma = 0.8, n.burn = 200L, n.samples = 1000L,
                      n.chains = 8L, rowSwap.prob = 0.45, colSwap.prob = 0.25, na.prob = 0.95)
  namedList(alpha, gamma, n.burn, n.samples, n.chains, rowSwap.prob, colSwap.prob, na.prob)

#localSuppression <-
#  function(x, keyVars = colnames(x), strataVars = NULL, divVar = NULL, risk.f = NULL, na.risk.within = FALSE,
#           risk.k = 5, keyVars.w = NULL, par = rsupp.par(), verbose = FALSE, skip.rinit = FALSE)
localSuppression <-
  function(x, keyVars = colnames(x), strataVars = NULL, divVar = NULL, risk.f = NULL, na.risk.within = FALSE,
           risk.k = 5, n.threads = parallel::detectCores(), n.chains = max(8L, n.threads), keyVars.w = NULL, verbose = FALSE)
{
  ## force random walk to be off
  par <- rsupp.par(n.chains = n.chains, n.samples = 0L, n.burn = 0)
  ##
  par$risk.k  <- coerceOrError(risk.k[1L], "double")
  par$alpha   <- coerceOrError(par$alpha[1L], "double")
  par$gamma   <- coerceOrError(par$gamma[1L], "double")
  par$n.burn  <- coerceOrError(par$n.burn[1L], "integer")
  par$n.samples <- coerceOrError(par$n.samples[1L], "integer")
  par$rowSwap.prob <- coerceOrError(par$rowSwap.prob[1L], "double")
  par$colSwap.prob <- coerceOrError(par$colSwap.prob[1L], "double")
  par$na.prob      <- coerceOrError(par$na.prob[1L], "double")
  par$verbose <- coerceOrError(verbose[1L], "integer")
  
  na.risk.within <- coerceOrError(na.risk.within[1L], "logical")
  #skip.rinit <- coerceOrError(skip.rinit[1L], "logical")
  skip.rinit <- FALSE
  
  n.chains <- coerceOrError(par$n.chains, "integer")
  par$n.chains <- NULL
  if (n.chains <= 0L) stop("n.chains must be a positive integer")
  if (n.chains == 1L) n.threads <- 1L ## used to avoid references to parallel
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  vars <- colnames(x)
  
  nonStrataVars <- runVars <- NULL # R CMD check warnings
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
  
  ## check for trivial conditions first
  if (all(is.na(x.run[,keyVars]))) {
    return(list(x = x, obj = NA_real_, na.row = 0, na.tot = 0))
  }
  risk <- calcRisk(x.run, keyVars, strataVars, divVar, risk.f, na.risk.within)
  if (par$risk.k > 0 && min(risk) >= par$risk.k) {
    x[,"risk"] <- risk
    return(list(x = x, obj = NA_real_, na.row = 0, na.tot = 0))
  }
  
  varTypes <- namedList(keyVars, strataVars, nonStrataVars, divVar)

  if (n.threads > 1L && n.chains > 1L) {
    cluster <- parallel::makeCluster(n.threads)
    verbose <- FALSE
    parallel::clusterExport(cluster, c("x.run", "vars", "risk.f", "na.risk.within", "par", "skip.rinit", "verbose"), envir = environment())
    parallel::clusterExport(cluster, "anonymize", asNamespace("rsupp"))
    parallel::clusterEvalQ(cluster, require(rsupp))
    
    seeds <- as.integer(runif(n.chains) * .Machine$integer.max)
    tryResult <- tryCatch(results <- parallel::parLapply(cluster, seeds, function(seed) {
      set.seed(seed)
      anonymize(x.run, varTypes, risk.f, na.risk.within, par, skip.rinit, verbose = FALSE)
    }), error = function(e) e)
    
    parallel::stopCluster(cluster)
    
    if (!is(tryResult, "error"))
      result <- results[[which.max(sapply(results, function(result.i) result.i$obj))[1L]]]
    else
      stop(tryResult)
    
  } else {
    result <- anonymize(x.run, varTypes, risk.f, na.risk.within, par, skip.rinit, verbose)
    if (n.chains > 2L) for (i in seq.int(2L, n.chains)) {
      result.i <- anonymize(x.run, varTypes, risk.f, na.risk.within, par, skip.rinit, verbose)
      if (is.null(result$x) || result.i$obj > result$obj)
        result <- result.i
    }
  }
      
  if (any(vars %not_in% runVars)) {
    extraCols <- vars[vars %not_in% runVars]
    result$x[,extraCols] <- x[,extraCols] 
  }
  result$x <- result$x[,c(vars, "risk")]
  
  result$n <- NULL
  x.new.na <- is.na(result$x[,keyVars]) & !is.na(x.run[,keyVars])
  
  result$na.tot <- sum(x.new.na)
  result$na.row <- sum(apply(x.new.na, 1L, any))
  result$na.col <- apply(x.new.na, 2L, sum)
  
  result
}

