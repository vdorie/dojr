calcRisk <- function(x, keyVars = colnames(x), div = NULL, risk.f = NULL)
{
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (any(keyVars %not_in% colnames(x)))
    stop("keyVars '", paste0(keyVars[keyVars %not_in% colnames(x)], collapse = "', '"), "' not found")
  if (!is.null(div)) {
    if (div %not_in% colnames(x)) stop("div '", div, "' not found")
    if (div %in% keyVars) stop("div variable cannot be in keyVars")
    
    x.run <- x[,match(c(div, keyVars), names(x))]
  } else {
    x.run <- x[,keyVars]
  }
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  .Call(C_calcRisk, x.run, risk.f)
}

rsupp.par <- function(alpha = 15, gamma = 0.8, n.burn = 200L, n.samp = 1000L,
                      n.chain = 8L, rowSwap.prob = 0.45, colSwap.prob = 0.25, na.prob = 0.95)
  namedList(alpha, gamma, n.burn, n.samp, n.chain, rowSwap.prob, colSwap.prob, na.prob)


getAtRiskSubset <- function(x, keyVars = colnames(x), div = NULL, risk.f = NULL, risk.k = 5)
{
  risk.k <- coerceOrError(risk.k[1L], "double")
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (any(keyVars %not_in% colnames(x)))
    stop("keyVars '", paste0(keyVars[keyVars %not_in% colnames(x)], collapse = "', '"), "' not found")
  if (!is.null(div)) {
    if (div %not_in% colnames(x)) stop("div '", div, "' not found")
    if (div %in% keyVars) stop("div variable cannot be in keyVars")
    
    x.run <- x[,match(c(div, keyVars), names(x))]
  } else {
    x.run <- x[,keyVars]
  }
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  res <- .Call(C_getAtRiskSubset, x.run, risk.f, risk.k)
  
  if (any(colnames(x) %not_in% colnames(x.run))) {
    extraCols <- colnames(x)[colnames(x) %not_in% colnames(x.run)]
    res[,extraCols] <- x[,extraCols]
    res <- res[,c(colnames(x), "orig.risk")]
  }
  res
}
  

localSuppression <-
  function(x, keyVars = colnames(x), div = NULL, risk.f = NULL, risk.k = 5,
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
  
  n.chain <- par$n.chain
  par$n.chain <- NULL
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (any(keyVars %not_in% colnames(x)))
    stop("keyVars '", paste0(keyVars[keyVars %not_in% colnames(x)], collapse = "', '"), "' not found")
  if (!is.null(div)) {
    if (div %not_in% colnames(x)) stop("div '", div, "' not found")
    if (div %in% keyVars) stop("div variable cannot be in keyVars")
    
    x.run <- x[,match(c(div, keyVars), names(x))]
  } else {
    x.run <- x[,keyVars]
  }
  
  if (is.null(keyVars.w)) {
    par$theta <- rep.int(1, length(keyVars))
  } else {
    if (is.character(keyVars.w)) {
      temp <- seq_along(keyVars.w)
      names(temp) <- keyVars.w
      keyVars.w <- temp
      rm(temp)
    }
    if (is.null(names(keyVars.w))) par$theta <- coerceOrError(keyVars.w, "double")
    else par$theta <- coerceOrError(keyVars.w[match(colnames(x), names(keyVars.w), nomatch = 0L)], "double")
  }
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
 
  risk.min <- min(.Call(C_calcRisk, x.run, risk.f))
  if (risk.min >= risk.k) return(list(x = x, obj = NA_real_, n = NA_real_))
  
  res <- .Call(C_localSuppression, x.run, risk.f, par, skip.rinit)
  if (any(colnames(x) %not_in% colnames(x.run))) {
    extraCols <- colnames(x)[colnames(x) %not_in% colnames(x.run)]
    res$x[,extraCols] <- x[,extraCols]
    res$x <- res$x[,c(colnames(x), "risk")]
  }
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
