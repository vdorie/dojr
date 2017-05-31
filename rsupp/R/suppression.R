calcRisk <- function(x, keyVars = colnames(x), div = NULL, risk.f = NULL)
{
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (!is.null(div)) x <- x[,c(div, setdiff(keyVars, div))]
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  .Call(C_calcRisk, x, risk.f)
}

rsupp.par <- function(alpha = 15, gamma = 0.8, n.burn = 200L, n.samp = 1000L,
                      n.chain = 8L, rowSwap.prob = 0.45, colSwap.prob = 0.25, na.prob = 0.95)
  namedList(alpha, gamma, n.burn, n.samp, n.chain, rowSwap.prob, colSwap.prob, na.prob)


getAtRiskSubset <- function(x, keyVars = colnames(x), div = NULL, risk.f = NULL, risk.k = 5)
{
  risk.k <- as.double(risk.k[1L])
  
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (!is.null(div)) x <- x[,c(div, setdiff(keyVars, div))]
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
  
  .Call(C_getAtRiskSubset, x, risk.f, risk.k)
}
  

localSuppression <-
  function(x, keyVars = colnames(x), div = NULL, risk.f = NULL, risk.k = 5,
           keyVars.w = NULL, par = rsupp.par(), verbose = FALSE, skip.rinit = FALSE)
{
  par$risk.k  <- as.double(risk.k[1L])
  par$alpha   <- as.double(par$alpha[1L])
  par$gamma   <- as.double(par$gamma[1L])
  par$rowSwap.prob   <- as.double(par$rowSwap.prob[1L])
  par$colSwap.prob <- as.double(par$colSwap.prob[1L])
  par$na.prob     <- as.double(par$na.prob[1L])
  par$n.burn  <- as.integer(par$n.burn[1L])
  par$n.samp  <- as.integer(par$n.samp[1L])
  par$verbose <- as.integer(verbose[1L])
  
  skip.rinit <- as.logical(skip.rinit[1L])
  
  n.chain <- par$n.chain
  par$n.chain <- NULL
  
  x <- as.data.frame(x)
  if (!is.null(div)) {
    if (div %in% keyVars) stop("div variable cannot be in keyVars")
    
    varNames <- c(div, keyVars)
    x <- x[,match(varNames, names(x))]
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
    if (is.null(names(keyVars.w))) par$theta <- as.double(keyVars.w)
    else par$theta <- as.double(keyVars.w[match(colnames(x), names(keyVars.w), nomatch = 0L)])
  }
  
  if (!is.null(risk.f) && is.function(risk.f)) risk.f <- list(risk.f, new.env(parent = baseenv()))
 
  risk.min <- min(.Call(C_calcRisk, x, risk.f))
  if (risk.min >= risk.k) return(list(x = x, obj = NA_real_, n = NA_real_))
  
  res <- .Call(C_localSuppression, x, risk.f, par, skip.rinit)
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
