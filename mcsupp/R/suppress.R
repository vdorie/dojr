## alpha - controls how strongly concentrated the penalty term for k is around k itself
## high gives more concentration (and thus permits fewer iterations from being above/below)

## gamma - relative tension between the k term and NA term; 1 is full k term, 0 is full NA

## theta - cost of NAing each variable in df
kAnon <- function(x, k = 5L, alpha = 15, gamma = 0.8, theta = rep_len(1, ncol(x)),
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
