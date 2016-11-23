standardize <- function(x) {
  .z <- function(x) { i <- !is.na(x) & is.finite(x); (x - mean(x[i])) / sd(x[i]) }
  if (is.matrix(x)) sapply(seq_len(nrow(x)), function(i) .z(x[i,])) else .z(x)
}

getTransformationsForMatrix <- function(x) {
  pars <- sapply(seq_len(ncol(x)), function(j) {
    u <- unique(x[,j])
    if (length(u) == 1L) return(c(0, 1))
    c(mean(x[,j], na.rm = TRUE), sd(x[,j], na.rm = TRUE))
  })
  forward <- function(x)
    if (is.data.frame(x))
      as.data.frame(lapply(seq_along(x), function(j) (x[[j]] - mu[j]) / sigma[j]), col.names = colnames(x))
    else 
      sapply(seq_len(ncol(x)), function(j) (x[,j] - mu[j]) / sigma[j])
  backward <- function(x)
    if (is.data.frame(x))
      as.data.frame(lapply(seq_along(x), function(j) sigma[j] * x[[j]] + mu[j]), col.names = colnames(x))
    else
      sapply(seq_len(ncol(x)), function(j) sigma[j] * x[,j] + mu[j])
  
  env <- new.env(parent = baseenv())
  env$mu <- pars[1,]
  env$sigma <- pars[2,]
  
  environment(forward) <- env
  environment(backward) <- env
  
  list(forward = forward, backward = backward)
}
