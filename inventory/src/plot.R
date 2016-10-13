.oldPars <-
  list(mar = c(5.1, 4.1, 4.1, 2.1),
       mgp = c(3, 1, 0),
       cex = 1,
       tcl = -0.5,
       bty = "o",
       pch = 1)

.defaultPars <-
  list(mar = c(2.2, 2.2, 2.0, 0.1),
       mgp = c(1.2, 0.3, 0),
       cex = 0.8,
       tcl = -0.2,
       bty = "l",
       pch = 20)

if (length(getHook("before.plot.new")) == 0L) setHook("before.plot.new", function() {
  currentSettings <- par()

  newSettings <- .defaultPars

  for (parameterName in names(.defaultPars)) {
    if (all(is.null(.oldPars[[parameterName]]) || is.na(.oldPars[[parameterName]]))) {
      if (any(!is.null(currentSettings[[parameterName]]) || !is.na(currentSettings[[parameterName]])))
        newSettings[[parameterName]] <- currentSettings[[parameterName]]
    } else {
      if (any(!is.null(currentSettings[[parameterName]]) || !is.na(currentSettings[[parameterName]])) &&
          !identical(.oldPars[[parameterName]], currentSettings[[parameterName]]))
        newSettings[[parameterName]] <- currentSettings[[parameterName]]
    }
  }

  do.call("par", newSettings)
})

discrete.histogram <- function(x, gap = 0.5, xlab = xname, ylab = "Freq",
                               main = paste("Discrete Histogram of", xname), ...) {
  dotsList <- list(...)
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  
  if (class(x) %in% c("integer", "numeric", "factor")) {
    tab <- table(x)
  } else if (class(x) == "table") {
    tab <- x
  } else {
    stop("cannot produce discrete histogram for class '", class(x), "'")
  }
  
  if (gap >= 1 || gap < 0) stop("gap must be in [0,1)")
  
  xVals <- as.numeric(names(tab))
  yVals <- as.numeric(tab)
  
  binWidth <- (1 - gap) * (min(diff(xVals)) / 2)
  
  
  args <- list(NULL, type = "n",
               xlim =  c(min(xVals) - binWidth, max(xVals) + binWidth),
               ylim = c(0, max(yVals)),
               xlab = xlab, ylab = ylab, main = main)
  plotArgs <- intersect(names(formals(plot.default)), names(dotsList))
  if (length(plotArgs) > 0L)
    args[names(dotsList[plotArgs])] <- dotsList[plotArgs]
  do.call("plot", args)
  
  args <- list(xVals - binWidth, rep(0, length(yVals)),
               xVals + binWidth, yVals)
  rectArgs <- intersect(names(formals(rect)), names(dotsList))
  if (length(rectArgs) > 0L)
    args[names(dotsList[rectArgs])] <- dotsList[rectArgs]
  do.call("rect", args)
  invisible(NULL)
}
