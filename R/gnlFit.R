gnlFit <- function(x, freq = NULL, breaks = "FD", paramStart = NULL,
                   startMethod = "Nelder-Mead",
                   startValues = c("MoM", "US"),
                   method = c("Nelder-Mead", "BFGS", "L-BFGS-B",
                              "nlm", "nlminb"),
                   hessian = FALSE,
                   plots = FALSE, printOut = FALSE,
                   controlBFGS = list(maxit = 200),
                   controlLBFGSB = list(maxit = 200),
                   controlNLMINB = list(),
                   controlNM = list(maxit = 1000),
                   maxitNLM = 1500, ...) {

  # Grabbing the correct starting value method & optimisation method
  startValues <- match.arg(startValues)
  method <- match.arg(method)

  # Extracts the variable name of the dataset x
  xName <- paste(deparse(substitute(x), 500), collapse = "\n")

  # Repeating xs by the associated frequencies to reproduce the data set
  if (!is.null(freq)) {
    if (length(freq) != length(x))
      stop("vectors x and freq are not of the same length")

    x <- rep(x, freq)
  }

  # Removing any NAs in the data
  x <- as.numeric(na.omit(x))

  # Attempting to get better starting parameters prior to MLE
  startInfo <- gnlFitStart(x, breaks = breaks,
                           paramStart = paramStart,
                           startValues = startValues,
                           startMethodMoM = startMethod, ...)

  paramStart <- startInfo$paramStart
  svName <- startInfo$svName
  breaks <- startInfo$breaks
  empDens <- startInfo$empDens
  midpoints <- startInfo$midpoints
  startValues <- startInfo$startValues

  ## Function to return the log-likelihood of the estimated parameters
  llhood <- function (param) {
    ## Returning the negative of the log likelihood because optim()
    ## minimises by default
    -sum(log(dgnl(x, param = param)))
  }

  if (method == "BFGS") {
    opOut <- optim(paramStart, llhood, method = "BFGS",
                   control = controlBFGS, ...)
  }

  if (method == "L-BFGS-B") {
    opOut <- optim(par = paramStart, llhood,
                   method = "L-BFGS-B",
                   lower = c(-Inf, 0, 0, 0, 0),
                   upper = c(Inf, Inf, Inf, Inf, Inf),
                   control = controlLBFGSB, ...)
  }

  if (method == "Nelder-Mead") {
    opOut <- optim(paramStart, llhood, method = "Nelder-Mead",
                   control = controlNM, ...)
  }

  if (method == "nlm") {
    tempOpOut <- nlm(llhood, paramStart, iterlim = maxitNLM, ...)
    opOut <- list(par = tempOpOut$estimate,
                  value = tempOpOut$minimum,
                  counts = tempOpOut$iterations,
                  convergence = tempOpOut$code)
  }

  if (method == "nlminb") {
    opOut <- nlminb(start = paramStart, llhood,
                    lower = c(-Inf, 0, 0, 0, 0),
                    upper = c(Inf, Inf, Inf, Inf, Inf),
                    control = controlNLMINB, ...)
  }

  param <- as.numeric(opOut[[1]])[1:5]       # parameter values
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")
  maxLik <- as.numeric(opOut[[2]])           # maximum likelihood
  conv <- as.numeric(opOut[[4]])             # convergence
  iter <- as.numeric(opOut[[3]])[1]          # iterations

  if (hessian) {
    opOut$hessian <- tsHessian(param, llhood)
  }

  fitResults <- list(param = param, maxLik = maxLik,
                     hessian = if (hessian) opOut$hessian else NULL,
                     method = method, conv = conv, iter = iter,
                     obs = x, obsName = xName, paramStart = paramStart,
                     svName = svName, startValues = startValues,
                     breaks = breaks, midpoints = midpoints,
                     empDens = empDens)

  class(fitResults) <- c("gnlFit", "distFit")

  if (printOut)
    print.gnlFit(fitResults)

  if (plots)
    plot.gnlFit(fitResults, ...)

  fitResults
}


print.gnlFit <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  if (! "gnlFit" %in% class(x))
    stop("Object must belong to class gnlFit")

  cat("\nData:     ", x$obsName, "\n")
  cat("Parameter estimates:\n")
  print.default(format(x$param, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("Likelihood:        ", x$maxLik, "\n")
  cat("Method:            ", x$method, "\n")
  cat("Convergence code:  ", x$conv, "\n")
  cat("Iterations:        ", x$iter, "\n")
  invisible(x)
}


plot.gnlFit <- function(x, which = 1:4,
                        plotTitles = paste(c("Histogram of ",
                                             "Log-Histogram of ",
                                             "Q-Q Plot of ",
                                             "P-P Plot of "),
                                           x$obsName, sep = ""),
                        ask = prod(par("mfcol")) < length(which) &
                              dev.interactive(), ...) {

  if (! "gnlFit" %in% class(x))
    stop("Object must belong to class gnlFit")

  if (ask) {
    op <- par(ask = TRUE)
    on.exit(par(op))
  }

  par(mar = c(6, 4, 4, 2) + 0.1)
  show <- rep(FALSE, 4)
  show[which] <- TRUE
  param <- x$param
  breaks <- x$breaks
  empDens <- x$empDens
  midpoints <- x$midpoints
  obs <- x$obs
  obsName <- x$obsName

  gnlDens <- function(x)
    dgnl(x, param = param)

  loggnlDens <- function(x)
    log(dgnl(x, param = param))

  ymax <- 1.06*max(gnlDens(seq(min(breaks), max(breaks), 0.1)),
                   empDens, na.rm = TRUE)

  if (show[1]) {
    hist.default(obs, breaks, right = FALSE, freq = FALSE, ylim = c(0, ymax),
                 main = plotTitles[1], ...)
    curve(gnlDens, min(breaks) - 1, max(breaks) + 1, add = TRUE, ylab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ", ",
          round(param[5], 3), ")", sep = ""))
  }

  if (show[2]) {
    logHist(obs, breaks, include.lowest = TRUE, right = FALSE,
            main = plotTitles[2], ...)
    curve(loggnlDens, min(breaks) - 1, max(breaks) + 1, add = TRUE,
          ylab = NULL, xlab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ", ",
          round(param[5], 3), ")", sep = ""))
  }

  if (show[3])
    qqgnl(obs, param = param, main = plotTitles[3], ...)

  if (show[4])
    ppgnl(obs, param = param, main = plotTitles[4], ...)

  invisible()
}


coef.gnlFit <- function(object, ...) {
  object$param
}


vcov.gnlFit <- function(object, ...) {

  if (is.null(object$hessian))
    stop("gnlFit must be run again with the hessian parameter set to TRUE")

  varcov <- solve(object$hessian)
  varcov
}
