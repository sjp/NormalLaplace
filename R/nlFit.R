nlFit <- function(x, freq = NULL, paramStart = NULL,
                  method = "Nelder-Mead", hessian = FALSE,
                  plots = FALSE, printOut = FALSE,
                  controlBFGS = list(maxit = 200),
                  controlNM = list(maxit = 1000), maxitNLM = 1500, ...) {

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

  # If we do not have starting values for parameters, use defaults
  # of mu = 0, sigma, alpha and beta = 1
  # Ideally would like to start off with better parameters, add this later
  if (is.null(paramStart))
    paramStart <- c(0, 1, 1, 1)

  # Function to return the log-likelihood of the
  llhood <- function (param) {
    # Returning the negative of the log likelihood because optim()
    # minimises by default
    -sum(log(dnl(x, param = param)))
  }

  if (method == "BFGS") {
    opOut <- optim(paramStart, llhood, method = "BFGS",
                   control = controlBFGS, ...)
  }

  if (method == "Nelder-Mead") {
    opOut <- optim(paramStart, llhood, method = "Nelder-Mead",
                   control = controlNM, ...)
  }

  if (method == "nlm") {
    opOut <- nlm(llhood, paramStart, iterlim = maxitNLM, ...)
  }

  param <- as.numeric(opOut[[1]])[1:4]       # parameter values
  names(param) <- c("mu", "sigma", "alpha", "beta")
  maxLik <- as.numeric(opOut[[2]])           # maximum likelihood
  conv <- as.numeric(opOut[[4]])             # convergence
  iter <- as.numeric(opOut[[3]])[1]          # iterations

  if (hessian) {
    # do hessian code here
  }

  fitResults <- list(param = param, maxLik = maxLik,
                     hessian = if (hessian) opOut$hessian else NULL,
                     method = method, conv = conv, iter = iter,
                     obs = x, obsName = xName, paramStart = paramStart)

  class(fitResults) <- c("nlFit", "distFit")

  if (printOut)
    print.nlFit(fitResults, ...)

  if (plots)
    plot.nlFit(fitResults, ...)

  fitResults
}


print.nlFit <- function(x,
                        digits = max(3, getOption("digits") - 3), ...) {

  if (! "nlFit" %in% class(x))
    stop("Object must belong to class nlFit")

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


plot.nlFit <- function(x, which = 1:4,
                       plotTitles = paste(c("Histogram of ",
                                            "Log-Histogram of ",
                                            "Q-Q Plot of ",
                                            "P-P Plot of "),
                                          x$obsName, sep = ""),
                       ask = prod(par("mfcol")) < length(which) &
                             dev.interactive(), ...) {

  if (! "nlFit" %in% class(x))
    stop("Object must belong to class nlFit")

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
  mipoints <- x$midpoints
  obs <- x$obs
  obsName <- x$obsName

  nlDens <- function(x)
    dnl(x, param = param)

  lognlDens <- function(x)
    log(dnl(x, param = param))

  ymax <- 1.06 * max(nlDens(seq(min(breaks), max(breaks), 0.1)),
                     empDens, na.rm = TRUE)

  if (show[1]) {
    hist.default(obs, breaks, right = FALSE, freq = FALSE, ylim = c(0, ymax),
                 main = plotTitles[1], ...)
    curve(nlDens, min(breaks) - 1, max(breaks) + 1, add = TRUE, ylab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ")", sep = ""))
  }

  if (show[2]) {
    logHist(obs, breaks, include.lowest = TRUE, right = FALSE,
            main = plotTitles[2], ...)
    curve(lognlDens, min(breaks) - 1, max(breaks) + 1, add = TRUE,
          ylab = NULL, xlab = NULL)
    title(sub = paste("param = (",
          round(param[1], 3), ", ", round(param[2], 3), ", ",
          round(param[3], 3), ", ", round(param[4], 3), ")", sep = ""))
  }

  if (show[3])
    qqnl(obs, param = param, main = plotTitles[3], ...)

  if (show[4])
    ppnl(obs, param = param, main = plotTitles[4], ...)

  invisible()
}
