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
