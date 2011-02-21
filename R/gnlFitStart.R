gnlFitStart <- function(x, breaks = "FD",
                        paramStart = NULL,
                        startValues = c("MoM", "US"),
                        startMethodMoM = "Nelder-Mead", ...) {

  ## Grabbing the correct starting value method
  startValues <- match.arg(startValues)

  histData <- hist(x, breaks = breaks, plot = FALSE, right = FALSE)

  breaks <- histData$breaks

  midpoints <- histData$mids
  empDens <- ifelse(!is.finite(log(histData$density)), NA, histData$density)
  maxIndex <- order(empDens, na.last = FALSE)[length(empDens)]

  if (startValues == "US") {
    svName <- "User Specified"

    if (is.null(paramStart))
      stop("paramStart must be specified")

    if (length(paramStart) != 5)
      stop("paramStart must contain 4 values")

    ## check parameters
    parResult <- gnlCheckPars(paramStart)
    case <- parResult$case
    errMessage <- parResult$errMessage

    if (case == "error")
      stop(errMessage)
  }

  if (startValues == "MoM") {
    svName <- "Method of Moments"
    paramStart <- gnlFitStartMoM(x, startMethodMoM = startMethodMoM, ...)
  }

  names(paramStart) <- c("mu", "sigma", "alpha", "beta", "rho")
  list(paramStart = paramStart, breaks = breaks, midpoints = midpoints,
       empDens = empDens, svName = svName, startValues = startValues)
}


gnlFitStartMoM <- function(x, startMethodMoM = "Nelder-Mead", ...) {

  diffMean <- function(param) {
    gnlMean(param = param) - mean(x)
  }

  diffVar <- function(param) {
    gnlVar(param = param) - var(x)
  }

  diffSkew <- function(param) {
    gnlSkew(param = param) - skewness(x)
  }

  diffKurt <- function(param) {
    gnlKurt(param = param) - kurtosis(x)
  }

  MoMOptimFun <- function(param) {
    diffMean(param)^2 + diffVar(param)^2 +
    diffSkew(param)^2 + diffKurt(param)^2
  }

  ## Beginning parameter estimates
  mu <- mean(x)
  sigma <- sqrt(var(x))
  alpha <- 1  # Setting these two to default of 1
  beta <- 1   # though it could be improved by solving a system of eqs
  rho <- 1    # Starting with no scaling present
  startValuesMoM <- c(mu, sigma, alpha, beta, rho)
  ## Get Method of Moments estimates
  MoMOptim <- optim(startValuesMoM, MoMOptimFun, method = startMethodMoM, ...)
  paramStart <- MoMOptim$par
  paramStart[2] <- sqrt(paramStart[2])  # Var -> SD
  paramStart
}
