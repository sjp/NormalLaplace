nlFitStart <- function(x, breaks = NULL,
                       paramStart = NULL,
                       startValues = "MoM",
                       startMethodMoM = "Nelder-Mead", ...) {

  histData <- hist(x, plot = FALSE, right = FALSE)

  if (is.null(breaks))
    breaks <- histData$breaks

  midpoints <- histData$mids
  empDens <- ifelse(!is.finite(log(histData$density)), NA, histData$density)
  maxIndex <- order(empDens, na.last = FALSE)[length(empDens)]

  if (startValues == "US") {
    svName <- "User Specified"

    if (is.null(paramStart))
      stop("paramStart must be specified")

    if (length(paramStart) != 4)
      stop("paramStart must contain 4 values")

    # check parameters
    parResult <- nlCheckPars(paramStart)
    case <- parResult$case
    errMessage <- parResult$errMessage

    if (case == "error")
      stop(errMessage)
  }

  if (startValues == "MoM") {
    svName <- "Method of Moments"
    paramStart <- nlFitStartMoM(x, startMethodMoM = startMethodMoM, ...)
  }

  names(paramStart) <- c("mu", "sigma", "alpha", "beta")
  list(paramStart = paramStart, breaks = breaks, midpoints = midpoints,
       empDens = empDens, svName = svName, startValues = startValues)
}


nlFitStartMoM <- function(x, startMethodMoM = "Nelder-Mead", ...) {

  diffMean <- function(param) {
    diff <- nlMean(param = param) - mean(x)
    diff
  }

  diffVar <- function(param) {
    diff <- nlVar(param = param) - var(x)
    diff
  }

  diffSkew <- function(param) {
    diff <- nlSkew(param = param) - skewness(x)
    diff
  }

  diffKurt <- function(param) {
    diff <- nlKurt(param = param) - kurtosis(x)
    diff
  }

  MoMOptimFun <- function(param) {
    x <- diffMean(param)^2 + diffVar(param)^2 +
    diffSkew(param)^2 + diffKurt(param)^2
  }

  # Beginning parameter estimates
  mu <- mean(x)
  sigma <- sqrt(var(x))
  alpha <- 1  # Setting these two to default of 1
  beta <- 1   # though it could be improved by solving a system of eqs
  startValuesMoM <- c(mu, sigma, alpha, beta)
  ## Get Method of Moments estimates
  MoMOptim <- optim(startValuesMoM, MoMOptimFun, method = startMethodMoM, ...)
  paramStart <- MoMOptim$par
  paramStart[2] <- sqrt(paramStart[2])  # Var -> SD
  paramStart
}
