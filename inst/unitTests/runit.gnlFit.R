### Testing gnlFit
test.gnlFit <- function() {
  param <- c(0, 2, 1, 1, 1)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  ## RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(1000, param = param)

  ## Grabbing starting parameter object
  testParamStart <- gnlFitStart(dataVector)

  ## Running fitting over different methods of optimisation
  testgnlFitDefault <- gnlFit(dataVector, hessian = TRUE)

  ## These checks will either pass or fail regardless of method choice
  checkEquals(dataVector, testgnlFitDefault$obs)
  checkEquals("dataVector", testgnlFitDefault$obsName)
  checkEquals(is.null(testgnlFitDefault$hessian), FALSE)
  checkEquals(testParamStart$paramStart, testgnlFitDefault$paramStart)
  checkEquals(testParamStart$svName, testgnlFitDefault$svName)
  checkEquals(testParamStart$breaks, testgnlFitDefault$breaks)
  checkEquals(testParamStart$empDens, testgnlFitDefault$empDens)
  checkEquals(testParamStart$midpoints, testgnlFitDefault$midpoints)
  checkEquals(testParamStart$startValues, testgnlFitDefault$startValues)

  ## Only checking default/N-M
  checkEquals("Nelder-Mead", testgnlFitDefault$method)
  checkTrue(is.numeric(testgnlFitDefault$conv))
  checkTrue(is.numeric(testgnlFitDefault$iter) &
            testgnlFitDefault$iter > 0)  # More than one iteration occurred
  checkTrue(is.numeric(testgnlFitDefault$maxLik))
  checkTrue(all(! is.na(testgnlFitDefault$param)) &
            is.numeric(testgnlFitDefault$param))  # Want no NAs and all numeric
  checkEquals(class(testgnlFitDefault), c("gnlFit", "distFit"))
}


## Testing graphical output
graphicstest.gnlFit <- function() {
  param <- c(0, 2, 1, 1, 1)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  ## RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(1000, param = param)
  testgnlFit <- gnlFit(dataVector)

  pdf("Histogram of dataVector.pdf")
  plot.gnlFit(testgnlFit, which = 1)
  dev.off()

  pdf("Log-Histogram of dataVector.pdf")
  plot.gnlFit(testgnlFit, which = 2)
  dev.off()
}
