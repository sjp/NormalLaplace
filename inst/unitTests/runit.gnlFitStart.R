# Testing gnlFitStart
test.gnlFitStart <- function() {
  param <- c(0, 1, 2, 3, 1)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  # RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(1000, param = param)

  # Testing user-supplied parameters
  checkEquals(gnlFitStart(dataVector, paramStart = param,
                          startValues = "US")$paramStart, param)

  # Testing generated starting parameters, also tests gnlFitStartMoM
  testParamStart <- gnlFitStart(dataVector)$paramStart
  values <- c(mu = 1.0192411, sigma = 1.6449032,
              alpha = 4.4851024, beta = 10.4689417, rho = 0.1757478)
  checkEquals(testParamStart, values)
}
