# Testing summary.gnlFit
test.summary.gnlFit <- function() {
  param <- c(0, 2, 1, 1, 1)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  # RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(1000, param = param)

  testgnlFit <- gnlFit(dataVector, hessian = TRUE)
  testSummary <- summary(testgnlFit)
  checkTrue(is.numeric(testSummary$sds))
}
