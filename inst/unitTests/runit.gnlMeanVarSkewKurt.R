### Testing gnlMean
test.gnlMean <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  testMean <- gnlMean(param = param)

  ddist <- function(x, order, param, about) {
    (x - about)^order*dgnl(x, param = param)
  }

  momMean <- integrate(ddist, -30, 30, param = param, order = 1,
                       about = 0, subdivisions = 1000,
                       rel.tol = .Machine$double.eps^0.5)[[1]]

  checkEquals(testMean, momMean, tol = 0.05)
}


### Testing gnlVar
test.gnlVar <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  testVar <- gnlVar(param = param)
  mn <- gnlMean(param = param)

  ddist <- function(x, order, param, about) {
    (x - about)^order*dgnl(x, param = param)
  }

  momVar <- integrate(ddist, -30, 30, param = param, order = 2,
                      about = mn, subdivisions = 1000,
                      rel.tol = .Machine$double.eps^0.5)[[1]]

  checkEquals(testVar, momVar, tol = 0.05)
}


### Testing gnlSkew
test.gnlSkew <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  testSkew <- gnlSkew(param = param)
  mn <- gnlMean(param = param)

  ddist <- function(x, order, param, about) {
    (x - about)^order*dgnl(x, param = param)
  }

  m3 <- integrate(ddist, -30, 30, param = param, order = 3,
                  about = mn, subdivisions = 1000,
                  rel.tol = .Machine$double.eps^0.5)[[1]]
  sigma3 <- gnlVar(param = param)^(3/2)
  momSkew <- m3/sigma3

  checkEquals(testSkew, momSkew, tol = 0.05)
}


### Testing gnlKurt
test.gnlKurt <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  testKurt <- gnlKurt(param = param)
  mn <- gnlMean(param = param)

  ddist <- function(x, order, param, about) {
    (x - about)^order*dgnl(x, param = param)
  }

  m4 <- integrate(ddist, -30, 30, param = param, order = 4,
                  about = mn, subdivisions = 1000,
                  rel.tol = .Machine$double.eps^0.5)[[1]]
  sigma4 <- gnlVar(param = param)^2
  momKurt <- m4/sigma4 - 3

  ## Giving large tolerance due to the ease at which the kurtosis coefficient
  ## can skew (due to large exponents)
  checkEquals(testKurt, momKurt, tol = 6)
}
