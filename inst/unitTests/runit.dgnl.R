# Testing dgnl
test.dgnl <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  tol <- 0.001

  testdgnl1 <- dgnl(1, param = param)
  pdgnl1 <- 0.24089

  testdgnl2 <- dgnl(2, param = param)
  pdgnl2 <- 0.12631

  testdgnl3 <- dgnl(3, param = param)
  pdgnl3 <- 0.04285

  checkEquals(testdgnl1, pdgnl1, tol = tol)
  checkEquals(testdgnl2, pdgnl2, tol = tol)
  checkEquals(testdgnl3, pdgnl3, tol = tol)
}


# Testing pgnl
test.pgnl <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  tol <- 0.001

  testpgnl1 <- pgnl(0, param = param)
  ppgnl1 <- 0.4344321

  testpgnl2 <- pgnl(1, param = param)
  ppgnl2 <- 0.7061278

  testpgnl3 <- pgnl(2, param = param)
  ppgnl3 <- 0.8906825

  checkEquals(testpgnl1, ppgnl1, tol = tol)
  checkEquals(testpgnl2, ppgnl2, tol = tol)
  checkEquals(testpgnl3, ppgnl3, tol = tol)

  # Checking that the CDF is equal to the inverse
  # of the inverse of the CDF
  checkTrue(pgnl(1) == pgnl(qgnl(pgnl(1))))
}


# Testing qgnl
test.qgnl <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  tol <- 0.001

  testqgnl1 <- qgnl(0.25, param = param)
  pqgnl1 <- -0.7115

  testqgnl2 <- qgnl(0.5, param = param)
  pqgnl2 <- 0.23182

  testqgnl3 <- qgnl(0.75, param = param)
  pqgnl3 <- 1.18981

  checkEquals(testqgnl1, pqgnl1, tol = tol)
  checkEquals(testqgnl2, pqgnl2, tol = tol)
  checkEquals(testqgnl3, pqgnl3, tol = tol)

  # Checking that the CDF is equal to the inverse
  # of the inverse of the CDF
  checkTrue(pgnl(1) == pgnl(qgnl(pgnl(1))))
}


# Testing rgnl
test.rgnl <- function() {
  param <- c(0, 1, 2, 3, 1.5)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  # RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(5, param = param)

  checkEquals(dataVector, c(0.7539770, 0.9929244, -0.8534241, -3.3190444, -0.7244840))
}
