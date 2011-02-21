### Testing gnlCheckPars
test.gnlCheckPars <- function() {
  ## Simply passing in several sets of parameters and comparing
  ## their error messages with the expected error messages
  checkEquals(gnlCheckPars(c(0, 1.5, 1, 2, 1))$errMessage, "")
  checkEquals(gnlCheckPars(c(3, 1, 1.5, 2, 2))$errMessage, "")
  checkEquals(gnlCheckPars(c(2, -1, 1, 1, 1))$errMessage,
              "sigma must be non-negative")
  checkEquals(gnlCheckPars(c(2, 1, -1, 2, 1))$errMessage,
              "alpha must be non-negative")
  checkEquals(gnlCheckPars(c(0, 1, 2, -1, 1))$errMessage,
              "beta must be non-negative")
  checkEquals(gnlCheckPars(c(0, 1, 2, 1, -1))$errMessage,
              "rho must be non-negative")
  checkEquals(gnlCheckPars(c(0, -0.01, -0.1, 1, 1))$errMessage,
              "sigma and alpha must be non-negative")
  checkEquals(gnlCheckPars(c(2, -0.5, 1, -0.2, 1))$errMessage,
              "sigma and beta must be non-negative")
  checkEquals(gnlCheckPars(c(2, -0.5, 1, 0.2, -1))$errMessage,
              "sigma and rho must be non-negative")
  checkEquals(gnlCheckPars(c(1, 1, -0.2, -1, 1))$errMessage,
              "alpha and beta must be non-negative")
  checkEquals(gnlCheckPars(c(1, 1, -0.2, 1, -1))$errMessage,
              "alpha and rho must be non-negative")
  checkEquals(gnlCheckPars(c(1, 1, 0.2, -1, -1))$errMessage,
              "beta and rho must be non-negative")
  checkEquals(gnlCheckPars(c(1, -1, -0.2, -1, 1))$errMessage,
              "sigma, alpha and beta must be non-negative")
  checkEquals(gnlCheckPars(c(1, -1, -0.2, 1, -1))$errMessage,
              "sigma, alpha and rho must be non-negative")
  checkEquals(gnlCheckPars(c(1, -1, 0.2, -1, -1))$errMessage,
              "sigma, beta and rho must be non-negative")
  checkEquals(gnlCheckPars(c(1, 1, -0.2, -1, -1))$errMessage,
              "alpha, beta and rho must be non-negative")
  checkEquals(gnlCheckPars(c(0, -0.1, -0.2, -0.3, -0.4))$errMessage,
              "sigma, alpha, beta and rho must be non-negative")
  checkEquals(gnlCheckPars(c(0.5, NA, 1, 1, 1))$errMessage,
              "NAs encountered in parameter vector")
  checkEquals(gnlCheckPars(c(-1, 1, 1))$errMessage,
              "param vector must contain 5 values")
}
