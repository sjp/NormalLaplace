nlCheckPars <- function(param) {

  param <- as.numeric(param)
  mu <- param[1]
  sigma <- param[2]
  alpha <- param[3]
  beta <- param[4]

  case <- ""
  errMessage <- ""

  if (length(param) != 4 | any(is.na(param))) {
    # Accounting for the first condition
    if (length(param) != 4) {
      case <- "error"
      errMessage <- "param vector must contain 4 values"
    }

    # Handling NAs so that no checking is done on them
    if (any(is.na(param))) {
      case <- "error"
      errMessage <- "NAs encountered in parameter vector"
    }
  } else {
    # Handling individual cases of non-negativity
    if (sigma < 0) {
      case <- "error"
      errMessage <- "sigma must be non-negative"
    }

    if (alpha < 0) {
      case <- "error"
      errMessage <- "alpha must be non-negative"
    }

    if (beta < 0) {
      case <- "error"
      errMessage <- "beta must be non-negative"
    }

    # Handling cases where 2 parameters are non-negative
    if (sigma & alpha < 0) {
      case <- "error"
      errMessage <- "sigma and alpha must be non-negative"
    }

    if (sigma & beta < 0) {
      case <- "error"
      errMessage <- "sigma and beta must be non-negative"
    }

    if (alpha & beta < 0) {
      case <- "error"
      errMessage <- "alpha and beta must be non-negative"
    }

    # Handling the case where all three parameters are non-negative
    if (sigma & alpha & beta < 0) {
      case <- "error"
      errMessage <- "sigma, alpha and beta must be non-negative"
    }
  }

  result <- list(case = case, errMessage = errMessage)
  return(result)
}
