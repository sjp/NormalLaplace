### Mean function
gnlMean <- function(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
                    param = c(mu, sigma, alpha, beta, rho)) {

  ## check parameters
  parResult <- gnlCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- param[1]
  sigma <- param[2]
  alpha <- param[3]
  beta <- param[4]
  rho <- param[5]

  gnlMean <- rho*(mu + 1/alpha - 1/beta)

  return(gnlMean)
}


### Variance function
gnlVar <- function(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
                   param = c(mu, sigma, alpha, beta, rho)) {

  ## check parameters
  parResult <- gnlCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- param[1]
  sigma <- param[2]
  alpha <- param[3]
  beta <- param[4]
  rho <- param[5]

  gnlVar <- rho*(sigma^2 + 1/(alpha^2) + 1/(beta^2))

  return(gnlVar)
}


### Skewness function
gnlSkew <- function(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
                    param = c(mu, sigma, alpha, beta, rho)) {

  ## check parameters
  parResult <- gnlCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- param[1]
  sigma <- param[2]
  alpha <- param[3]
  beta <- param[4]
  rho <- param[5]

  k2 <- gnlVar(param = param)
  k3 <- (2*rho)/(alpha^3) - (2*rho)/(beta^3)

  gnlSkew <- k3/(k2^(3/2))

  return(gnlSkew)
}


### Kurtosis function
gnlKurt <- function(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
                    param = c(mu, sigma, alpha, beta, rho)) {

  ## check parameters
  parResult <- gnlCheckPars(param)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- param[1]
  sigma <- param[2]
  alpha <- param[3]
  beta <- param[4]
  rho <- param[5]

  k2 <- gnlVar(param = param)
  k4 <- (6*rho)/(alpha^4) + (6*rho)/(beta^4)
  nlKurt <- k4/(k2^2)

  return(nlKurt)
}
