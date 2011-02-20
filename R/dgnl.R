## Probability density function
dgnl <- function(x, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
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

  ## Shifting by mu
  x <- x - mu

  ## Initialising result vector
  pdfValues <- rep(0, length(x))
  
  ## Because 'integrate' doesn't take vectors as input, we need to iterate over
  ## x to evaluate densities
  for (i in 1:length(x)) {
    ## Modified characteristic function. Includes minor calculation regarding
    ## complex numbers to ensure the function returns a real number
    chfn <- function(s) {
      result <- (alpha * beta * exp(-((sigma^2 * s^2) / 2))) /
                (complex(real = alpha, imaginary = -s) *
                 complex(real = beta, imaginary = s))
      result <- result^rho  ## Scaling result by rho
      r <- Mod(result)
      theta <- Arg(result)  
      r * cos(theta - (s * x[i]))
    }
    
    ## Integrating modified characteristic function
    pdfValues[i] <- (1 / pi) * integrate(chfn, 0, Inf)$value
  }
  
  ## Returning vector of densities
  pdfValues
}


## Probability density function
pgnl <- function(q, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
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

  ## Shifting by mu
  q <- q - mu

  ## Initialising result vector
  cdfValues <- rep(0, length(q))

  ## Because 'integrate' doesn't take vectors as input, we need to iterate over
  ## x to evaluate densities
  for (i in 1:length(q)) {
    ## Modified characteristic function. Includes minor calculation regarding
    ## complex numbers to ensure the function returns a real number
    chfn <- function(s) {
      result <- (alpha * beta * exp(-((sigma^2 * s^2) / 2))) /
                (complex(real = alpha, imaginary = -s) *
                 complex(real = beta, imaginary = s))
      result <- result^rho  ## Scaling result by rho
      r <- Mod(result)
      theta <- Arg(result)  
      (r / s) * sin((s * q[i]) - theta)
    }
    
    ## Integrating modified characteristic function
    cdfValues[i] <- (1 / 2) + (1 / pi) * integrate(chfn, 0, Inf)$value
  }
  
  ## Returning vector of cumulative probabilities
  cdfValues
}


### Quantile function
qgnl <- function(p, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
                 param = c(mu, sigma, alpha, beta, rho),
                 tol = 10^(-5), nInterpol = 100, subdivisions = 100, ...) {

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

  maxp <- max(p[p < 1])
  upper <- mu + sigma

  while (pgnl(upper, param = param) < maxp) {
    upper <- upper + sigma
  }

  minp <- min(p[p > 0])
  lower <- mu - sigma

  while (pgnl(lower, param = param) > minp) {
    lower <- lower - sigma
  }

  xValues <- seq(lower, upper, length = nInterpol)
  pgnlValues <- pgnl(xValues, param = param)
  pgnlSpline <- splinefun(xValues, pgnlValues)
  q <- rep(NA, length(p))

  for (i in (1:length(p))) {
    zeroFun <- function(x) {
      pgnlSpline(x) - p[i]
    }

    if ((0 < p[i]) & (p[i] < 1)) {
      q[i] <- uniroot(zeroFun, interval = c(lower, upper), ...)$root
    }

    if (p[i] == 0) q[i] <- -Inf
    if (p[i] == 1) q[i] <- Inf
    if ((p[i] < 0) | (p[i] > 1)) q[i] <- NA
  }

  return(q)
}

### Random number function
rgnl <- function(n, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
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

  ## generate random variates
  z <- rnorm(n)
  g1 <- rgamma(n, rho)
  g2 <- rgamma(n, rho)
  r <- rho*mu + sigma*sqrt(rho)*z + (1/alpha)*g1 - (1/beta)*g2

  ## return the results
  return(r)
}
