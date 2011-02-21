\name{GeneralisedNormalLaplaceDistribution}
\alias{GeneralisedNormalLaplaceDistribution}
\alias{GeneralizedNormalLaplaceDistribution}
\alias{dgnl}
\alias{pgnl}
\alias{qgnl}
\alias{rgnl}

\title{
  Generalised Normal Laplace Distribution
}
\description{
  Density function, distribution function, quantiles and random number
  generation for the Generalised Normal Laplace distribution, with parameters
  \eqn{\mu}{mu} (location), \eqn{\sigma}{sigma} (scale),
  \eqn{\alpha}{alpha} (tail), \eqn{\beta}{beta} (tail) and \eqn{\rho}{rho} (scale).
}
\usage{
dgnl(x, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
     param = c(mu, sigma, alpha, beta, rho))
pgnl(q, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
     param = c(mu, sigma, alpha, beta, rho))
qgnl(p, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
     param = c(mu, sigma, alpha, beta, rho),
     tol = 10^(-5), nInterpol = 100, subdivisions = 100, ...)
rgnl(n, mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
     param = c(mu, sigma, alpha, beta, rho))
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of random variates to be generated.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{sigma}{Scale parameter \eqn{\sigma}{sigma}, default is 1.}
  \item{alpha}{Tail parameter \eqn{\alpha}{alpha}, default is 1.}
  \item{beta}{Tail parameter \eqn{\beta}{beta}, default is 1.}
  \item{rho}{Scale parameter \eqn{\rho}{rho}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu, sigma, alpha, beta, rho)}.}
  \item{tol}{Specified level of tolerance when checking if
    parameter beta is equal to 0.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density and determine the accuracy of the distribution
    function calculation.}
  \item{nInterpol}{Number of points used in \code{qnl} for cubic
    spline interpolation of the distribution function.}
  \item{\dots}{Passes arguments to \code{uniroot}.}
}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones.

  The characteristic function of the distribution is required to be evaluted
  and integrated over in order to obtain the density and distribution functions.

  The characteristic function is

  \deqn{\phi(s)=\left(\frac{\alpha \beta e^{-\sigma^2s^2/2}}{
                (\alpha - is)(\beta + is)}\right)^{\rho}
  }
  
  The density function is

  \deqn{f(x - \mu)=\frac{1}{\pi} \int_0^\infty r(s)cos(\theta(s) - sy)ds}
  
  Where \eqn{r(s)} and \eqn{\theta(s)} are respectively the modulus and argument
  of the complex number returned from the characteristic function.

  The distribution function is
  
  \deqn{F(x)=\frac{1}{2} + \frac{1}{\pi} \int_0^\infty \frac{r(s)}{s}
             sin(sx - \theta(s))ds}
  
  Generation of random observations from the Generalised Normal Laplace
  distribution using \code{rgnl} is based upon the composition of three
  random variables, \eqn{Z}, \eqn{G_1} and \eqn{G_2}. These are independent with
  \eqn{Z \sim N(0, 1)} and \eqn{G_1}, \eqn{G_2} being gamma random variables with a scale of 1
  and a shape of \eqn{\rho}.
  
  A GNL random variable, \eqn{X \sim \textrm{GNL}(\mu, \sigma, \alpha, \beta, \rho)} can
  be represented as:
  
  \deqn{X = \rho\mu + \sigma\sqrt{\rho}Z + \frac{1}{\alpha}G_1 - \frac{1}{\beta}G_2}
}
\value{
  \code{dgnl} gives the density function, \code{pgnl} gives the
  distribution function, \code{qgnl} gives the quantile function and
  \code{rgnl} generates random variates.
}
\references{
  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  \enc{Birkhäuser}{Birkhauser},
  Boston.
  
  William J. Reed. (2008) Maximum Likelihood Estimation for Brownian-Laplace
  Motion and the Generalized Normal-Laplace (GNL) Distribution.
  Submitted to COMPSTAT 2008.
}

\author{
  Simon Potter
}

\examples{
param <- c(0, 1, 3, 2, 1)
par(mfrow = c(1, 2))

## Curves of density and distribution
curve(dgnl(x, param = param), -5, 5, n = 1000)
title("Density of the Generalised Normal Laplace Distribution")
curve(pgnl(x, param = param), -5, 5, n = 1000)
title("Distribution Function of the Generalised Normal Laplace Distribution")

## Example of density and random numbers
par(mfrow = c(1, 1))
param1  <- c(0, 1, 1, 1, 1)
data1  <- rgnl(1000, param = param1)
curve(dgnl(x, param = param1),
      from = -5, to = 5, n = 1000, col = 2)
hist(data1, freq = FALSE, add = TRUE)
title("Density and Histogram")
}
\keyword{distribution}
