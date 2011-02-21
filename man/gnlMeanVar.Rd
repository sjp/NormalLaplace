\name{GeneralisedNormalLaplaceMeanVar}
\alias{GeneralisedNormalLaplaceMeanVar}
\alias{gnlMean}
\alias{gnlVar}
\alias{gnlSkew}
\alias{gnlKurt}

\title{
Mean, Variance, Skewness and Kurtosis of the Generalised Normal Laplace Distribution.
}
\description{
  Functions to calculate the mean, variance, skewness and kurtosis
  of a specified Generalised Normal Laplace distribution.
}
\usage{
gnlMean(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
        param = c(mu, sigma, alpha, beta, rho))
gnlVar(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
         param = c(mu, sigma, alpha, beta, rho))
gnlSkew(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
         param = c(mu, sigma, alpha, beta, rho))
gnlKurt(mu = 0, sigma = 1, alpha = 1, beta = 1, rho = 1,
         param = c(mu, sigma, alpha, beta, rho))

}
\arguments{
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{sigma}{Scale parameter \eqn{\sigma}{sigma}, default is 1.}
  \item{alpha}{Tail parameter \eqn{\alpha}{alpha}, default is 1.}
  \item{beta}{Tail parameter \eqn{\beta}{beta}, default is 1.}
  \item{rho}{Scale parameter \eqn{\rho}{rho}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu, sigma, alpha, beta, rho)}.}
}

\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones.

  The mean function is
  \deqn{E(X)= \rho\left(\mu + \frac{1}{\alpha} - \frac{1}{\beta}\right)}%
       {E(X) = rho(mu + 1/alpha - 1/beta)}

  The variance function is
  \deqn{Var(X)= \rho\left(\sigma^2 + \frac{1}{\alpha^2} + \frac{1}{\beta^2}\right)}%
       {Var(X) = rho(sigma^2 + 1/alpha^2 + 1/beta^2)}

  The skewness function is
  \deqn{\Upsilon = \frac{2(\beta^3 - \alpha^3)}{\rho^{\frac{1}{2}}(\sigma^2\alpha^2\beta^2 + \alpha^2 + \beta^2)^2}}%
       {skewness = (2(beta^3 - alpha^3)) / (rho^(1/2)*(sigma^2*alpha^2*beta^2 + alpha^2 + beta^2)^2}

  The kurtosis function is
  \deqn{\Gamma = \frac{6(\alpha^4 + \beta^4)}{\rho(\sigma^2\alpha^2\beta^2 + \alpha^2 + \beta^2)^2}}%
       {kurtosis = (6(alpha^4 + beta^4)) / (rho(sigma^2*alpha^2*beta^2 + alpha^2 + beta^2)^2}
}
\value{
  \code{gnlMean} gives the mean of the Generalised Normal Laplace,
  \code{gnlVar} the variance, \code{gnlSkew} the skewness and \code{gnlKurt}
  the kurtosis.
}

\references{

  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  \enc{Birkhäuser}{Birkhauser},  Boston.

}

\author{
  Simon Potter
}

\examples{
param <- c(10, 1, 5, 9, 2)
gnlMean(param = param)
gnlVar(param = param)
gnlSkew(param = param)
gnlKurt(param = param)

curve(dgnl(x, param = param), -10, 10)
}
\keyword{distribution}
