\name{summary.gnlFit}
\alias{summary.gnlFit}
\alias{print.summary.gnlFit}

\title{Summarizing Generalised Normal Laplace Distribution Fit}
\description{
  \code{summary} Method for class \code{"gnlFit"}.
}

\usage{
  \method{summary}{gnlFit}(object, ...)
  \method{print}{summary.gnlFit}(x,
        digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{object}{An object of class \code{"gnlFit"},
    resulting from a call to \code{\link{gnlFit}}.}
  \item{x}{An object of class \code{"summary.gnlFit"}
    resulting from a call to \code{summary.gnlFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}

\details{
  \code{summary.gnlFit} calculates standard errors for the estimates
  of \eqn{\mu}{mu}, \eqn{\sigma}{sigma}, \eqn{\alpha}{alpha}, \eqn{\beta}{beta}
  and \eqn{\rho}{rho} of the Generalised Normal Laplace distribution parameter vector
  \code{param} if the Hessian from the call to \code{\link{gnlFit}} is available.
}

\value{
  If the Hessian is available, \code{summary.gnlFit} computes
  standard errors for the estimates of \eqn{\mu}{mu}, \eqn{\sigma}{sigma},
  \eqn{\alpha}{alpha}, \eqn{\beta}{beta} and \eqn{\rho}{rho} and adds them to \code{object}
  as \code{object$sds}. Otherwise, no calculations are performed and the
  composition of \code{object} is unaltered.

  \code{summary.gnlFit} invisibly returns \code{object}
  with class changed to \code{summary.gnlFit}.

  See \code{\link{gnlFit}} for the composition of an object of class
  \code{gnlFit}.

  \code{print.summary.gnlFit} prints a summary in the same format as
  \code{\link{print.gnlFit}} when the Hessian is not available from
  the fit. When the Hessian is available, the standard errors for the
  parameter estimates are printed in parentheses beneath the parameter
  estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.
}

\seealso{
  \code{\link{gnlFit}}, \code{\link{summary}}.
}

\examples{
## Continuing the  gnlFit() example:
param <- c(2, 2, 1, 1, 1)
dataVector <- rgnl(500, param = param)
fit <- gnlFit(dataVector, hessian = TRUE)
print(fit)
summary(fit)
}

\keyword{distribution}
