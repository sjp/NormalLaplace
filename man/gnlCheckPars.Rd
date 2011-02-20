\name{gnlCheckPars}
\alias{gnlCheckPars}

\title{
  Check Parameters of the Generalised Normal Laplace Distribution
}
\description{
  Given a set of parameters for the Generalised Normal Laplace distribution,
  the functions checks the validity of each parameter and if they
  and if they correspond to the boundary cases.
}
\usage{
gnlCheckPars(param)
}
\arguments{
  \item{param}{Numeric. Parameter values for the Generalised Normal
    Laplace distribution.}
}
\details{
  The vector \code{param} takes the form \code{c(mu, sigma, alpha, beta, rho)}.

  If any of \code{sigma}, \code{alpha}, \code{beta} or \code{rho} is negative or NA, an
  error is returned.
}
\references{

  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  \enc{Birkhäuser}{Birkhauser},
  Boston.

}

\author{
  David Scott \email{d.scott@auckland.ac.nz}, Simon Potter
}

\examples{
## Correct parameters
gnlCheckPars(c(0, 1.5, 1, 2, 1))
gnlCheckPars(c(3, 1, 1.5, 2, 2))

## Incorrect parameters, each error providing a different error message
gnlCheckPars(c(2, -1, 1, 1, 1))             # invalid sigma
gnlCheckPars(c(2, 1, -1, 2, 1))             # invalid alpha
gnlCheckPars(c(0, 1, 2, -1, 1))             # invalid beta
gnlCheckPars(c(0, 1, 2, 1, -1))             # invalid rho
gnlCheckPars(c(0, -0.01, -0.1, 1, 1))       # sigma and alpha incorrect
gnlCheckPars(c(2, -0.5, 1, -0.2, 1))        # sigma and beta incorrect
gnlCheckPars(c(2, -0.5, 1, 0.2, -1))        # sigma and rho incorrect
gnlCheckPars(c(1, 1, -0.2, -1, 1))          # alpha and beta incorrect
gnlCheckPars(c(1, 1, -0.2, 1, -1))          # alpha and rho incorrect
gnlCheckPars(c(1, 1, 0.2, -1, -1))          # beta and rho incorrect
gnlCheckPars(c(1, -1, -0.2, -1, 1))         # sigma, alpha and beta incorrect
gnlCheckPars(c(1, -1, -0.2, 1, -1))         # sigma, alpha and rho incorrect
gnlCheckPars(c(1, -1, 0.2, -1, -1))         # sigma, beta and rho incorrect
gnlCheckPars(c(1, 1, -0.2, -1, -1))         # alpha, beta and rho incorrect
gnlCheckPars(c(0, -0.1, -0.2, -0.3, -0.4))  # all four parameters erroneous
gnlCheckPars(c(0.5, NA, 1, 1, 1))           # NA introduced
gnlCheckPars(c(-1, 1, 1))                   # incorrect number of parameters
}
\keyword{distribution}
