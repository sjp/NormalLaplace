\name{gnlParam}
\docType{data}
\alias{gnlParam}
\alias{gnlLargeParam}
\alias{gnlSmallParam}
\alias{gnlLargeShape}
\alias{gnlSmallShape}
\title{Test parameter sets for the Generalised Normal Laplace distribution}
\description{
Datasets have been created for the purpose of testing functions related to
the Generalised Normal Laplace distribution. These test a wide range of possible parameters.

\code{gnlLargeParam} and \code{gnlSmallParam} vary over all of the Generalised Normal Laplace
parameters while \code{gnlLargeShape} and \code{gnlSmallShape} vary over all parameters except
\code{mu} (\code{mu} is fixed at 0).
}

\format{
  The \code{gnlParam} data are all matrices that each have the following columns:
  \tabular{rl}{
    [, 1] \tab mu \cr
    [, 2] \tab sigma \cr
    [, 3] \tab alpha \cr
    [, 4] \tab beta \cr
    [, 5] \tab rho \cr
  }
  
  The number of rows present in each matrix is:
  \tabular{ll}{
    gnlLargeParam \tab 31752 \cr
    gnlSmallParam \tab 108 \cr
    gnlLargeShape \tab 4536 \cr
    gnlSmallShape \tab 54 \cr
  }
}
\usage{data(gnlParam)}
\keyword{datasets}
