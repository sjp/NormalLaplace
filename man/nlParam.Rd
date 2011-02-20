\name{nlParam}
\docType{data}
\alias{nlParam}
\alias{nlLargeParam}
\alias{nlSmallParam}
\alias{nlLargeShape}
\alias{nlSmallShape}
\title{Test parameter sets for the Normal Laplace distribution}
\description{
Datasets have been created for the purpose of testing functions related to
the Normal Laplace distribution. These test a wide range of possible parameters.

\code{nlLargeParam} and \code{nlSmallParam} vary over all of the Normal Laplace
parameters while \code{nlLargeShape} and \code{nlSmallShape} vary over all parameters except
\code{mu} (\code{mu} is fixed at 0).
}

\format{
  The \code{nlParam} data are all matrices that each have the following columns:
  \tabular{rl}{
    [, 1] \tab mu \cr
    [, 2] \tab sigma \cr
    [, 3] \tab alpha \cr
    [, 4] \tab beta \cr
  }
  
  The number of rows present in each matrix is:
  \tabular{ll}{
    nlLargeParam \tab 3969 \cr
    nlSmallParam \tab 36 \cr
    nlLargeShape \tab 567 \cr
    nlSmallShape \tab 18 \cr
  }
}
\usage{data(nlParam)}
\keyword{datasets}
