\name{MSE}
\alias{MSE}
\title{
MSE
}
\description{
Function calculates mean squared error as predicted vs. observed
}
\usage{
MSE(vect1, vect2, rows_no)
}

\arguments{
  \item{vect1}{
Numeric vector of predicted values
}
  \item{vect2}{
Numeric vector of observed values
}
  \item{rows_no}{
Number of observations
}
}
\author{
Jakub Szlek and Aleksander Mendyk
}

\examples{

## The function is currently defined as
function (vect1, vect2, rows_no) 
{
    result = 0
    pred <- 0
    obs <- 0
    for (i in 1:rows_no) {
        result <- result + (vect1[i] - vect2[i])^2
    }
    result <- (result/rows_no)
    return(result)
  }
}

\keyword{ math }
\keyword{ arith }
