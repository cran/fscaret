\name{RMSE}
\alias{RMSE}
\title{
RMSE
}
\description{
Function calculates root mean squared error.
}
\usage{
RMSE(vect1, vect2, rows_no)
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
Aleksander Mendyk
}

\examples{
## The function is currently defined as
function (vect1, vect2, rows_no) 
{
    result = 0
    obs <- 0
    pred <- 0
    for (i in 1:rows_no) {
        result <- result + (vect1[i] - vect2[i])^2
    }
    result <- (result/rows_no)^0.5
    return(result)
  }
}

\keyword{ math }
\keyword{ arith }
