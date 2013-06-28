\name{fscaret-package}
\alias{fscaret-package}
\docType{package}
\title{
Automated feature selection caret (fscaret)
}
\description{
This package provide fast and automated feature selection based on caret package modeling methods.
The main advantage of this extension is that it requires minimum user involvement.
Also the variety of used methods in combination with the scaling according to RMSE or MSE obtained from models profit the user.
The idea is based on the assumption that the variety of models will balance the roughness of calculations (default model settings are applied).
On Windows OS the time limiting function is off also the number of cores used is set to 1.
}
\details{
\tabular{ll}{
Package: \tab fscaret\cr
Type: \tab Package\cr
Version: \tab 0.8.5.2\cr
Date: \tab 2013-06-23\cr
License: \tab GPL-2 | GPL-3\cr
}
}
\author{
Jakub Szlek <j.szlek@uj.edu.pl>, acknowledgments to Aleksander Mendyk, contributions from stackoverflow and r-help@r-project.org mailing list community.

Maintainer: Jakub Szlek <j.szlek@uj.edu.pl>.
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.
}
\keyword{ package }
\seealso{
\code{\link[caret]{train}}, \code{\link[caret]{trainControl}}, \code{\link[caret]{rfeControl}} 
by Max Kuhn <Max.Kuhn at pfizer.com> and \code{\link{predict}} base utilities
}