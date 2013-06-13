\name{timeout}
\alias{timeout}
\title{
timeout
}
\description{
This function limits the cpu time spent on single model fitting. It simply sets the killing process of itself to sleep for chosen number of seconds. Shouldn't be called from R console.
}
\usage{
timeout(expr, seconds, my.pid)
}

\arguments{
  \item{expr}{
Expression to be time limited
}
  \item{seconds}{
Number of seconds
}
  \item{my.pid}{
Process PID
}
}
\author{
Jakub Szlek and Aleksander Mendyk with contribution to nabble users
}

\examples{
## The function is currently defined as
function (expr, seconds, my.pid) 
{
    killer.pid <- system(intern = TRUE, paste(" (sleep", seconds, 
        " ; kill -9", my.pid, ")>/dev/null&\n echo $!", sep = " "))
    on.exit(system(paste("kill", killer.pid, "> /dev/null 2>&1", 
        sep = " ")))
    withCallingHandlers(expr, interrupt = function(...) {
        stop("Timedout", call. = FALSE)
    })
  }
}

\keyword{ methods }
\keyword{ error }