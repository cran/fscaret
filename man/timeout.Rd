\name{timeout}
\alias{timeout}
\title{
timeout
}
\description{
This function limits elapsed time spent on single model development. It uses low-level functions of parallel packege and sets the fork process with time limit. If the result is not returned within set time, it kills fork. Function shouldn't be called from R console.
The function is not used under Windows OS. Only Unix-like systems have fork functionality.
}
\usage{
timeout(..., seconds)
}

\arguments{
  \item{...}{
Expression to be time limited
}
  \item{seconds}{
Number of seconds
}

}
\author{
Original code by Jeroen Ooms <jeroen.ooms at stat.ucla.edu> of OpenCPU package.
Modifications by Jakub Szlek and Aleksander Mendyk.
}

\examples{
## The function is currently defined as

function(..., seconds) {
  
  fork_to_check <- parallel::mcparallel(
    {eval(...)},
    silent = FALSE)
  
  # call mccollect to wait "seconds" for returning result of mcparallel.
  my_result <- parallel::mccollect(fork_to_check, wait = FALSE, timeout = seconds)
  # If my_result is returned kill fork
  tools::pskill(fork_to_check$pid, tools::SIGKILL)
  tools::pskill(-1 * fork_to_check$pid, tools::SIGKILL)
  
  # kill the fork of forks if they were spawned
  parallel::mccollect(fork_to_check, wait = FALSE)
  # If the function mccollect had NULL (timedout), make stop
  if (is.null(my_result))
    stop("Time limit has reached!")
  
  # remove list format
  my_result <- my_result[[1]]
  
  # return result
  return(my_result)
}
}

\keyword{ methods }
\keyword{ error }