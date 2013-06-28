\name{NEWS}
\title{News for Package \pkg{fscaret}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}


\section{Changes in version 0.8.5.2}{
\itemize{ 

\item A bug found by Aleksander Mandyk fixed. If dependent package caret wasn't
      changed at time when other dependencies were changing the function stopped.
      It was fixed making the predict() fn with try() clause,
      but the resulting generalization error is NA and then in impCalc() it gets
      x100000 less rank than the worse model.

\item A vignette was added.

\item A NEWS log was added.

\item A bug related to double output "Model calculation failed! ..." was fixed.

 }  
}