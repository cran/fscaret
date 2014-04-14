\name{NEWS}
\title{News for Package \pkg{fscaret}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 0.8.5.6}{
\itemize{

\item Small bug for parameter no.cores was fixed, now one should use numeric format, leave in NULL or simple delete the parameter.

}

}

\section{Changes in version 0.8.5.5}{
\itemize{

\item Information on founding bodies were added to description section and vigniette together with references.

}

}

\section{Changes in version 0.8.5.4}{
\itemize{

\item Multiple threads CRAN check fixed

\item Citation file added

}

}

\section{Changes in version 0.8.5.3}{
\itemize{ 

\item A buggy code creating vignette on some OS's was fixed
\item Probably buggy code reading data frame header was fixed.

}

}

\section{Changes in version 0.8.5.2}{
\itemize{ 

\item A bug found by Aleksander Mandyk fixed. If dependent package caret wasn't
      changed at time when other dependencies were changing the function stopped.
      It was fixed making the predict() fn with try() clause,
      but the resulting generalization error is NA and then in impCalc() it gets
      x100000 less rank than the worst model. Aleksander found another bug related
      models using java (RWeka especially). Also fixed.

\item A vignette was added.

\item A NEWS log was added.

\item A bug related to double output "Model calculation failed! ..." was fixed.

 }  
}

