\name{NEWS}
\title{NEWS file for the QDiabetes pacakge}
\section{Changes in version 0.1.1}{
  \itemize{
    \item Initial creation of a working version of the QDiabetes package.
  }
}
\section{Changes in version 0.1.2}{
  \itemize{
    \item Made the examples in the \code{\link{QDRA}}, \code{\link{QDRB}} and \code{\link{QDRC}} documentation a little more interesting.
    \item Set the postcode-to-Townsend backend dataset to load upon package load, as opposed to lazyloading upon first execution of the getTDS function.
    \item Added postcode data to the \code{\link{dat_qdr}} sample dataset to allow for exploration of the \code{\link{getTDS}} function.
  }
}
\section{Changes in version 0.1.3}{
  \itemize{
    \item Minor under-the-hood optimisations.
  }
}
\section{Changes in version 0.1.4}{
  \itemize{
    \item Added the QDiabetes-2013 algorithm.
    \item Altered all functions names to allow for easier inclusion of past and future updates to QDiabetes.
    \item Set all risk prediction functions to return a named vector when only one of inputs is of length > 1.
  }
}
\section{Changes in version 0.1.5}{
  \itemize{
    \item Expanded the abilities of the \code{\link{getTDS}} function to allow searching by postcode prefix, postcode suffix, and regular expressions. A \code{squash} parameter has also been added to \code{\link{getTDS}} to allow the user to specify whether an overall median should be produced for all Townsend deprivation scores matched.
  }
}
