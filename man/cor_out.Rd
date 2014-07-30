\name{cor_out}
\alias{cor_out}
\title{Format Correlation Test Statistics}
\description{Distilles the most relevant data from an output object of \code{cor.test} and displays the results in a compact format.}
\usage{
cor_out(coroutput, stats = FALSE, print = TRUE)
}
\arguments{
  \item{coroutput}{Output object created by a call to \code{cor.test}.}
  \item{stats}{If \code{TRUE}, the output includes t-values and corresponding degrees of freedom (default=\code{FALSE}).}
  \item{print}{Display results (logical; default=\code{TRUE}).}
}
\details{
The output of a call to \code{cor.test} is formatted according to the guidelines of the APA (American Psychological Association) as well as the DGPs ("Deutsche Gesellschaft fuer Psychologie"; German Psychological Society).
}
\value{
  \code{cor_out(coroutput,...)} returns a line containing the formatted correlation results.
}
% \references{}
\author{Markus Janczyk, Roland Pfister}
% \note{}

\seealso{
  \code{\link{cor}}; \code{\link{cor.test}}; \code{\link{anova_out}}; \code{\link{chi_out}}; \code{\link{t_out}};
}
% \examples{}
% \keyword{ }
