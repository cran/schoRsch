\name{chi_out}
\alias{chi_out}
\title{Format Chi-Squared Test Output}
\description{Distilles the most relevant data from an output object of \code{chisq.test} and displays the results in a compact format.}
\usage{
chi_out(chioutput, show.n = FALSE,
	print = TRUE)
}
\arguments{
  \item{chioutput}{Output object created by a call to \code{chisq.test}.}
  \item{show.n}{Display sample size (logical; default=\code{FALSE})}
  \item{print}{Display results (logical; default=\code{TRUE}).}
}
\details{
The output of a call to \code{chi_out} is formatted according to the guidelines of the APA (American Psychological Association) as well as the DGPs ("Deutsche Gesellschaft fuer Psychologie"; German Psychological Society).
}
\value{
  \code{chi_out(chioutput,...)} returns a data.frame containing (1) a description of the test and (2) a line with formatted results.
}
% \references{}
\author{Daniel Gromer}
% \note{}

\seealso{
  \code{\link{chisq.test}}; \code{\link{anova_out}}; \code{\link{cor_out}}; \code{\link{t_out}};
}
% \examples{}
% \keyword{ }
