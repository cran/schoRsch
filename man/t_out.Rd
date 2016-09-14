\name{t_out}
\alias{t_out}
\title{Format t-Test Output}
\description{Distilles the most relevant data from an output object of \code{t.test} and displays the results in a compact format.}
\usage{
t_out(toutput, n.equal = TRUE,
	welch.df.exact = TRUE, welch.n = NA,
	d.corr = TRUE, print = TRUE)
}
\arguments{
  \item{toutput}{Output object created by a call to \code{t.test}.}
  \item{n.equal}{Only applicable to two-sample t-tests. If sample sizes are not equal, \code{n.equal} specifies a vector of sample sizes, e.g., \code{n.equal = c(12,8)}.}
  \item{welch.df.exact}{Only applicable to Welch-tests. Indicates whether Welch-adjusted or unadjusted degrees of freedom (dfs) are reported (default=\code{TRUE}, i.e., Welch-adjusted dfs). If set to \code{FALSE}, the parameter \code{welch.n} has to be set as well.}
  \item{welch.n}{Only applicable to Welch-tests with unadjusted degrees of freedom. Parameter should be equal to the total sample size \code{n=n_1+n_2}.}
  \item{d.corr}{Only applicable to one-sample or paired-samples t-tests. If \code{TRUE} (default), Cohen's ds are computed using \code{sqrt(2)}-correction.}
  \item{print}{Force results to be displayed, even if the function output is assigned to a variable (e.g., \code{output <- t_out(...)}; logical; default=\code{TRUE}).}
}
\details{
The output of a call to \code{t_out} is formatted according to the guidelines of the APA (American Psychological Association) as well as the DGPs ("Deutsche Gesellschaft fuer Psychologie"; German Psychological Society).
}
\value{
  \code{t_out(toutput,...)} returns a list containing (1) a description of the t-test (two-sample t-test, Welch-test, paired-samples t-test, one-sample t-test) and (2) a line with formatted results.
}
% \references{}
\author{Roland Pfister, Markus Janczyk}
% \note{}

\seealso{
  \code{\link{t.test}}; \code{\link{anova_out}}; \code{\link{chi_out}}; \code{\link{cor_out}};
}
% \examples{}
% \keyword{ }
