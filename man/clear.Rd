\name{clear}
\alias{clear}
\title{Clear Global Workspace}
\description{The global workspace is cleared; \code{clear} is a shortcut for the usual \code{rm(list=ls())}.}
\usage{
clear()
}
%\arguments{}
%\details{ }
%\value{ }
%\references{}
\author{Roland Pfister, Markus Janczyk}
% \note{}

\seealso{
  \code{\link{rm}}; \code{\link{ls}};
}
\examples{
## Declare variables
a <- 1
b <- "abc"
ls()

## Clear workspace
clear()
ls()
}
\keyword{utilities}
