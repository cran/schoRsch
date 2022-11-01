\name{clear_all_but}
\alias{clear_all_but}
\title{Clear Parts of Global Workspace}
\description{The global workspace is cleared while keeping (only) selected variables.}
\usage{
clear_all_but(keep = NULL)
}
\arguments{
  \item{keep}{Variables to keep. Specified as a vector of strings.}
}
\details{
An R version of the eponymous custom MATLAB function (https://de.mathworks.com/matlabcentral/fileexchange/25339-clear-all-but).
}
%\value{ }
%\references{}
\author{Moritz Schaaf}
% \note{}

\seealso{
  \code{\link{clear}}; \code{\link{rm}}; \code{\link{ls}};
}
\examples{
## Declare variables
a <- 1
b <- "abc"
c <- NA
ls()

## Clear workspace
clear_all_but(c("a","b"))
ls()
}
\keyword{utilities}
