\name{ntiles}
\alias{ntiles}
\title{Split distribution into quantiles}
\description{The data of a variable are rank-ordered and split to bins of (approximately) equal size. When tied ranks span across category borders, the function assigns all values to the lowest possible bin. This procedure can result in slightly different results as the corresponding function \code{Rank Cases} of SPSS with option \code{Ntiles}. }
\usage{
ntiles(data, dv, 
       factors = NaN,
       bins = 5,
       res.labels = FALSE)
}
\arguments{
  \item{data}{A data frame containing the data relevant variable and possible factors that can be used to split the data frame into separate compartments.}
  \item{dv}{Character string specifying the name of the variable within \code{data} that is to be cut in bins. Alternatively, \code{dv} can be the appropriate column index.}
  \item{factors}{A string or vector of strings (e.g., \code{c("subject","condition")}) stating the conditions that should be used for splitting the data.}
  \item{bins}{The number of bins to be generated. Alternatively, a vector of cut-points can be specified according to the \code{break} argument of the function \code{\link{cut}}.}
  \item{res.labels}{The default value \code{FALSE} returns the bin number for each observation whereas \code{TRUE} returns the corresponding interval borders (in ranks).}
}
% \details{}
\value{
  \code{ntiles(data, dv, ...)} returns a vector of bins.
}
% \references{}
\author{Roland Pfister; Markus Janczyk}
% \note{}

\seealso{
  \code{\link{cut}}; \code{\link{rank}}; \code{\link{split}}; \code{\link{lapply}};
}
\examples{
## Build data frame
var1 <- c(1:9)
var2 <- c(1,1,1,2,2,2,3,3,3)
tmpdata <- data.frame(cbind(var1,var2))
tmpdata$var2 <- as.factor(tmpdata$var2)

## Get overall bins and display result
tmpdata$bins <- ntiles(tmpdata, dv = "var1", bins=3)
tmpdata

## Get bins separately for each factor level
## and display result
tmpdata$bins2 <- ntiles(tmpdata, dv = "var1", bins=3, factors = "var2")
tmpdata
}
% \keyword{ }
