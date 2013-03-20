\name{zscores}
\alias{zscores}
\title{Compute z-Scores by Condition}
\description{Data of an input vector is transformed to z-scores (mean = 0, sd = 1). The function operates on single vectors as well as on specified columns of a data frame.}
\usage{
zscores(data, factors=NaN, dv=NaN)
}
\arguments{
  \item{data}{Either a data frame containing the data of interest or a single vector.}
  \item{factors}{If called with \code{factors=NaN} (default), the entire data is processed according to its grand mean and total variance. If \code{data} is a vector, \code{factors} can be a list of variables for splitting the variable into separate compartments. If \code{data} is a data frame, \code{factors} has to be specified as a character vector of column names or column indices.}
  \item{dv}{If \code{data} is a single vector, \code{dv} does not have to be specified. If \code{data} is a data frame, \code{dv} indicates the column of the data frame which contains the variable for z-transformation (e.g., \code{dv="rt"}) or its column index (e.g., \code{dv=15}).}
}
\details{
\code{zscores} computes z-score of a vector or a specified column within a dataframe. Computation can be done separately for combinations of factors.
}
\value{
  \code{zscores()} returns a vector containing the requested z-scores.
}
% \references{}
\author{Roland Pfister, Markus Janczyk}
% \note{}

\seealso{
  \code{\link{scale}}; \code{\link{split}}; \code{\link{outlier}};
}
\examples{
# Create input vector and compute z-scores
measurements <- c(3,12,5,4,2,23,1,6)
zscores(measurements)

# Compute z-scores separately
# for conditions
cond1 <- c(1,1,1,1,2,2,2,2)
cond2 <- c(1,1,2,2,1,1,2,2)
zscores(measurements,list(cond1))
zscores(measurements,list(cond1,cond2))

# Calling zscores for data frames
data <- data.frame(measurements,
	cond1,cond2)
zscores(data,dv="measurements",
	factors=c("cond1","cond2"))
	
# Operating on column indices
zscores(data,dv=1,
	factors=3)

}
\keyword{utilities}
