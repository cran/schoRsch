\name{bimod_coef}
\alias{bimod_coef}
\title{Compute bimodality coefficient}
\description{Computes the bimodality coefficient for a vector of data points (for a tutorial, see Pfister et al., 2013, Frontiers in Quantitative Psychology and Measurement). }
\usage{
bimod_coef(data, moments = FALSE, na.rm = TRUE)
}
\arguments{
  \item{data}{A vector containing the data.}
  \item{moments}{A logical specifying whether the sample moments skewness and kurtosis should be contained in the output.}
  \item{na.rm}{A logical specifying whether NAs should be removed from the data vector.}  
}
% \details{}
\value{
  \code{bimod_coef(data)} returns a bimodality coefficient for the input data; \code{bimod_coef(data,moments=TRUE)} returns a vector with three elements: the bimodality coefficient, skewness of the data, and sample kurtosis of the data.
}
% \references{}
\author{Moritz Schaaf, Roland Pfister}
% \note{}

\seealso{
  \code{\link{rank}}; \code{\link{ntiles}};
}
\examples{
# Input slightly bimodal data
data <- c( 3, 5, 5, 5, 5, 7, 10, 17, 18, 18, 19, 19, 20)

# Show histogram
hist(data,breaks=c(0:20),include.lowest=FALSE,
     ylim=c(0,5),xlim=c(0,20))

# Compute bimodality coefficient
bimod_coef(data)

# Get bimodality coefficient, skewness, and kurtosis
bimod_coef(data,moments=TRUE)
}
% \keyword{ }
