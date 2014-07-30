\name{outlier}
\alias{outlier}
\title{Screen Data for Outliers}
\description{A chosen column of a data frame is screened for outliers, outliers are marked and/or eliminated. Either absolute lower and upper limits are applied, or outliers are identified based on z-transformed data.}
\usage{
outlier(data, dv, 
        todo = "na", res.name = "outlier",
        upper.limit = NaN, lower.limit = NaN,
        limit.exact = FALSE,
        upper.z = NaN, lower.z = NaN,
        z.exact = FALSE, factors = NaN,
        z.keep = TRUE, z.name = "zscores",
        vsj = FALSE,
        print.summary = TRUE)
}
\arguments{
  \item{data}{A data frame containing the data to be screened as well was appropriate condition variables.}
  \item{dv}{Character string specifying the name of the variable within \code{data} that is to be screened for outlier. Alternatively, \code{dv} can be the appropriate column index.}
  \item{todo}{Character string specifying the fate of outliers: \code{"na"} - outliers are turned into NAs, \code{"elim"} - rows containing outliers are deleted from dataframe, \code{"nothing"} - nothing happens, DEFAULT: \code{todo = "na"}.}
  \item{res.name}{Character string specifying the name of the variable wherein outliers are marked, DEFAULT: \code{res.name = "outlier"}.}
  \item{upper.limit}{An optional numerical specifying the absolute upper limit defining outliers.}
  \item{lower.limit}{An optional numerical specifying the absolute lower limit defining outliers.}
  \item{limit.exact}{Logical, if \code{TRUE} values equal to \code{lower.limit}/\code{upper.limit} are deemed outlier.}
  \item{upper.z}{An optional numerical specifying how much standard deviations within a cell a value must exceed to be identified as an outlier.}
  \item{lower.z}{An optional numerical specifying how much standard deviations within a cell a value must undercut to be identified as an outlier.}
  \item{factors}{A string or vector of strings (e.g., \code{c("subject","condition")}) stating the conditions that should be used for splitting the data. (Note: If one of the resulting combinations contains only one observation, a z-score is not defined and the function produces an error.)}
  \item{z.exact}{Logical, if \code{TRUE} z-values equal to \code{lower.z}/\code{upper.z} are deemed outlier.}
  \item{z.keep}{Logical, if \code{TRUE}, z-scores are stored in an additional column. If \code{FALSE}, z-scores are discarded after outlier correction.}
  \item{z.name}{Character string, specifying a name for the variable that should be used for storing z-scores.}
  \item{vsj}{To be implemented in a future version...}
  \item{print.summary}{Logical, if \code{TRUE}, a short summary on identified outliers is printed.}
}
\details{
If both, absolute limits and z-limits are specified, absolute limits are processed first and z-scores are computed for the remaining data points.
}
\value{
  \code{outlier(data,...)} returns the original data frame with the outlier correction applied. This data frame also has one additional column containing flags for outliers (\code{0} = not suspicious, \code{1} = outlier). If z-scores are requested, these scores are returned as an additional column.
}
% \references{}
\author{Markus Janczyk, Roland Pfister}
% \note{}

\seealso{
  \code{\link{split}}; \code{\link{zscores}};
}
% \examples{}
% \keyword{ }
