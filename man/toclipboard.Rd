\name{toclipboard}
\alias{toclipboard}
\title{Copy Data to Clipboard}
\description{A data frame of variable is written to the clipboard, allowing easy pasting to MS Excel and Open/Libre Office Calc. This function is a wrapper to \code{write.table} with pre-specified options for plug-and-play usage. Most options of \code{write.table} are also supported by \code{toclipboard}. Note: The current version of \code{toclipboard} only supports Windows systems; the function will not run under Linux or Mac OS.}
\usage{
toclipboard(data,
            sep = "\t", quote = FALSE,
            eol = "\n",  na = "NA",
            dec = ".", row.names = FALSE,
            col.names = TRUE,
			clipwarning = FALSE)
}
\arguments{
\item{data}{The first argument should be the data frame or variable that is to be written to the clipboard. Data frames are copied with column names but without row names and columns are separated by tabs. This behavior can be customized with the following optional arguments (passed to \code{\link{write.table}}).}
\item{sep}{Delimiter string.}
\item{quote}{Put quotes around strings?}
\item{eol}{End-of-line character.}
\item{na}{How should NA-values be written?}
\item{dec}{Decimal separator.}
\item{row.names}{Should row names be written?}
\item{col.names}{Should column names be written?}
\item{clipwarning}{Determine if warnings should overwrite clipboard content (especially in case of buffer overflow when attempting to copy large datasets to the clipboard.}
}
%\details{ }
%\value{ }
%\references{}
\author{Roland Pfister}
% \note{}

\seealso{
  \code{\link{write.table}}
}
\examples{
## Build data frame
var1 <- c(1:9)
var2 <- c(1,1,1,2,2,2,3,3,3)
tmpdata <- data.frame(cbind(var1,var2))

## Write data frame to clipboard
toclipboard(tmpdata)

## -> The data frame can now be pasted
## into any other application.
}
\keyword{utilities}
