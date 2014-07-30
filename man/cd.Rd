\name{cd}
\alias{cd}
\title{Change Directory}
\description{Performs relative changes of the working directory. Calling \code{cd("..")} moves one level up in the hierarchy whereas \code{cd("folder_name")} moves one level down to the designated folder.}
\usage{
cd(x)
}
\arguments{
  \item{x}{: A character string corresponding to target directory or \code{".."}.
}
}
\details{
\code{cd} is designed as an equivalent to the DOS command. Contrary to the common use of cd, however, this function does not take absolute paths as input. Use \code{setwd} instead to navigate to an absolute path.
}
\value{
  \code{cd(x)} returns the new working directory.
}
% \references{}
\author{Roland Pfister, Markus Janczyk}
% \note{}

\seealso{
  \code{\link{getwd}}; \code{\link{setwd}};
}
\examples{
## Create temporary folder
dir.create("a_test_dir")

## Navigate into the new folder...
cd("a_test_dir")
## ... and back again
cd("..")

## Remove temporary folder
unlink("a_test_dir",recursive=TRUE)
}
\keyword{utilities}
