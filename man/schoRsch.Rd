\name{schoRsch}
\alias{schoRsch}
\alias{schoRsch-package}
\docType{package}
\title{
Tools for Analyzing Factorial Experiments
}
\description{
Offers a helping hand to psychologists and other behavioral scientists who routinely deal with experimental data from factorial experiments. It includes several functions to format output from other R functions according to the style guidelines of the APA (American Psychological Association). This formatted output can be copied directly into manuscripts to facilitate data reporting. These features are backed up by a toolkit of several small helper functions, e.g., offering out-of-the-box outlier removal. The package lends its name to Georg "Schorsch" Schuessler, ingenious technician at the Department of Psychology III, University of Wuerzburg.
}
\details{
\tabular{ll}{
Package: \tab schoRsch\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2015-07-05\cr
License: \tab GPL-3\cr
}
This package contains the following functions:
\itemize{
  \item{\code{\link[schoRsch:anova_out]{anova_out}}: }{Formats the output object from \code{\link[ez:ezANOVA]{ezANOVA}} to the APA style.}
  \item{\code{\link[schoRsch:cor_out]{cor_out}}: }{Formats the output object from \code{\link[stats:cor.test]{cor.test}} to the APA style.}
  \item{\code{\link[schoRsch:chi_out]{chi_out}}: }{Formats the output object from \code{\link[stats:chisq.test]{chisq.test}} to the APA style.}
  \item{\code{\link[schoRsch:t_out]{t_out}}: }{Formats the output object from \code{\link[stats:t.test]{t.test}} to the APA style.}
  \item{\code{\link[schoRsch:outlier]{outlier}}: }{Screens data for outliers, based on absolute values or z-scores. Outliers can either be marked or eliminated.}
  \item{\code{\link[schoRsch:ntiles]{ntiles}}: }{Split distribution into quantiles for distribution analysis.}
  \item{\code{\link[schoRsch:zscores]{zscores}}: }{Computes z-scores of values separately for defined design cells.}
  \item{\code{\link[schoRsch:cd]{cd}}: }{To easily change the current working directory.}
  \item{\code{\link[schoRsch:toclipboard]{toclipboard}}: }{Write data to clipboard (Windows only).}
  \item{\code{\link[schoRsch:clear]{clear}}: }{Clears the whole workspace (i.e., like \code{rm(list=ls())})}
}
Version history:
\itemize{
  \item{v1.2 | 2015-07-05 | }{Bugfix for the \code{print} option of \code{anova_out} (thanks to Sylvain Clement for sending in the bug report). Minor code changes.}
  \item{v1.1 | 2014-07-30 | }{Current version. New functions \code{chi_out} (contributed by Daniel Gromer) and \code{toclipboard}; bugfixes when \code{anova_out} is called without detailed=TRUE. Updated help files.}
  \item{v1.0 | 2013-03-20 | }{Package release.}
}
}
\author{
Roland Pfister, Markus Janczyk; <firstname.lastname(at)uni-wuerzburg.de>
}
% \references{}
\keyword{ package }
% \seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
% }
% \examples{
%% ~~ simple examples of the most important functions ~~
% }
