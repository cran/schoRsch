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
Version: \tab 1.10\cr
Date: \tab 2022-11-01\cr
License: \tab GPL-3\cr
}
This package contains the following functions:
\itemize{
  \item{\code{\link[schoRsch:anova_out]{anova_out}}: }{Formats the output object from \code{\link[ez:ezANOVA]{ezANOVA}} to the APA style (requires the \code{ez} package).}
  \item{\code{\link[schoRsch:cor_out]{cor_out}}: }{Formats the output object from \code{\link[stats:cor.test]{cor.test}} to the APA style.}
  \item{\code{\link[schoRsch:chi_out]{chi_out}}: }{Formats the output object from \code{\link[stats:chisq.test]{chisq.test}} to the APA style.}
  \item{\code{\link[schoRsch:t_out]{t_out}}: }{Formats the output object from \code{\link[stats:t.test]{t.test}} to the APA style.}
  \item{\code{\link[schoRsch:outlier]{outlier}}: }{Screens data for outliers, based on absolute values or z-scores. Outliers can either be marked or eliminated.}
  \item{\code{\link[schoRsch:ntiles]{ntiles}}: }{Split distribution into quantiles for distribution analysis.}
  \item{\code{\link[schoRsch:zscores]{zscores}}: }{Computes z-scores of values separately for defined design cells.}
  \item{\code{\link[schoRsch:cd]{cd}}: }{To easily change the current working directory.}
  \item{\code{\link[schoRsch:toclipboard]{toclipboard}}: }{Write data to clipboard (Windows only).}
  \item{\code{\link[schoRsch:clear]{clear}}: }{Clears the whole workspace (i.e., like \code{rm(list=ls())}).}
  \item{\code{\link[schoRsch:clear]{clear_all_but}}: }{Clears the whole workspace while keeping named variables.}
}
Version history:
\itemize{
  \item{v1.10 | 2022-11-01 | }{Added \code{clear_all_but} as kindly contributed by Moritz Schaaf.}
  \item{v1.9  | 2020-12-11 | }{Added argument \code{clipwarning} to \code{toclipboard}. Thanks to Moritz Schaaf for the feature request (v1.9.1 provided an instant bugfix to the new code).}
  \item{v1.8  | 2020-09-23 | }{Fix for the changed behavior of factor levels for strings as introduced in R 4.0 (relevant for \code{anova_out}). Thanks to Valentin Koob for sending in the bug report.}
  \item{v1.7  | 2019-11-12 | }{Bugfix for \code{anova_out} which crashed when assembling corrected degrees of freedom in certain cases after violations of sphericity. Thanks to Mirela Dubravac for sending in the bug report.}
  \item{v1.6  | 2019-05-02 | }{Bugfix for \code{cor_out} which did not display negative correlations with 0 > r > -0.1 correctly. Thanks to Mario Reutter for the bug report.}
  \item{v1.5  | 2018-12-15 | }{Default value for correcting effect sizes for paired-samples t-tests changed to \code{FALSE}; the use of corrections is now displayed as feedback message. Also: New options for \code{anova_out}; it is now possible to display corrected degrees of freedom for violations of the sphericity assumption and corresponding epsilon estimates. Thanks to Onur Asci for the feature request. Additional bugfix for the dfsep argument of \code{anova_out}.}
  \item{v1.4  | 2017-02-14 | }{Bugfix for \code{cor_out} that no longer displays leading zeros for correlation coefficients; thanks to Juan Ramon Barrada for sending in the bug report.}
  \item{v1.3  | 2016-09-13 | }{Overall documentation update based on comments from Vincent LeBlanc.}
  \item{v1.2  | 2015-07-05 | }{Bugfix for the \code{print} option of \code{anova_out}; thanks to Sylvain Clement for sending in the bug report. Minor code changes.}
  \item{v1.1  | 2014-07-30 | }{New functions \code{chi_out} (contributed by Daniel Gromer) and \code{toclipboard}; bugfixes when \code{anova_out} is called without detailed=TRUE. Updated help files.}
  \item{v1.0  | 2013-03-20 | }{Package release.}
}
}
\author{
Roland Pfister <mail(at)roland-pfister.net>, Markus Janczyk; 
}
\references{
Pfister, R., & Janczyk, M. (2016). schoRsch: An R package for analyzing and reporting factorial experiments. \emph{The Quantitative Methods for Psychology}, \emph{12}(2), 147-151. doi: 10.20982/tqmp.12.2.p147
}
\keyword{ package }
% \seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
% }
% \examples{
%% ~~ simple examples of the most important functions ~~
% }
