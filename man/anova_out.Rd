\name{anova_out}
\alias{anova_out}
\title{Format ANOVA Output}
\description{Distilles the most relevant data from an output object of \code{ezANOVA} and displays the results in a compact format.}
\usage{
anova_out(ezout, print = TRUE, sph.cor = "GG", mau.p = 0.05,
          etasq = "partial", dfsep = ", ", corr.df = FALSE, show.eps = 0)
}
\arguments{
  \item{ezout}{Output object created by a call to \code{ezANOVA}. This call has to have included a detailed output (\code{detailed=TRUE}).}
  \item{print}{Force results to be displayed, even if the function output is assigned to a variable (e.g., \code{output <- anova_out(...)}; logical; default=\code{TRUE}).}
  \item{sph.cor}{Correction method (one of \code{"no","GG","HF"}; default=\code{"GG"}).}
  \item{mau.p}{Threshold for Mauchly's test of sphericity (numerical; default=\code{0.05}).}
  \item{etasq}{Effect size estimate to be used; either partial eta-squared (\code{"partial"; default}) or generalized eta-squared (\code{"generalized"}).}
  \item{dfsep}{String that delimits the degrees of freedom of each F-value (default=\code{", "}).}
  \item{corr.df}{Display corrected degrees of freedom when Mauchly's test of sphericity is significant (default=\code{FALSE}).}
  \item{show.eps}{Show epsilon estimates when Mauchly's test of sphericity is significant? \code{0} = do not show, \code{1} = print after denominator dfs, \code{2} = print after F-value, \code{3} = print after effect size (default=\code{0}).}
}
\details{
The output of a call to \code{ezANOVA} is formatted according to the guidelines of the APA (American Psychological Association) as well as the DGPs ("Deutsche Gesellschaft fuer Psychologie"; German Psychological Society).\cr\cr
For repeated-measures ANOVAs, sphericity corrections are automatically applied to the p-values of effects that produced a significant result in Mauchly's test of sphericity. The corresponding input arguments (\code{sph.cor} and \code{mau.p}) do not affect between-subject designs.
}
\value{
  \code{anoout(ezout,...)} returns a list containing (1) the ANOVA table, (2) sphericity tests and corrections (if applicable), (3) formatted ANOVA results, (4) notes about which correction was applied to which effect.
}
% \references{}
\author{Roland Pfister, Markus Janczyk}
% \note{}

\seealso{
  \code{\link[ez:ezANOVA]{ezANOVA}}; \code{\link{aov}}; \code{\link{chi_out}}; \code{\link{cor_out}}; \code{\link{t_out}};
}
% \examples{}
% \keyword{ }
