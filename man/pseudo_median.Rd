% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{pseudo_median}
\alias{pseudo_median}
\title{Cut down version of wilcox.test that only returns the pseudo-median without
CIs. Uses Rfast for a little bit of a speed boost.}
\usage{
pseudo_median(x, max_length = 20000)
}
\arguments{
\item{x}{Vector of values.}

\item{max_length}{Because the outer product is calculated, large numbers of
observations will result in a vector that cannot be allocated.}
}
\value{
Pseudo-median of x
}
\description{
Cut down version of wilcox.test that only returns the pseudo-median without
CIs. Uses Rfast for a little bit of a speed boost.
}
