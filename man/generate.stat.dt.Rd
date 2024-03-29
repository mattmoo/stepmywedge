% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{generate.stat.dt}
\alias{generate.stat.dt}
\title{Construct a statistic distribution generated by permuting sites to different
clusters.}
\usage{
generate.stat.dt(
  max.r,
  outcome.col.name,
  intervention.col.name,
  data.dt,
  cluster.dt,
  study.dt,
  site.dt,
  perm.dt = NULL,
  stat.dt.level = c("study", "cluster", "site")[1],
  equal.weight.site = F,
  equal.weight.cluster = F,
  statistic = "WMWU",
  other.predictors = NULL,
  progress.bar = T,
  ...
)
}
\arguments{
\item{max.r}{How many permutations?}

\item{outcome.col.name}{Name of the column containing the outcome}

\item{intervention.col.name}{NAme of the column containing the intervention
(i.e. has levels "control" and "intervention")}

\item{statistic}{Can be WMWU, ANOVA, mean_diff, chisq, or risk_ratio.}

\item{...}{Passed on to the test}

\item{stat.per.site}{If TRUE, the statistic will be calculated separately for
each site on each permutation, if not site will be ignored. MAy not be a
good idea to use TRUE}
}
\value{
A data.table with the statistic value at each permutation (with zero
  as the unpermuted comparison). May be divided by site if requested.
}
\description{
Statistic is Wilcoxon-Mann-Whitney at the moment.
}
