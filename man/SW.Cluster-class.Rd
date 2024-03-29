% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SW.Cluster.R
\docType{class}
\name{SW.Cluster-class}
\alias{SW.Cluster-class}
\alias{SW.Cluster}
\title{A Reference Class to represent a cluster in a stepped-wedge cluster-randomised trial.}
\description{
A Reference Class to represent a cluster in a stepped-wedge cluster-randomised trial.
}
\section{Fields}{

\describe{
\item{\code{name}}{The name of the site.}

\item{\code{cluster.start.time}}{When data from the cluster starts being collected (usually start of study).}

\item{\code{transition.start.time}}{When the transition period starts (i.e. intervention started rolling out, but not finished).}

\item{\code{intervention.start.time}}{When the intervention is considered implemented.}

\item{\code{cluster.end.time}}{After which data is not included.}

\item{\code{sites}}{List of SW.Site included in the cluster.}

\item{\code{cluster.dt}}{A data.table with all sites.}
}}

\section{Methods}{

\describe{
\item{\code{add.sites(sites.to.add)}}{Add sites to the cluster, sites should be provided as a SW.Site object.}

\item{\code{get.name()}}{Get cluster name.}

\item{\code{get.site.by.name(site.name)}}{Get a site given it's name.}

\item{\code{get.site.names()}}{Get sites in the cluster.}

\item{\code{get.sites()}}{Get sites in the cluster.}

\item{\code{initialize(
  name = "Cluster",
  cluster.start.time = NaN,
  transition.start.time = NaN,
  intervention.start.time = NaN,
  cluster.end.time = NaN
)}}{Initialize cluster with a name.}

\item{\code{set.sites(sites)}}{Replace sites in the cluster, sites should be provided as a SW.Site object.}
}}

