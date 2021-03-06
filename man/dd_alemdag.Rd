% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance-dependent.R
\name{dd_alemdag}
\alias{dd_alemdag}
\title{Alemdag index}
\usage{
dd_alemdag(id, x, y, dbh, search = "nearest", n)
}
\arguments{
\item{id}{tree identification}

\item{x}{x numeric vector of tree coordinates.}

\item{dbh}{diameter of trees at 1.3 meters.}

\item{search}{method to search competitive trees. Use "nearest" to find
the \code{n} trees nearest of object tree or "dfixed" to find any tree around
of \code{n} distance.}
}
\description{
Sum of circular segments calculated between the object tree and
its competitors.
}
\references{
ALEMDAG, I.S., 1978. Evaluation of some competition indexes
for the predictions of diameter increment in planted white spruce. Inf.
Rep. FMR-X-108, Canadian Forestry Management Institute, Ottawa, Ont., 39 pp.
}
