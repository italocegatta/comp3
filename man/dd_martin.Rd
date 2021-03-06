% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance-dependent.R
\name{dd_martin}
\alias{dd_martin}
\title{Matin & Ek (1984) index}
\usage{
dd_martin(id, x, y, dbh, search = "nearest", n)
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
Sum of the ratio between the diameter of the competitor and
object tree multiplied by the exponential factor weighted by
the distance and diameter of the trees.
}
\references{
MARTIN, G.L.; EK, A.R., 1984. A comparison of competition
measures and growth models for predicting plantation red pine diameter
and height growth. Forest Science 30(3): 731-743.
}
