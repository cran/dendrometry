% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{loreyHeight}
\alias{loreyHeight}
\title{Lorey's mean height}
\usage{
loreyHeight(basal, height)
}
\arguments{
\item{basal}{numeric, individual basal areas.}

\item{height}{numeric vector of individual heights.}
}
\value{
Average Lorey height of a species.
}
\description{
The average height of the trees in a plot, weighted by their basal area.
}
\examples{
set.seed(1)
donnee <- data.frame(hauteur = rnorm(10, 12, 3), area = basal_i(rnorm(10, 100, 20)))
loreyHeight(basal = donnee$area, height = donnee$hauteur)
}
\seealso{
\code{\link{height}}, \code{\link{basal_i}}
}
