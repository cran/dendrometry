% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{diameterMean}
\alias{diameterMean}
\title{Mean diameter}
\usage{
diameterMean(dbh)
}
\arguments{
\item{dbh}{numeric, vector of diameter.}
}
\value{
Mean diameter.
}
\description{
Mean diameter of a forestry stand.
}
\examples{
set.seed(1)
diameter <- rnorm(10, 100, 20)
diameterMean(dbh = diameter)
}
\seealso{
\code{\link{dbh}}, \code{\link{basal_i}}
}
