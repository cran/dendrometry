% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{decreaseMetric}
\alias{decreaseMetric}
\title{Metric scrolling or decay}
\usage{
decreaseMetric(dmh, dbh, mh, bh = 1.3)
}
\arguments{
\item{dmh}{numeric, the diameter at middle height.}

\item{dbh}{numeric, the diameter at breast height.}

\item{mh}{numeric, the middle (or cut) height.}

\item{bh}{Either a numeric value standing for the breast height of all trees or a numeric vector standing for the breast height of each tree. Default is \code{1.3}.}
}
\value{
Metric decay
}
\description{
The average metric decay expresses the difference, in
centimeters per meter, between the diameter (or circumference) at breast
height and its diameter at mid-height of a stem related to the difference
between the height at mid-height and that at breast height.
}
\examples{
decreaseMetric(dmh = 40, dbh = 90, mh = 7)
decreaseMetric(45, 85, 9)
}
