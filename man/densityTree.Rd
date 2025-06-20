% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{densityTree}
\alias{densityTree}
\title{Tree density}
\usage{
densityTree(number, area, overall = TRUE)
}
\arguments{
\item{number}{numeric, vector of tree count in each plot.}

\item{area}{numeric, area of a plot.}

\item{overall}{logical, if \code{TRUE}, an overall mean density is computed,
otherwise density is computed for each plot. Default is \code{TRUE}.}
}
\value{
Vector of density.
}
\description{
Density of trees per plot.
}
\details{
If every plot have same area, \code{area} is a numeric value,
otherwise \code{area} is a vector of each plot area.
}
\examples{
count <- setNames(
  c(87, 104, 83, 132, 107, 84, 110, 115, 112, 94),
  LETTERS[1:10]
)
densityTree(count, 10)
densityTree(count, area = 10, overall = FALSE)
densityTree(count, area = 10:19, overall = FALSE)

}
\seealso{
\code{\link{densityRegen}} for regeneration density.
}
