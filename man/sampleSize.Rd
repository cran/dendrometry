% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{sampleSize}
\alias{sampleSize}
\title{Sample size}
\usage{
sampleSize(
  confLev = 0.95,
  popPro = 0.5,
  errorMargin = 0.05,
  size = NULL,
  method = "",
  cv = NULL
)
}
\arguments{
\item{confLev}{numeric, the confidence level. Default is \code{0.05}.}

\item{popPro}{numeric, proportion of population which have considered factor.
Default is \code{0.5}.}

\item{errorMargin}{numeric, margin error. Default is \code{0.05}.}

\item{size}{integer, population size when it is known. If not specified,
simple random sampling will be used. Allows infinite.}

\item{method}{optional character string specifying method to use if not
simple adjusted is desired. Only "cauchran" is implemented now.}

\item{cv}{variation coefficient.}
}
\value{
The sample size.
}
\description{
Sample size
}
\note{
Population size to be considered as large or infinite heavily depends
on error margin. Lower error margin increases population size to be conidered
 as large or infinite. For errorMargin = .05, size = 152 231 and cauchran
 151 760 when confLev = .05
}
\examples{
sampleSize(confLev = .95, popPro = 0.4, errorMargin = .05)
sampleSize(confLev = .95, popPro = 0.5, errorMargin = .05, size = 150)
sampleSize(
  confLev = .95, popPro = 0.5, errorMargin = .05, size = 150,
  method = "cauchran"
)
sampleSize()

}
