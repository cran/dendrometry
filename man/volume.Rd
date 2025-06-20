% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{volume}
\alias{volume}
\title{Tree stem and log Volume}
\usage{
volume(height, dm, do, ds, circum, circumo, circums,
       method = "huber", successive = FALSE, log)
}
\arguments{
\item{height}{numeric, stem (whole bole) length. When \code{successive} is
"\code{TRUE}",
it stands for log length.}

\item{do, dm, ds}{numeric, respectively base, median and end diameter.}

\item{circumo, circum, circums}{numeric, respectively base, median and end
circumference.}

\item{method}{character string, the method of volume computation. Can be one
of "\code{huber}", "\code{smalian}", "\code{cone}", or "\code{newton}".
Default is "\code{huber}".}

\item{successive}{logical. If \code{TRUE}, Successive method is applied.
is applied. Default is \code{FALSE}.}

\item{log}{a vector indicating tree to which belongs each log.
Is used only when \code{successive} is "\code{TRUE}".}
}
\value{
A numeric vector of logs or trees volume.
}
\description{
Determining the volume of the log or of the tree.
}
\details{
Using \code{method = cone} refers to truncated cone method.
}
\examples{
## huber method
volume(height = 10, dm = 35)
volume(height = 10, circum = 100)

## smalian method
volume(height = 10, do = 45, ds = 15, method = "smalian")
volume(height = 10, circumo = 200, circums = 110, method = "smalian")

## cone method
volume(height = 10, do = 45, ds = 15, method = "cone")
volume(height = 10, circumo = 200, circums = 110, method = "cone")

## newton method
volume(height = 10, dm = 35, do = 45, ds = 15, method = "newton")
volume(
  height = 10, circum = 100, circumo = 200, circums = 110,
  method = "newton"
)
}
\seealso{
\code{\link{shape}}, for shape coefficient.
}
