% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrometry.R
\name{angle2slope}
\alias{angle2slope}
\alias{slope2angle}
\alias{principal}
\title{Angle - Slope conversion and Principal Measure determination}
\usage{
angle2slope(angle, angleUnit = c("deg", "rad"))

slope2angle(slope, angleUnit = c("deg", "rad"))

principal(angle, angleUnit = c("deg", "rad"))
}
\arguments{
\item{angle}{numeric, vector of angle to be converted to slope.}

\item{angleUnit}{character, unit of \code{angle}.\cr
For \code{slope2angle}, the desired unit for the returned angle value.\cr
For \code{principal}, both the angle input and output unit.\cr
Either \code{deg} or \code{rad}. Default is \code{deg}.}

\item{slope}{numeric, vector of slope to be converted to angle.}
}
\value{
Object of class \code{angle}.\cr
\code{angle2slope} returns vector of slope values while
\code{slope2angle} and \code{principal} return vector of angle values in unit
specified in \code{angle} argument.
}
\description{
Conversion of angle to slope values and reciprocally.\cr
\code{angle2slope} converts angle to slope values. \cr
\code{slope2angle} converts slope to angle values. \cr
\code{principal} determines the principal measure of an angle value.
Principal measure ranges from -pi to pi for radian unit while it ranges from
-180 to 180 for degree unit.
}
\note{
Use \code{principal} in position computations, not distance computations.
}
\examples{
angle2slope(10)
angle2slope(angle = 45)
angle2slope(angle = pi / 4, angleUnit = "rad")
angle2slope(1.047198, "rad")
angle2slope(seq(0.2, 1.5, .4), angleUnit = "rad") #'

slope2angle(100)
slope2angle(100, "rad")
round(pi / 4, 2)

slope2angle(17.6327)
slope2angle(angle2slope(30))

principal(303)
principal(23 * pi / 8, "rad")
principal(7 * pi / 4, angleUnit = "rad")
deg(principal(7 * pi / 4, angleUnit = "rad"))
principal(7 * 45)

}
\seealso{
\code{\link{deg}} and \code{\link{rad}}.
}
