# Data in Package ####
#' @details
#' Type \code{RShowDoc("dendrometry", package = "dendrometry")} to read a HTML
#' user guide vignette.
#'
# Type \code{RShowDoc("dendrometry_pdf", package = "dendrometry")} to read a
# PDF user guide vignette. NOT WORKING NOW!
#'
#' Type \code{demo(dendro, package = "dendrometry")} for a demo of dendrometric
#' computations. Click on \code{Index} bellow to see the index of the package.
#'
#' Type \code{demo(volume, package = "dendrometry")} for a demo of dendrometric
#'  computations. Click on \code{Index} bellow to see the index of the package.
#' @keywords internal
"_PACKAGE"


#' Dendrometric measures on tree
#' @docType data
#' @description Data frame of 10 rows and 5 columns containing tree measures.
#' @format Data frame with ten observations and five variables:
#' \describe{
#'   \item{circum}{Tree circumference in centimeter (cm).}
#'   \item{dist}{Horizontal distance between the person measuring angles and
#'   the tree (m).}
#'   \item{up}{Angle measured for the top part of the tree in degree (\\u00b0).
#'   It is used to calculate the total tree height.}
#'   \item{down}{Angle measured for the bottom part of the tree in degree (\\u00b0).}
#'   \item{fut}{Bole angle measure in degree (\\u00b0); Bole is where the first branch
#'   occurs on the trunk. It is used to calculate the merchantable tree height.}
#' }
#' @usage data(Tree)
#'
#' @examples # demo(dendro)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Tree"

#' Tree metrics for logging
#' @docType data
#' @description Data frame of 24 rows and 8 columns containing tree measures.
#' @format Data frame with twenty five observations and eight variables:
#' \describe{
#'   \item{tree}{Tree name (scientific gender).}
#'   \item{hauteur}{Stem length in meter (m).}
#'   \item{diametreMedian}{Tree median diameter in centimeter (cm).}
#'   \item{perimetreMedian}{Tree median circumference in centimeter (cm).}
#'
#'   \item{diametreSection}{Tree diameter at the end in centimeter (cm).}
#'   \item{perimetreSection}{Tree circumference at the end in centimeter (cm).}
#'
#'   \item{diametreBase}{Tree diameter at the base in centimeter (cm).}
#'   \item{perimetreBase}{Tree circumference at the base in centimeter (cm).}
#' }
#' @usage data(Logging)
#' @examples # demo(volume)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Logging"

# Methods ####

#' Print Angle
#'
#' Method to print angle and returns it invisibly.
#'
#' @param x an angle object.
#' @param ... further arguments passed to or from other methods.
#' @export
#'
print.angle <- function(x, ...) {
  unit <- switch(attr(x, "unit"),
    deg = "degrees (\\u00b0)",
    rad = "radian"
  )
  invisible(cat(unit, "angle values. \n", round(x, 2), "\n"))
}

#' Print Slope
#'
#' Method to print slope and returns it invisibly.
#'
#' @param x a slope object.
#' @param ... further arguments passed to or from other methods.
#' @export
#'
print.slope <- function(x, ...) {
  invisible(cat("slope values in percentage(%). \n", round(x, 2), "\n"))
}

# Useful functions ####


#' Skewness coefficient
#' @param x numeric vector.
#' @examples data("Logging")
#' skewness(Logging$hauteur)
#' hist(Logging$hauteur, 3)
#' @return The skewness coefficient.
#' @import stats
#' @export
skewness <- function(x) {
  if (!is.numeric(x)) {
    stop("'x' must be numeric. \n")
  }

  x <- na.omit(x)
  n <- length(x)
  n * (sum(((x - mean(x)) / sd(x))**3)) / ((n - 1) * (n - 2))
}

#' Degree and Radian
#' @description \code{deg} converts angle values from radians to degrees. \cr
#' \code{rad} converts angle values from degrees to radians.
#'
#' @aliases rad degree radian
#' @usage deg(radian)
#' @usage rad(degree)
#'
#' @param radian numeric, vector of radian values to be converted to degrees.
#' @param degree numeric, vector of degree values to be converted to radians.
#'
#' @return \code{deg} returns vector of degree values while
#' \code{rad} returns vector of radian values.
#'
#' @examples
#' deg(pi / 2)
#' rad(180)
#'
#' @seealso \code{\link{principal}}.
#'
#' @export
deg <- function(radian) {
  if (!is.numeric(radian)) {
    stop("'radian' must be numeric. \n")
  }

  angle <- radian * (180 / pi)
  attr(angle, "unit") <- "deg"

  class(angle) <- c("angle", "numeric")
  return(angle)
}

#' @export
rad <- function(degree) {
  if (!is.numeric(degree)) {
    stop("'degree' must be numeric. \n")
  }

  angle <- degree * (pi / 180)
  attr(angle, "unit") <- "rad"

  class(angle) <- c("angle", "numeric")
  return(angle)
}


#' Angle - Slope conversion and Principal Measure determination
#' @description Conversion of angle to slope values and reciprocally.\cr
#' \code{angle2slope} converts angle to slope values. \cr
#' \code{slope2angle} converts slope to angle values. \cr
#' \code{principal} determines the principal measure of an angle value.
#' Principal measure ranges from -pi to pi for radian unit while it ranges from
#' -180 to 180 for degree unit.
#'
#' @aliases slope2angle principal
#'
#' @usage angle2slope(angle, angleUnit = c("deg", "rad"))
#' @usage slope2angle(slope, angleUnit = c("deg", "rad"))
#' @usage principal(angle, angleUnit = c("deg", "rad"))
#'
#' @param angle numeric, vector of angle to be converted to slope.
#' @param slope numeric, vector of slope to be converted to angle.
#'
#' @param angleUnit character, unit of \code{angle}.\cr
#' For \code{slope2angle}, the desired unit for the returned angle value.\cr
#' For \code{principal}, both the angle input and output unit.\cr
#' Either \code{deg} or \code{rad}. Default is \code{deg}.
#'
#' @return Object of class \code{angle}.\cr
#' \code{angle2slope} returns vector of slope values while
#' \code{slope2angle} and \code{principal} return vector of angle values in unit
#' specified in \code{angle} argument.
#'
#' @examples
#' angle2slope(10)
#' angle2slope(angle = 45)
#' angle2slope(angle = pi / 4, angleUnit = "rad")
#' angle2slope(1.047198, "rad")
#' angle2slope(seq(0.2, 1.5, .4), angleUnit = "rad") #'
#'
#' slope2angle(100)
#' slope2angle(100, "rad")
#' round(pi / 4, 2)
#'
#' slope2angle(17.6327)
#' slope2angle(angle2slope(30))
#'
#' principal(303)
#' principal(23 * pi / 8, "rad")
#' principal(7 * pi / 4, angleUnit = "rad")
#' deg(principal(7 * pi / 4, angleUnit = "rad"))
#' principal(7 * 45)
#'
#' @seealso \code{\link{deg}} and \code{\link{rad}}.
#'
#' @note Use \code{principal} in position computations, not distance computations.
#' @export
angle2slope <- function(angle, angleUnit = c("deg", "rad")) {
  if (!(angleUnit[[1L]] %in% c("deg", "rad"))) {
    stop("angleUnit should be either 'deg' or 'rad'. \n")
  }

  if (missing(angleUnit) || angleUnit == "deg") {
    angle <- rad(angle)
  }

  slope <- 100 * tan(angle)
  class(slope) <- c("slope", "numeric")

  return(slope)
}

#' @export
slope2angle <- function(slope, angleUnit = c("deg", "rad")) {
  if (!(angleUnit[[1L]] %in% c("deg", "rad"))) {
    stop("angleUnit should be either 'deg' or 'rad'. \n")
  }

  angle <- atan(.01 * slope)

  if (missing(angleUnit) || angleUnit == "deg") {
    angle <- deg(angle)
    attr(angle, "unit") <- "deg"
  } else {
    attr(angle, "unit") <- "rad"
  }

  class(angle) <- c("angle", "numeric")
  return(angle)
}

#' @export
principal <- function(angle, angleUnit = c("deg", "rad")) {
  if (!(angleUnit[[1L]] %in% c("deg", "rad"))) {
    stop("angleUnit should be either 'deg' or 'rad'. \n")
  }

  if (missing(angleUnit) || angleUnit == "deg") {
    angle <- rad(angle)
    angle <- Arg(complex(real = cos(angle), imaginary = sin(angle)))
    angle <- deg(angle)
    attr(angle, "unit") <- "deg"
  } else {
    angle <- Arg(complex(real = cos(angle), imaginary = sin(angle)))
    attr(angle, "unit") <- "rad"
  }

  class(angle) <- c("angle", "numeric")
  return(angle)
}


#' Horizontal distance
#' @description Horizontal distance calculation for sloping area.
#'
#' @param distance numeric, vector of the distance measured on sloping area.
#' @param angle numeric, vector of angle or slope values.
#' @param type character, type of \code{angle} argument.
#' Either \code{"angle"} or \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit character, unit of \code{angle} measures if
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#'
#' @return A vector of horizontal distance.
#' @examples
#' distanceH(20, 30)
#' distanceH(20, angle = 30, type = "slope")
#' distanceH(20, angle = 25, type = "angle")
#' @export
#'
distanceH <- function(distance, angle, type = c("slope", "angle"),
                      angleUnit = c("deg", "rad")) {
  if (!(type[[1L]] %in% c("slope", "angle"))) {
    stop("'type' should be either 'angle' or 'slope'. \n")
  }

  if (missing(type) || type == "slope") {
    angle <- slope2angle(slope = angle, angleUnit = "rad")
  } else if (missing(angleUnit) || angleUnit == "deg") {
    angle <- rad(angle)
  }
  as.numeric(distance * cos(angle))
}

#' Relative Frequency
#' @description
#' Relative Frequency in percentage.
#' @param x numeric vector.
#' @export
rfreq <- function(x) {
  100 * x / sum(x)
}


#' Girard Form Class
#' Girard Form Class is a form quotient used to estimate taper.
#'
#' @param dbhIn numeric, diameter inside bark at the top of the first log
#' @param dbh numeric, diameter outside bark at breast height.
#' @references Strimbu, B. (2021). Dendrometry Field Manual.
#' @export
#'
girard <- function(dbh, dbhIn) {
  if (is.numeric(dbh) && is.numeric(dbhIn)) {
    return(dbhIn / dbh)
  } else {
    stop("'dbh' and 'dbhIn' should be numeric. \n")
  }
}


#' Bark factor
#' The bark factor (k) is computed for trees in order to assess the importance
#' of the valuable wood in the overall volume of a tree (Husch et al., 1982):
#'
#' @param thickness numeric, bark thickness measured on individual trees.
#' @param dbh numeric, diameter over bark of the individual trees.
#'
#' @references Husch, B., Miller, C., Beers, T., 1982. Forest mensuration.
#' Ronald Press Company, London, pp. 1 – 410.
#' @export

barkFactor <- function(dbh, thickness) {
  if (is.numeric(dbh) && is.numeric(thickness)) {
    dbhIn <- dbh - 2 * thickness
    res <- sum(dbhIn) / sum(dbh)
  } else {
    stop("'dbh' and 'thickness' should be numeric. \n")
  }

  return(res)
}

# END ####




#' Make stand data
#' @description Make data of stands according to defined
#' \code{factor1,factor2,factor3}.
#' @param data data frame containing optional factors
#' \code{factor1,factor2,factor3}.
#' @param factor1,factor2,factor3 optional variables of the data frame that
#' define subsets to consider.
#' @return A list of data.
#' @examples
#' # require(BiodiversityR)
#' # data(ifri, package = "BiodiversityR")
#' # a1=makedata(ifri, factor1 = "forest", factor2 = "plotID", factor3 = "species")
#' # a2=makedata(ifri, factor1 = "species")
#' # makedata(ifri, factor2 = "")
#' # identical(makedata(ifri), ifri)
#' @export
makedata <- function(data, factor1 = "", factor2 = "", factor3 = "") {
  if (!is.character(c(factor1, factor2, factor3))) {
    stop("When difined, 'factor1', 'factor2' and/or 'factor3' should be character.
         Please, quote them when using.")
  }
  if (factor1 != "") {
    pos1 <- match(factor1, names(data))
  }
  if (factor2 != "") {
    pos2 <- match(factor2, names(data))
  }
  if (factor3 != "") {
    pos3 <- match(factor3, names(data))
  }

  nm <- c(factor1, factor2, factor3)
  mis <- !(nm %in% c(names(data), ""))
  if (sum(mis) != 0) {
    miss <- nm[mis]
    warning(
      "The following are not considered because they are not variable names in data: ",
      paste(miss, collapse = ", "), ".\n"
    )

    for (nmi in c("factor1", "factor2", "factor3")[mis]) {
      assign(nmi, "")
    }
  }

  if ((factor1 != "")) {
    dataf <- lapply(unique(data[[pos1]]), function(i) {
      dataf <- subset(data, data[[pos1]] == i)

      if (factor2 != "") {
        datafp <- lapply(unique(dataf[[pos2]]), function(j) {
          datafp <- subset(dataf, dataf[[pos2]] == j)

          if (factor3 != "") {
            datafps <- lapply(unique(datafp[[pos3]]), function(k) {
              subset(datafp, datafp[[pos3]] == k)
            })
            names(datafps) <- unique(datafp[[pos3]])
            datafps
          } else {
            datafp
          }
        })
        names(datafp) <- unique(dataf[[pos2]])
        datafp
      } else if (factor3 != "") {
        datafs <- lapply(unique(dataf[[pos3]]), function(k) {
          subset(dataf, dataf[[pos3]] == k)
        })
        names(datafs) <- unique(dataf[[pos3]])
        datafs
      } else {
        dataf
      }
    })
    names(dataf) <- unique(data[[pos1]])
    return(dataf)
  } else if (factor2 != "") {
    datap <- lapply(unique(data[[pos2]]), function(j) {
      datap <- subset(data, data[[pos2]] == j)

      if (factor3 != "") {
        dataps <- lapply(unique(datap[[pos3]]), function(k) {
          subset(datap, datap[[pos3]] == k)
        })
        names(dataps) <- unique(datap[[pos3]])
        dataps
      } else {
        datap
      }
    })
    names(datap) <- unique(data[[pos2]])
    return(datap)
  } else if (factor3 != "") {
    datas <- lapply(unique(data[[pos3]]), function(k) {
      subset(data, data[[pos3]] == k)
    })
    names(datas) <- unique(data[[pos3]])
    return(datas)
  } else {
    warning("Any of 'factor1', 'factor2' and 'factor3' are not defined. The same
            data are returned.")
  }
  # stop("At least one of 'factor1', 'factor2' and 'factor3' should be defined.")
  return(data)
}

#' Making factor vectors
#' @description Changes character vectors of a data set to factor vectors.
#' @aliases factorise
#' @param data data frame or tibble data set.
#' @param binary logical indicating if binary numeric data should be considered
#' as factor.
#' Default is \code{FALSE}.
#' @return Data frame with all character vectors changed to factor vectors.
#' @details When \code{binary = TRUE}, variables stored as numeric and which have
#' exactly two levels are changed to factor.
#' @export
factorize <- function(data, binary = FALSE) {
  # if(!"data.frame" %in% class(data)) warning("data is not a data frame.\n")

  for (i in 1:length(data)) {
    if (is.character(data[[i]]) ||
      (binary && is.numeric(data[[i]]) &&
        length(levels(as.factor(data[[i]]))) == 2)) {
      data[[i]] <- as.factor(data[[i]])
    }

    # else data[[i]] <- data[[i]]
  }
  return(data)
}

#' @export
factorise <- factorize

#' Sample size
#'
#' @param confLev numeric, the confidence level. Default is \code{0.05}.
#' @param popPro numeric, proportion of population which have considered factor.
#' Default is \code{0.5}.
#'
#' @param errorMargin numeric, margin error. Default is \code{0.05}.
#' @param size integer, population size when it is known. If not specified,
#' simple random sampling will be used. Allows infinite.
#'
#' @param method optional character string specifying method to use if not
#' simple adjusted is desired. Only "cauchran" is implemented now.
#'
#' @param cv variation coefficient.
#' @note Population size to be considered as large or infinite heavily depends
#' on error margin. Lower error margin increases population size to be conidered
#'  as large or infinite. For errorMargin = .05, size = 152 231 and cauchran
#'  151 760 when confLev = .05
#' @return The sample size.

#' @examples sampleSize(confLev = .95, popPro = 0.4, errorMargin = .05)
#' sampleSize(confLev = .95, popPro = 0.5, errorMargin = .05, size = 150)
#' sampleSize(
#'   confLev = .95, popPro = 0.5, errorMargin = .05, size = 150,
#'   method = "cauchran"
#' )
#' sampleSize()
#'
#' @export
sampleSize <- function(confLev = .95, popPro = .5, errorMargin = .05,
                       size = NULL, method = "", cv = NULL) {
  alpha <- 1 - confLev
  if (!missing(cv)) {
    n <- qt(p = 1 - (alpha / 2), df = size - 1)**2 * cv**2 / errorMargin**2
    return(n)
  }
  if (is.null(size) || is.infinite(size)) {
    n <- qnorm(p = 1 - (alpha / 2))**2 * popPro * (1 - popPro) / errorMargin**2
    return(n)
  } else if (method == "cauchran") {
    if (size >= 30) {
      n <- qnorm(p = 1 - (alpha / 2))**2 * popPro * (1 - popPro) / errorMargin**2
    } else {
      n <- qt(p = 1 - (alpha / 2), df = size - 1)**2 * popPro * (1 - popPro) / errorMargin**2
    }
    nc <- n / ((n - 1) / size + 1)
    return(nc)
  } else {
    if (size >= 30) {
      n <- qnorm(p = 1 - (alpha / 2))**2 * popPro * (1 - popPro) / errorMargin**2
    } else {
      n <- qt(p = 1 - (alpha / 2), df = size - 1)**2 * popPro * (1 - popPro) / errorMargin**2
    }
    np <- size * n / (size + n)
    return(np)
  }
}


#' Stack all vectors of a data frame or list
#' @description Stacking all columns of a data frame or vectors of a list into
#' a single vector.
#' @param data data frame, tibble or list.
#' @return A vector of all element of the argument \code{data}.
#' @export
stacking <- function(data) {
  if (inherits(data, what = "list")) {
    data <- list2DF(data)
  }
  as.vector(as.matrix(data))
}


#' Fibonacci series
#' @description Generates numbers from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first two numbers of the series.
#' @param PrintFib logical, indicating if the series should be printed.
#' @return Either an integer, result of the function or a vector of \code{n}
#' first numbers of the series.
#' @examples fibonacci(n = 10, PrintFib = TRUE)
#' fibonacci(n = 10, Uo = 1, U1 = 3, PrintFib = FALSE)
#' @details The series equation is Un = U_(n-2) /U_(n-1).
#' @seealso \code{\link{fiboRate}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export
fibonacci <- function(n, PrintFib = FALSE, Uo = 0, U1 = 1) {
  Un <- numeric(length = n)
  Un[1:2] <- c(Uo, U1)

  if (n < 2) {
    return(Uo)
  } else if (n > 2) {
    for (i in 3:n) {
      Un[i] <- Un[i - 1] + Un[i - 2]
    }
    Fib <- Un
  } else {
    Fib <- Un
  }

  if (!PrintFib) {
    Fib <- Fib[n]
  }

  return(Fib)
}

#' Fibonacci series ratio
#' @description Computes rates from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first number of the series.
#' @param PrintSer logical, indicating if the series should be printed.
#' @return Either a numeric, result of the rate of \code{nth} and \code{(n-1)th}
#'  numbers
#' in Fibonacci series or all \code{(n-1)th} those rates.
#' @examples ## Golden number (Le Nombre d'Or)
#' fiboRate(n = 18, PrintSer = FALSE, Uo = 0, U1 = 1)
#' ## (1+sqrt(5))/2
#' fiboRate(n = 10, PrintSer = TRUE, Uo = 0, U1 = 1)
#' @details The series equation is Un = U_(n-2) /U_(n-1).
#' The function returns golden number when Uo = 0, and U1 = 1. Larger n is, more
#'  precise the number (result) is.
#' @seealso \code{\link{fibonacci}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export

fiboRate <- function(n, PrintSer = FALSE, Uo = 0, U1 = 1) {
  a <- fibonacci(n = n, Uo = Uo, U1 = U1, PrintFib = TRUE)
  # $$U_n = \frac{U_{n-2}}{U_{n-1}}$$
  serie <- a[2:n] / a[1:n - 1]
  if (PrintSer) {
    return(serie)
  } else {
    return(serie[n - 1])
  }
}

# Tree parameter ####

#' Diameter (DBH) and Circumference
#' @description \code{DBH} computes diameter (at breast height) based on
#' circumference (at breast height). \cr
#' \code{circum} computes circumference (at breast height) based on diameter
#' (at breast height). \cr
#' They are based on circle diameter and perimeter formulas.
#'
#' @aliases circum
#' @usage dbh(circum)
#' @usage circum(dbh)
#'
#' @param circum numeric, vector of circumference.
#' @param dbh numeric, vector of diameter.
#'
#' @return \code{dbh}, returns diameter and \code{circum}, returns circumference.
#' @examples
#' perimeter <- seq(30, 60, 1.4)
#' diameter <- dbh(perimeter)
#' circum(diameter)
#'
#' @seealso See also \code{\link{height}} for tree  height.
#' @export
dbh <- function(circum) {
  if (is.numeric(circum)) {
    return(circum / pi)
  } else {
    stop("'circum' must be numeric. \n")
  }
}

#' @export
circum <- function(dbh) {
  if (is.numeric(dbh)) {
    return(dbh * pi)
  } else {
    stop("'dbh' must be numeric. \n")
  }
}


#' Individual Basal Area and DBH (diameter)
#' @description \code{basal_i} computes the basal area of a tree stem
#' (individual), the area of a circle of diameter \code{dbh}. \cr
#' \code{basal2dbh} computes the dbh (diameter) based on the basal area.
#'
#' @aliases basal2dbh
#'
#' @usage basal_i(dbh, circum = NULL)
#' @usage basal2dbh(basal)
#'
#' @param dbh numeric, vector of diameter.
#' @param circum numeric, vector of circumference. Is used only when \code{dbh}
#' is not given.
#' @param basal numeric, individual basal area.
#'
#' @return \code{basal_i} returns individual basal area while \code{basal2dbh}
#' returns DBH.
#'
#' @examples
#' basal_i(dbh = 10)
#' basal_i(circum = 31.41)
#' basal2dbh(78.53982)
#'
#' @details If \code{circum} is given, \code{dbh} is not used.
#' @export
basal_i <- function(dbh = NULL, circum = NULL) {
  if (is.numeric(circum)) {
    dbh <- dbh(circum = circum)
  } else if (!is.numeric(dbh)) {
    stop("'dbh' or 'circum' should be given and numeric. \n")
  }

  return(pi * .25 * dbh**2)
}

#' @export
basal2dbh <- function(basal) {
  2 * sqrt(basal / pi)
}



#' Height of Tree or any vertical Object
#' @description Computes the height of tree, pillar, girder, mast or any
#' vertical object. It allows either slope (in percent) or angle (in degrees or
#' radians). No matter the relative position of the persons who measures the
#' angle or the slope.
#'
#' @param distance numeric, vector of the horizontal distance between object
#' and the person who measures angle.
#' @param top,bottom numeric vector of top angle and bottom angle respectively
#' (readings from a clinometer).
#' @param type the type of \code{top} and \code{bottom} measures. Either
#' \code{"angle"} or \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit the unit of \code{top} and \code{bottom} measures when
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#'
#' @return A vector of heights.
#' @examples
#' height(10, 80, 17)
#' height(17, top = -18, bottom = -113)
#' height(distance = 18, top = 42, bottom = -12, type = "angle", angleUnit = "deg")
#' height(
#'   distance = 18:21, top = 42:45, bottom = -12:-15, type = "angle",
#'   angleUnit = "deg"
#' )
#' ## Below shows warning messages
#' height(
#'   distance = 18:21, top = -42:-45, bottom = -12:-15, type = "angle",
#'   angleUnit = "deg"
#' )
#' @export
height <- function(distance, top, bottom, type = c("angle", "slope"),
                   angleUnit = c("deg", "rad")) {
  if (!is.numeric(distance)) {
    stop("'distance' must be numeric. \n")
  }

  if (!is.numeric(top)) {
    stop("'top' must be numeric. \n")
  }

  if (!is.numeric(bottom)) {
    stop("'bottom' must be numeric. \n")
  }

  if (!(type[[1L]] %in% c("slope", "angle"))) {
    stop("'type' should be either 'angle' or 'slope'. \n")
  }

  if (!(angleUnit[[1L]] %in% c("deg", "rad"))) {
    stop("'angleUnit' should be either 'deg' or 'rad'. \n")
  }

  if (sum(top <= bottom)) {
    warning("One or more top angles are less than or equal to their bottom angles. \n")
  }

  if (missing(type) || type == "slope") {
    res <- as.numeric(0.01 * distance * (top - bottom))
  } else {
    if (missing(angleUnit) || angleUnit == "deg") {
      top <- rad(top)
      bottom <- rad(bottom)
    }

    res <- as.numeric(distance * (tan(top) - tan(bottom)))
  }
  return(res)
}

# END ####

#' The decrease coefficient
#' @description This coefficient expresses the ratio between the diameter
#' (or circumference) at mid-height of the bole and the diameter
#' (or circumference) measured at breast height.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Not mixture.
#' @return A vector of decrease coefficients.
#' @examples decrease(30, 120)
#' decrease(middle = 40, breast = 90)
#' @export
decrease <- function(middle, breast) {
  if (!is.numeric(middle)) {
    stop("'middle' should be numeric")
  }

  if (!is.numeric(breast)) {
    stop("'breast' should be numeric")
  }

  if (any(middle > breast)) {
    warning("One or more breast value are less than their middle values.
          Please check your data")
  }
  return(middle / breast)
}


#' The reduction coefficient
#' @description The reduction coefficient is the ratio between the difference
#' in size at breast height and mid-height on the one hand, and the size at
#' breast height on the other. It is thus the complement to 1 of the
#' coefficient of decrease.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Not mixture.
#' @return The reduction coefficient.
#' @examples reducecoef(30, 120)
#' reducecoef(middle = 40, breast = 90)
#' @seealso \code{decrease}
#' @export
reducecoef <- function(middle, breast) {
  if (!is.numeric(middle)) {
    stop("'middle' should be numeric")
  }

  if (!is.numeric(breast)) {
    stop("'breast' should be numeric")
  }

  if (any(middle > breast)) {
    warning("One or more breast value are less than their middle values.
          Please check your data")
  }
  r <- (breast - middle) / breast
  return(r)
}


#' Metric scrolling or decay
#' @description The average metric decay expresses the difference, in
#' centimeters per meter, between the diameter (or circumference) at breast
#' height and its diameter at mid-height of a stem related to the difference
#' between the height at mid-height and that at breast height.
#' @param dmh numeric, the diameter at middle height in centimeter (cm).
#' @param dbh numeric, the diameter at breast height in centimeter (cm).
#' @param mh numeric, the middle (or cut) height in meter (m).
#' @param bh Either a numeric value standing for the breast height in meter (m)
#' of all trees or a numeric vector standing for the breast height of each tree.
#' Default is \code{1.3}.
#' @return Metric decay
#' @examples decreaseMetric(dmh = 40, dbh = 90, mh = 7)
#' decreaseMetric(45, 85, 9)
#' @seealso \code{reducecoef}
#' @export
decreaseMetric <- function(dmh, dbh, mh, bh = 1.3) {
  if (!is.numeric(dbh)) {
    stop("'dbh' should be numeric")
  }
  if (!is.numeric(dmh)) {
    stop("'dmh' should be numeric")
  }
  if (!is.numeric(mh)) {
    stop("'mh' should be numeric")
  }

  if (any(dmh > dbh)) {
    warning("One or more middle height diameter are greater than their DBH.
            Please check your data")
  }
  if (any(mh <= bh)) {
    warning("One or more middle height are greater or equal to their breast
    height. Please check your data")
  }
  d <- (dbh - dmh) / (mh - bh)
  return(d)
}


.huberMethod <- function(height, dm, circum, successive, log) {
  if (is.null(dm) && is.null(circum)) {
    stop("Specify either 'dm' or 'circum'")
  } else if (!is.null(dm) && !is.null(circum)) {
    warning("Don't specify both 'dm' (diameter) and 'circum' (circumference).
            Only 'dm' is considered.")
  }

  if (is.null(dm)) {
    dm <- dbh(circum)
  }
  v <- .25 * pi * dm**2 * height

  if (successive) {
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  }

  return(v)
}
.smalianMethod <- function(height, do, ds, circumo, circums, successive,
                           log) {
  # Don't specify mixture of args
  if (all(
    any(is.null(circumo), is.null(circums)),
    any(is.null(do), is.null(ds))
  )) {
    stop("Specify either both 'circumo' and 'circums' or both 'do' and 'ds'
           when using 'smalian' method.")
  } else if (sum(
    !is.null(do), !is.null(ds), !is.null(circumo),
    !is.null(circums)
  ) > 2) {
    warning("Don't specify both diameters and circumferences.")
  }
  # le && est inutile
  if (!is.null(do) && !is.null(ds)) {
    v <- .125 * pi * (do**2 + ds**2) * height
  } else if (!is.null(circumo) && !is.null(circums)) {
    v <- .125 * pi * (dbh(circumo)**2 + dbh(circums)**2) * height
  }

  if (successive) {
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  }
  return(v)
}
.coneMethod <- function(height, do, ds, circumo, circums, successive, log) {
  # Don't specify mixture of args
  if (all(
    any(is.null(circumo), is.null(circums)),
    any(is.null(do), is.null(ds))
  )) {
    stop("Specify either 'circumo' and 'circums' or 'do' and 'ds'
           when using 'cone' method.")
  } else if (sum(
    !is.null(do), !is.null(ds), !is.null(circumo),
    !is.null(circums)
  ) > 2) {
    warning("Don't specify both diameters and circumferences.")
  }
  # le && est inutile
  if (!is.null(do) && !is.null(ds)) {
    v <- pi * (do**2 + do * ds + ds**2) * height / 12
  } else if (!is.null(circumo) && !is.null(circums)) {
    v <- pi * (dbh(circumo)**2 + dbh(circumo) * dbh(circums) + dbh(circums)**2) *
      height / 12
  }
  if (successive) {
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  }
  return(v)
}
.newtonMethod <- function(height, do, dm, ds, circumo, circum, circums,
                          successive, log) {
  if (all(
    any(is.null(circum), is.null(circumo), is.null(circums)),
    any(is.null(dm), is.null(do), is.null(ds))
  )) {
    stop("Specify either 'circum', 'circumo' and 'circums' or 'dm', 'do' and 'ds'
           when using 'newton' method.")
  } # "Specify either only diameters or only circumferences."
  else if (sum(
    is.null(dm), !is.null(do), !is.null(ds), is.null(circum),
    !is.null(circumo), !is.null(circums)
  ) > 3) {
    warning("Don't specify both diameters and circumferences.")
  }

  if (!is.null(dm) && !is.null(do) && !is.null(ds)) {
    v <- pi * (do**2 + 4 * dm**2 + ds**2) * height / 24
  } else if (!is.null(circum) && !is.null(circumo) && !is.null(circums)) {
    v <- pi * (dbh(circumo)**2 + 4 * dbh(circum)**2 + dbh(circums)**2) *
      height / 24
  }

  if (successive) {
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  }
  return(v)
}


#' Tree stem and log Volume
#' @description Determining the volume of the log or of the tree.
#' @usage volume(height, dm, do, ds, circum, circumo, circums,
#'        method = "huber", successive = FALSE, log)
#' @param height numeric, stem (whole bole) length. When \code{successive} is
#' "\code{TRUE}",
#' it stands for log length.
#' @param do,dm,ds numeric, respectively base, median and end diameter.
#' @param circumo,circum,circums numeric, respectively base, median and end
#' circumference.
#' @param method character string, the method of volume computation. Can be one
#' of "\code{huber}", "\code{smalian}", "\code{cone}", or "\code{newton}".
#' Default is "\code{huber}".
#' @param successive logical. If \code{TRUE}, Successive method is applied.
#' is applied. Default is \code{FALSE}.
#' @param log a vector indicating tree to which belongs each log.
#' Is used only when \code{successive} is "\code{TRUE}".
#' @examples ## huber method
#' volume(height = 10, dm = 35)
#' volume(height = 10, circum = 100)
#'
#' ## smalian method
#' volume(height = 10, do = 45, ds = 15, method = "smalian")
#' volume(height = 10, circumo = 200, circums = 110, method = "smalian")
#'
#' ## cone method
#' volume(height = 10, do = 45, ds = 15, method = "cone")
#' volume(height = 10, circumo = 200, circums = 110, method = "cone")
#'
#' ## newton method
#' volume(height = 10, dm = 35, do = 45, ds = 15, method = "newton")
#' volume(
#'   height = 10, circum = 100, circumo = 200, circums = 110,
#'   method = "newton"
#' )
#' @return A numeric vector of logs or trees volume.
#' @details Using \code{method = cone} refers to truncated cone method.
#' @seealso \code{\link{shape}}, for shape coefficient.
#' @export
volume <- function(height, dm = NULL, do = NULL, ds = NULL, circum = NULL,
                   circumo = NULL, circums = NULL, method = "huber",
                   successive = FALSE, log = NULL) {
  if (!(method %in% c("huber", "smalian", "cone", "newton"))) {
    stop("'method' should be one of 'huber', 'smalian', 'cone', or 'newton'.")
  }

  if (all(!successive, !is.null(log))) {
    warning("Don't specify 'log' when 'successive' is not TRUE")
  } # unused arg ...


  if (method == "huber") {
    return(.huberMethod(
      height = height, dm = dm,
      circum = circum, log = log,
      successive = successive
    ))
  } else if (method == "smalian") {
    return(.smalianMethod(
      height = height, do = do,
      circumo = circumo,
      ds = ds,
      circums = circums,
      log = log,
      successive = successive
    ))
  } else if (method == "cone") {
    return(.coneMethod(
      height = height, do = do,
      ds = ds, circumo = circumo,
      circums = circums,
      successive = successive,
      log = log
    ))
  } else if (method == "newton") {
    return(.newtonMethod(
      height = height, do = do,
      dm = dm, ds = ds,
      circumo = circumo,
      circum = circum,
      circums = circums,
      successive = successive,
      log = log
    ))
  }
}


#' The shape coefficient
#' @description The shape coefficient of the tree is the ratio of the actual
#' volume of the tree to the volume of a cylinder having as base the surface of
#' the section at 1.3 m (or a given breast height) and as length, the height
#' (at bole level) of the tree.
#' @usage shape(volume, height, dbh, basal = NULL)
#' @param volume numeric, tree real volume.
#' @param height numeric, tree height.
#' @param dbh numeric, diameter at breast height (DBH).
#' @param basal numeric, basal area. Is used when \code{dbh} is not specified.
#' @examples shape(volume = 10000, 11, dbh = 40)
#' shape(volume = 10000, 11, 40)
#' shape(volume = 10000, 11, basal = 2256.637)
#' ## Bellow gives warning
#' shape(volume = 10000, height = 11, dbh = 40, basal = 2256.637)
#' @return The shape coefficient.
#' @seealso \code{\link{volume}}, for tree real volume.
#' @export
shape <- function(volume, height, dbh = NULL, basal = NULL) {
  if (all(is.null(dbh), is.null(basal))) {
    stop("Specify either 'dbh' or 'basal'")
  } else if ((!any(is.null(dbh), is.null(basal)))) {
    warning("Both of 'dbh' and 'basal' are specified. Only 'dbh' is considered.")
  }

  if (!is.null(dbh)) {
    f <- volume / (basal_i(dbh = dbh) * height)
  } else {
    f <- volume / (basal * height)
  }
  return(f)
}

# Stand parameter ####

#' Density of regeneration
#' @description
#' Computes the density per plot of tree regeneration based on counts in subplots.
#'
#' @param data an optional data frame, list, tibble or object coercible by
#' \code{\link{as.data.frame}} to a data frame containing the variables whose
#' names are given in \code{count} and \code{plot}.
#'
#' @param plot an optional character, name of the variable containing the plot
#' identities. If \code{data} is missing, a vector providing the plot identities.
#'
#' @param count character, name of the variable containing the counts: number
#' of stems (individuals). If \code{data} is missing, a numeric vector
#' providing the the counts: number of stems (individuals).
#'
#' @param nbSubPlot numeric, number of subplots per plot.
#' @param area numeric, area of each subplot.
#'
#' @export
densityRegen <- function(data = NULL, plot = NULL, count, nbSubPlot, area) {
  if (!is.numeric(area)) {
    stop("'area' must be numeric. \n")
  }

  if (!is.numeric(nbSubPlot)) {
    stop("'nbSubPlot' must be numeric. \n")
  }

  if (missing(data)) {
    if (!is.numeric(count)) {
      stop("'count' must be numeric vector if 'data' is not provided. \n")
    }

    if (missing(plot)) {
      return(sum(count) / (nbSubPlot * area))
    }

    if (length(count) != length(plot)) {
      stop("'count' and 'plot' must be vectors of same length. \n")
    }

    data <- data.frame(count = count, plot = plot)
    plot <- "plot"
    count <- "count"
  } else {
    data <- droplevels(as.data.frame(data))

    if (!is.character(count)) {
      stop("'count' must be character if 'data' is provided. \n")
    }

    if (missing(plot)) {
      return(sum(data[[count]]) / (nbSubPlot * area))
    }

    if (!is.character(plot)) {
      stop("'plot' must be character if 'data' is provided. \n")
    }
  }
  d <- makedata(data, factor1 = plot)

  sapply(X = d, FUN = function(plotData) {
    sum(plotData[[count]]) / (nbSubPlot * area)
  })
}


#' Tree density
#' @description Density of trees per plot.
#'
#' @param number numeric, vector of tree count in each plot.
#' @param area numeric, area of a plot.
#' @param overall logical, if \code{TRUE}, an overall mean density is computed,
#' otherwise density is computed for each plot. Default is \code{TRUE}.
#'
#' @return Vector of density.
#'
#' @details If every plot have same area, \code{area} is a numeric value,
#' otherwise \code{area} is a vector of each plot area.
#'
#' @examples
#' count <- setNames(
#'   c(87, 104, 83, 132, 107, 84, 110, 115, 112, 94),
#'   LETTERS[1:10]
#' )
#' densityTree(count, 10)
#' densityTree(count, area = 10, overall = FALSE)
#' densityTree(count, area = 10:19, overall = FALSE)
#'
#' @seealso \code{\link{densityRegen}} for regeneration density.
#' @export
#'
densityTree <- function(number, area, overall = TRUE) {
  if (!is.numeric(area)) {
    stop("'area' must be numeric. \n")
  }

  if (is.numeric(number)) {
    if (overall) {
      res <- mean(number / area)
    } else {
      res <- number / area
    }
  } else {
    stop("'number' must be numeric. \n")
  }

  return(res)
}


#' Mean diameter
#' @description Mean diameter of a forestry stand.
#'
#' @param dbh numeric, vector of diameter.
#'
#' @return Mean diameter.
#'
#' @seealso \code{\link{dbh}}, \code{\link{basal_i}}
#'
#' @examples
#' set.seed(1)
#' diameter <- rnorm(10, 100, 20)
#' diameterMean(dbh = diameter)
#' @export
diameterMean <- function(dbh) {
  if (is.numeric(dbh)) {
    return(sqrt(mean(dbh**2)))
  } else {
    stop("'dbh' must be numeric. \n")
  }
}


#' The basal area of stands
#' @description The basal area is the cross sectional area of the bole or stem
#' of a tree at breast height.
#'
#' @param dbh numeric, vector of diameter.
#' @param area numeric, area of a plot (see \code{details} for unit).
#' @param k numeric, used to convert diameter unit. Default is \code{100}
#' (see \code{details})
#'
#' @return A vector of basal area of stands.
#'
#' @details If \code{area} is expressed in ha and \code{dbh} expressed in cm,
#' the basal area unit is cm\\u00b2/ha when \code{k = 1}.
#' In order to convert centimeter (cm) to meter (m) for \code{dbh}, set
#' \code{k = 100}. Because 1m = 100 cm. Then, basal area unit will be
#' \code{m\\u00b2/ha}.
#'
#' If \code{dbh} is in meter (m), and \code{area} in in hectare (ha), setting
#' \code{k = 1} returns basal area in m\\u00b2/ha.
#'
#' If \code{dbh} is in feet, and \code{area} in acre, setting \code{k = 1}
#'  returns basal area in ft\\u00b2/ac.
#'
#' If \code{dbh} is in inch, and \code{area} in acre, setting
#' \code{k = 12} returns basal area in feet\\u00b2/acres (ft\\u00b2/ac).
#' @export
basal <- function(dbh, area, k = 100) {
  sum(basal_i(dbh = dbh)) / (area * k**2)
}


#' Basal area contribution
#' @description The basal area contribution (in per cent) is defined as the part
#' of a given species trees in the overall basal area of all trees in an area.
#'
#' @param basal numeric, basal area per species.
#'
#' @export
basalContribution <- function(basal) {
  rfreq(x = basal)
}



#' Lorey's mean height
#' @description The average height of the trees in a plot, weighted by their
#' basal area.
#'
#' @param basal numeric, vector of trees' individual basal area.
#' @param height numeric, vector of trees' individual height.
#'
#' @return Average Lorey height of a stand.
#'
#' @examples
#' set.seed(1)
#' donnee <- data.frame(
#'   hauteur = rnorm(10, 12, 3),
#'   area = basal_i(rnorm(10, 100, 20))
#' )
#' loreyHeight(basal = donnee$area, height = donnee$hauteur)
#'
#' @seealso \code{\link{height}}, \code{\link{basal_i}}
#' @export
loreyHeight <- function(basal, height) {
  sum(basal * height) / sum(basal)
}

#' Index of Blackman
#' @param density numeric, vector of the density.
#' @return Index of Blackman.
#' @export
#'
blackman <- function(density) {
  if (!is.numeric(density)) {
    stop("'density' must be numeric. \n")
  } else {
    var(density) / mean(density)
  }
}


#' Index of Green
#' @param density numeric, vector of the density.
#' @return Index of Green.
#' @export
#'
green <- function(density) {
  if (!is.numeric(density)) {
    stop("'density' must be numeric. \n")
  } else {
    (blackman(density) - 1) / (length(density) - 1)
  }
}


#' Structural parameters for stands
#' @description \code{param} computes structural parameters per stands specified in factor arguments.  \cr
#' \code{param_i} computes structural parameters for a stand.

#' @usage param_i(data, plot = "", DBH = "", height = "", crown = "", area = NULL,
#' k = 100, kCrown = 1)
#'
#' param(data, plot = "", DBH = "", height = "", crown = "", area = NULL,
#' k = 100, kCrown = 1, factor1 = "", factor2 = "", factor3 = "")
#'
#' @aliases param
#'
#' @param data a data frame, list, tibble or object coercible by
#' \code{\link{as.data.frame}} to a data frame containing the variables whose
#' names would be given in \code{DBH}, \code{height} and \code{crown}.
#'
#' @param plot,DBH,height,crown optional characters, names of the variables of
#' \code{data} containing respectively the plot's identification or code,
#' the diameter at breast height, the tree total height, and the crown diameter
#' of each individual tree.
#'
#' @param area numeric, area of a plot (see \code{\link{basal}} section
#' \code{details} for unit).
#'
#' @param k,kCrown numeric, used to convert diameter and crown diameter units
#' respectively. Default are \code{k = 100} and \code{kCrown = 1}.
#' (see \code{\link{basal}})
#'
#' @param factor1,factor2,factor3 character, optional variables of the data frame
#' that define subsets to consider.
#'
#' @returns A vector, matrix or list of matrices containing of structural
#' parameters.
#' @details
#' Blackman and Green indices are returned if combinations of specified
#' \code{factor1..3}
#' contain more than one plot. Otherwise, the right are returned as attributes.
#'
# Additional details on outputs according to inputs...
#'
#' @examples
#' param_i(
#'   data = Logging, plot = "tree", DBH = "diametreMedian",
#'   height = "hauteur", crown = "perimetreBase", area = 0.03, kCrown = 100
#' )
#' @export
#'
param_i <- function(data, plot = "", DBH = "", height = "", crown = "",
                    area = NULL, k = 100, kCrown = 1) {
  data <- droplevels(as.data.frame(data))
  nm <- c(plot, DBH, height, crown)
  mis <- !(nm %in% c(names(data), ""))
  if (sum(mis) != 0) {
    miss <- nm[mis]
    warning(
      "The following are not considered because they are not variable names in data: ",
      paste(miss, collapse = ", "), ".\n"
    )

    for (nmi in c("plot", "DBH", "height", "crown")[mis]) {
      assign(nmi, "")
    }
  }

  nbPlot <- ifelse(plot == "", 1, sum(!duplicated(data[[plot]])))
  # browser()
  if (DBH != "") {
    MeanDBH <- diameterMean(dbh = data[[DBH]])

    if (!is.null(area)) {
      Basal <- basal(dbh = data[[DBH]], area = area * nbPlot, k = k)
    } else {
      Basal <- NULL
    }
  } else {
    Basal <- MeanDBH <- NULL
  }

  if (crown != "") {
    MeanCrown <- diameterMean(dbh = data[[crown]])
    if (!is.null(area)) {
      BasalCrown <- basal(
        dbh = data[[crown]], area = area * nbPlot,
        k = kCrown
      )
    } else {
      BasalCrown <- NULL
    }
  } else {
    BasalCrown <- MeanCrown <- NULL
  }

  if (height != "") {
    Height <- mean(data[[height]])

    if (DBH != "") {
      LoreyHeight <- loreyHeight(
        basal = basal_i(dbh = data[[DBH]]),
        height = data[[height]]
      )
    } else {
      LoreyHeight <- NULL
    }
  } else {
    Height <- LoreyHeight <- NULL
  }


  if (!is.null(area)) {
    if (nbPlot > 1) {
      den <- densityTree(
        number = table(data[[plot]]), area = area,
        overall = FALSE
      )

      Density <- mean(den)
      Bla <- blackman(den)
      Gre <- green(den)
    } else {
      Bla <- Gre <- NULL
      Density <- densityTree(number = nrow(data), area = area * nbPlot)
    }
  } else {
    Density <- Bla <- Gre <- NULL
  }

  res <- c(
    MeanDBH = MeanDBH, Basal = Basal, MeanCrown = MeanCrown,
    BasalCrown = BasalCrown, Height = Height, LoreyHeight = LoreyHeight,
    Density = Density, Blackman = Bla, Green = Gre, nbPlot = nbPlot
  )
  return(res)
}


#' @export
param <- function(data, plot = "", DBH = "", height = "", crown = "",
                  area = NULL, k = 100, kCrown = 1,
                  factor1 = "", factor2 = "", factor3 = "") {
  lev <- sum(factor1 != "", factor2 != "", factor3 != "")

  if (lev == 0) {
    res <- param_i(data,
      plot = plot, area = area, DBH = DBH, height = height,
      crown = crown, k = k, kCrown = kCrown
    )
  } else {
    data <- makedata(
      data = data, factor1 = factor1, factor2 = factor2,
      factor3 = factor3
    )

    # Function to compute param_i
    parami_func <- function(dat) {
      param_i(
        data = dat, plot = plot, area = area, DBH = DBH, height = height,
        crown = crown, k = k, kCrown = kCrown
      )
    }

    if (lev == 1) {
      res <- sapply(X = data, FUN = parami_func)
      attr(res, "Blackman") <- blackman(res["Density", ])
      attr(res, "Green") <- green(res["Density", ])
    } else if (lev == 2) {
      res <- lapply(X = data, function(d1) {
        res0 <- sapply(X = d1, FUN = parami_func)

        attr(res0, "Blackman") <- blackman(res0["Density", ])
        attr(res0, "Green") <- green(res0["Density", ])
        res0
      })
    } else { # (lev == 3)
      res <- lapply(X = data, function(d2) {
        lapply(X = d2, function(d1) {
          res0 <- sapply(X = d1, FUN = parami_func)

          attr(res0, "Blackman") <- blackman(res0["Density", ])
          attr(res0, "Green") <- green(res0["Density", ])
          res0
        })
      })
    }
  }
  return(res)
}


#' Adjust (fit) three-parameter Weibull distribution
#'
#' @usage adjWeibull(x, amplitude = 10, shape = 2, plot = TRUE, main = NULL,
#' title.col = "black", mid = TRUE, line.col = "blue", legendPos = "topright",
#' lowLim = NULL, ymax = NULL, bg = "aliceblue", method = "mps", cex.axis = 0.6,
#' cex.lab = 0.8, las = 1, xlab = "Diameter class (cm)",
#' ylab = "Relative frequency (\%)", ...)
#'
#' @param x numeric, vector of observations.
#' @param amplitude numeric, amplitude of classes.
#' @param shape numeric, optional initial values of shape for starting the
#' iterative procedures such as Newton-Raphson.
#' @param plot logical. Should plot?
#' @param main character, overall title for the plot.
#' @param title.col,line.col the color to be used for the overall title and
#' the plot line plot respectively. Default are "blue" and "black".
#' @param mid logical. Should the line stop at the first and last classes middle?
#' @param legendPos character, keyword which is accepted by
#' \code{\link[grDevices]{xy.coords}}. To be used to position the legend.
#' Default is "topright".
#' @param lowLim,ymax numeric, xlim and ylim lowest values.
#' @param bg the color to be used for the background of the legend box.
#' @param method used method for estimating the three-parameter Weibull
#' distribution. See \code{\link[ForestFit]{fitWeibull}} for details.
#' @param cex.axis,cex.lab,las graphical parameters.
#' See \code{\link[graphics]{par}}.
#' @param xlab,ylab title for the x and y axis.
#' @param ... additional arguments to pass through \code{\link[base]{plot}}.
#'
#' @examples
#' set.seed(2)
#' d <- rweibull(85, shape = 1, scale = 30) + 5
#' res <- adjWeibull(d,
#'   amplitude = 10, mid = FALSE, shape = 3, ymax = 30,
#'   main = "Weibull adjustment", line.col = "red", legendPos = "right",
#'   method = "mps"
#' )
#' res
#' @import ForestFit graphics
#' @export
#'
adjWeibull <- function(x, amplitude = 10, shape = 2, plot = TRUE, main = NULL,
                       title.col = "black",
                       mid = TRUE, line.col = "blue", legendPos = "topright",
                       lowLim = NULL, ymax = NULL, bg = "aliceblue",
                       method = "mps", cex.axis = 0.6, cex.lab = 0.8, las = 1,
                       xlab = "Diameter class (cm)",
                       ylab = "Relative frequency (%)", ...) {
  MIN <- min(x)
  MAX <- max(x)
  brk <- hist(x, breaks = seq(MIN, MAX + 1, 1), plot = FALSE)

  # initial values determination
  # loc <- MIN <-  min(x)
  scale <- which(cumsum(brk$density) >= 0.632)[1]
  scale <- brk$mids[scale] - MIN
  initials <- c(shape, scale, MIN)

  # estimation of parameters
  est <- ForestFit::fitWeibull(
    data = x, location = TRUE, method = method,
    starts = initials
  )

  estimate <- setNames(c(est$estimate), nm = c("shape", "scale", "location"))
  measures <- setNames(c(est$measures), nm = colnames(est$measures))

  est <- list(estimate = estimate, measures = measures)
  rm(list = c("estimate", "measures"))

  loc <- est$estimate[3]
  scale <- est$estimate[2]
  shape <- est$estimate[1]

  # Run the KS test
  new_pweibull <- function(x) {
    pweibull(x - loc, shape, scale)
  }

  ks_result <- ks.test(x, new_pweibull)

  est$measures["KS_p.value"] <- ks_result$p.value
  est$note <- "KS alternative hypothesis is that data do not follow the specified Weibull distribution."

  if (plot) {
    f <- function(x, ampl = amplitude) {
      100 * ampl * dweibull(x - loc, shape = shape, scale = scale, log = FALSE)
    }

    lowLim <- ifelse(is.null(lowLim), MIN, lowLim)
    brk <- hist(x, breaks = seq(lowLim, MAX + amplitude, amplitude), plot = FALSE)
    brk$density <- rfreq(brk$density)

    ymax <- ifelse(is.null(ymax),
      min(max(brk$density) + diff(range(brk$density)) * .2, 100), ymax
    )

    minb <- min(brk$breaks)
    maxb <- max(brk$breaks)
    plot(brk,
      freq = FALSE, xlim = c(minb, maxb), las = las, xlab = xlab,
      ylab = ylab, cex.axis = cex.axis, cex.lab = cex.lab, ylim = c(0, ymax),
      main = main, ...
    )

    if (mid) {
      curve(f, col = line.col, add = T, from = min(brk$mids), to = max(brk$mids))
    } else {
      curve(f,
        col = line.col, add = T, from = min(brk$breaks),
        to = max(brk$breaks)
      )
    }

    legend(legendPos,
      title.col = title.col, lty = c(1, 0, 0, 0), lwd = 1,
      col = line.col, box.lty = 0, bty = "o", bg = bg, cex = 1,
      legend = c(
        "weibull",
        bquote(alpha == .(round(shape, 2))),
        bquote(beta == .(round(scale, 2))),
        bquote(theta == .(round(loc, 2)))
      )
    )
  }
  return(invisible(est))
}



# END ####


#' Abbreviates a Botanical or Zoological Latin Name into an Eight-character
#' from 'Gender epithet' to 'G. epithet'
#' @description To abbreviate  species name from 'Gender epithet' to
#' 'G. epithet'.
#' Useful in plots with species names.
#' @param name a factor coercible vector of species name in forms
#' 'Gender epithet'.
#' @param sep character string which separates Gender and epithet.
#' Default is space " ".
#' @details Returned reduced names are made unique.
#' @return A factor vector of species reduced names in forms 'G. epithet'.
#' @seealso \code{\link[vegan]{make.cepnames}} in \code{vegan} package.
#' @export
spNmReduce <- function(name, sep = " ") {
  name <- as.factor(name)
  nm <- strsplit(levels(name), sep)
  nm <- sapply(nm, function(sp) {
    paste(substr(sp[1], 1, 1), ". ", paste(sp[-1], collapse = " "), sep = "")
  })
  levels(name) <- make.unique(nm)
  return(name)
}
