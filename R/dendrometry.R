
#' @details
#' Type \code{RShowDoc("dendrometry", package = "dendrometry")} to read a HTML
#' user guide vignette.
#'
#' Type \code{RShowDoc("dendrometry_pdf", package = "dendrometry")} to read a
#' PDF user guide vignette.
#'
#' Type \code{demo(dendro, package = "dendrometry")} for a demo of dendrometric computations. Click on \code{Index} bellow to see the index of the package.
#'
#' Type \code{demo(volume, package = "dendrometry")} for a demo of dendrometric computations. Click on \code{Index} bellow to see the index of the package.
#' @keywords internal
"_PACKAGE"


#' Dendrometric measures on tree
#' @docType data
#' @description Data frame of 10 rows and 5 columns containing tree measures.
#' @format Data frame with ten observations and five variables:
#' \describe{
#'   \item{circum}{Tree circumference in centimeter (cm).}
#'   \item{dist}{Horizontal distance between the observer (person who measure angles) and the tree circumference in centimeter (cm).}
#'   \item{up}{Up angle measure in degree (°).}
#'   \item{down}{Down angle measure in degree (°).}
#'   \item{fut}{Bole angle measure in degree (°); Fut is where the first branch occurs on the trunk. This measure is usually useful for timber estimation on wood market.}
#' }
#' @usage data(Tree)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Tree"

#' Tree metrics for logging (in progress)
#' @docType data
#' @description Data frame of 24 rows and 8 columns containing tree measures.
#' @format Data frame with twenty five observations and eight variables:
#' \describe{
#'   \item{tree}{Tree name}
#'   \item{hauteur}{Stem lenght in meter (m).}
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
#' @examples #demo(dendro)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Logging"

#' Making factor vectors
#' @description Changes character vectors to factor vectors
#' @aliases factorise
#' @param data A data set containing ....
#' @param binary Logical indicating if binary data should be considered as factor
#' @return Data frame with all character vectors changed to factor vectors
#' @export
factorize <- function(data, binary = FALSE){
  for (i in 1:dim(data)[2]) {
    if(is.character(data[[i]]) ||
       (binary && is.numeric(data[[i]]) &&
        length(levels(as.factor(data[[i]]))) == 2))
      data[[i]] <-  as.factor(data[[i]])

    else data[[i]] <- data[[i]]
  }
  return(as.data.frame(data))
}

#' Fibonacci series
#' @description Generates numbers from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first number of the series.
#' @param PrintFib Logical, indicating if the series should be printed.
#' @return Either a real number, result of the function or a vector of all
#' the series.
#' @examples fibonacci(n = 10, PrintFib = TRUE)
#' fibonacci(n = 10, Uo = 1, U1 = 3, PrintFib = FALSE)
#' #' @details The series equation is Un = U_(n-2) /U_(n-1).
#' @seealso \code{\link{fiboRate}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export
fibonacci <- function(n, PrintFib = FALSE, Uo = 0, U1 = 1){
  Un <- numeric(length = n)
  Un[1:2] <- c(Uo, U1)

  if(n < 2) return(Uo)

  else if (n > 2){
    for (i in 3:n){
      Un[i] <- Un[i-1] + Un[i-2]
    }
    Fib <- Un
  }
  else
    Fib <- Un

  if(!PrintFib)
    Fib <- Fib[n]

  return(Fib)
}

#' Fibonacci series ratio
#' @description Computes rates from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first number of the series.
#' @param PrintSer Logical, indicating if the series should be printed.
#' @return Either a real number, result of the rate of nth and (n-1)th numbers
#' in Fibonacci series.
#' @examples ##Golden number (Le Nombre d'Or)
#' fiboRate(n = 18, PrintSer = FALSE, Uo = 0, U1 = 1)
#' ##(1+sqrt(5))/2
#' fiboRate(n = 10, PrintSer = TRUE, Uo = 0, U1 = 1)
#' @details The series equation is Un = U_(n-2) /U_(n-1).
#' The function returns golden number when Uo = 0, and U1 = 1. Larger n is, more precise the number (result) is.
#' @seealso \code{\link{fibonacci}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export

fiboRate <- function(n = 10, PrintSer = FALSE, Uo = 0, U1 = 1){
  a <- fibonacci(n = n, Uo = Uo, U1 = U1, PrintFib = TRUE)
  #$$U_n = \frac{U_{n-2}}{U_{n-1}}$$
  serie <- a[2:n]/a[1:n-1]
  if(PrintSer) return(serie)
  else return(serie[n-1])
}

#' Radians to degrees
#' @description Converts angle values from  radian to degree.
#' @param radian A vector of degree values to be converted
#' @return Returns a vector of radian values.
#' @examples deg(pi/2)
#' @seealso \code{\link{rad}}, the complement of \code{deg}
#' @export

deg <- function (radian)
{
  radian * (180/pi)
}

#' Degrees to radians
#' @description Converts angle values from  degree to radian.
#' @param degree A numeric vector of radian values to be converted
#' @return Returns a vector of radian values.
#' @examples rad(180)
#' @seealso \code{\link{deg}}, the the complement of \code{rad}
#' @export
rad <- function (degree)
{
  degree * (pi/180)
}

#' Height of tree or vertical object.
#' @description Computes the height of tree, pillar, girder, mast or any
#' vertical object. Allows both slope (in per cent) and angle measures
#' (in degree or radian) . No matter the relative position of the persons who
#' measures angle/slope.
#' @usage height(distance, top, base, type = c("angle", "slope"),
#'        angleUnit = c("deg", "rad"))
#' @param distance a numeric vector of the horizontal distance between object
#' and the person who measures angle.
#' @param top,base numeric vector of top angle and ground angle respectively
#' (readings from a clinometer).
#' @param type the type of \code{top} and \code{base} measures. Either
#' \code{"angle"} or \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit the unit of \code{top} and \code{base} measures when
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#' @return Returns a vector of heights.
#' @examples height(10, 80, 17)
#' height(17, top = -18, base = -113)
#' height(distance = 18, top = 42, base = -12, type = "angle", angleUnit = "deg")
#' height(distance = 18:21, top = 42:45, base = -12:-15, type = "angle", angleUnit = "deg")
#' ## Bellow shows warning messages
#' height(distance = 18:21, top = -42:-45, base = -12:-15, type = "angle", angleUnit = "deg")
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export
height <- function(distance, top, base, type = c("angle", "slope"),
                   angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") {
    VH <- rad(top)
    VB <- rad(base)
  }
  else if (!(angleUnit %in% c("deg", "rad"))) {
    stop("angleUnit should be either  'deg' or 'rad'")
  }

  if (prod(type == c("angle", "slope"))) {
    type <- "slope"
  }
  else if (!(type %in% c("angle", "slope"))) {
    stop("type should be either  'angle' or 'slope'")
  }

  if (sum(top <= base))
    warning("One or more top angles are less than their down angles.
          Please check your data")

  if (type ==  "slope") {
    h <- 0.01*distance * (top - base)
  }
  else
    h <- distance * (tan(VH) - tan(VB))
  return(h)
}

#' Diameter or DBH
#' @description Computes diameter based on circumference. If circumference (perimeter) at breast height is given, then Diameter at Breast Height (\code{DBH}) is obtained. Used in dendrometry for trees' \code{DBH} calculation.
#' @param perimeter numeric vector of circumference.
#' @return Diameter or \code{DBH} vector.
#' @examples x = seq(1, 5, .4)
#' dbh(x)
#' @seealso \code{\link{height}}
#' @export
dbh <- function(perimeter){
  if (is.numeric(perimeter))
    return(perimeter/pi)
  else stop("must be numeric")
}


#' Angle to slope
#' @description Converts angle values to  slope values.
#' @param angle numeric vector of angle to be converted to slope.
#' @param  angleUnit The unit of \code{angle}. Either \code{"deg"}, \code{"rad"}. Default is \code{"deg"}.
#' @return Returns a vector of slope values.
#' @examples angle2slope(10)
#' angle2slope(angle = 45)
#' angle2slope(angle = 50, angleUnit = "deg")
#' angle2slope(1.047198, "rad")
#' angle2slope(0.2617994, angleUnit = "rad")
#' @seealso \code{\link{slope2angle}}, the the complement of \code{angle2slope}.
#' @export
angle2slope <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    angle <- rad(angle)

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")
  slp <- 100 * tan(angle)
  return(slp)
}

#' Slope to angle
#' @description Converts slope values to angle values.
#' @param slope numeric vector of slope to be converted to angle.
#' @param  angleUnit The desired unit for the returned angle value. Either "deg", "rad". Default is "deg".
#' @return A vector of angle values in specified unit.
#' @examples slope2angle(100)
#' slope2angle(17.6327)
#' slope2angle(angle2slope(30))
#' @seealso \code{\link{angle2slope}}, the the complement of \code{slope2angle}
#' @export
slope2angle <- function(slope, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    return(deg(atan(.01 * slope)))

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")

  return(atan(.01 * slope))
}

#' Horizontal distance
#' @description Horizontal distance calculation for sloping area.
#' @param distance numeric vector of the distance measured on sloping area.
#' @param angle numeric vector of angle values.
#' @param type the type of \code{angle}. Either \code{"angle"} or
#' \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit the unit of \code{angle} measures when
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#' @return The horizontal distance.
#' @examples distanceH(20, 30)
#' distanceH(20, angle = 30, type = "slope")
#' distanceH(20, angle = 25, type = "angle")
#' @export
#'
distanceH <- function(distance, angle, type = c("angle", "slope"),
                     angleUnit = c("deg", "rad")){
   if (prod(type == c("angle", "slope")) || type == "slope")
      d <- distance * cos(slope2angle(slope = angle, angleUnit = "rad"))
   else{
      if(prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
         d <- distance * cos(rad(angle))
      else if (angleUnit == "rad")
         d <- distance * cos(angle)
      }
   return(d)
}


#' Principal measure
#' @description \code{principal} returns the principal measure of an angle value.
#' Principal measure ranges from -pi to pi for radian unit while it ranges from
#' -180 to 180 for degree unit.
#' @param angle numeric vector of angle.
#' @param  angleUnit The unit of \code{angle}. Either "deg", "rad".
#' Default is "deg".
#' @return A matrix of principal measure of angle in radian and in degree units.
#' @export
#' @seealso \code{\link{rad}} for radian, \code{\link{deg}} for degree,
#' \code{\link{slope2angle}} for slope to angle conversion,
#' \code{\link{angle2slope}} for angle to slope conversion.
#' @examples principal(303)
#' principal(23 * pi/8, "rad")
#' @note Use \code{principal} in position computations, not distance computations.
principal <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    angle <- rad(angle)

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")
  radian <- Arg(complex(real = cos(angle), imaginary = sin(angle)))
  degree <- deg(radian)
  return(cbind(radian, degree))
}

#' Individual basal area
#' @param dbh numeric vector of diameter.
#' @return Vector of individual basal area.
#' @examples basal_i(dbh = 0.1)
#' @export
basal_i <- function(dbh){
  return(pi * .25 * dbh**2)
}

#' Lorey's mean height
#' @description The average height of the trees in a plot, weighted by their basal area.
#' @param basal numeric, individual basal areas.
#' @param  height numeric vector of individual heights.
#' @return Average Lorey height of a species.
#' @seealso \code{\link{height}}, \code{\link{basal_i}}
#' @examples set.seed(1)
#' donnee <- data.frame(hauteur = rnorm(10, 12, 3), area = basal_i(rnorm(10, 100, 20)))
#' loreyHeight(basal = donnee$area, height = donnee$hauteur)
#' @export
loreyHeight <- function(basal, height){
  sum(basal * height)/sum(basal)
}

#' Mean diameter
#' @description Mean diameter of a species.
#' @param dbh numeric vector of diameter (\code{DBH}).
#' @return Mean diameter of a species.
#' @seealso \code{\link{dbh}}, \code{\link{basal_i}}
#' @examples set.seed(1)
#' diameter = rnorm(10, 100, 20)
#' diameterMean(dbh = diameter)
#' @export
diameterMean <- function(dbh){
  sqrt(mean(dbh**2))
}


#' Tree density
#' @description Density per plot.
#' @param number numeric vector, individual count.
#' @param area numeric, area of a plot.
#' @param overall a logical value. If \code{TRUE}, an overall density is
#' computed; if \code{FALSE}, density is computed for each plot.
#' Default is \code{TRUE}.
#' @return Vector of density.
#' @export
densityTree <- function(number, area, overall = TRUE){
  if (!overall) {
    return(number/area)
  } else return(mean(number/area))
}

#' The decrease coefficient
#' @description This coefficient expresses the ratio between the diameter
#' (or circumference) at mid-height of the bole and the diameter
#' (or circumference) measured at breast height.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Don't mix.
#' @return The decrease coefficient
#' @examples decrease(30, 120)
#' decrease(middle = 40, breast = 90)
#' @export
decrease <- function(middle, breast){
  if(!is.numeric(middle))
    stop("'middle' should be numeric")

  if(!is.numeric(breast))
    stop("'breast' should be numeric")

  if(any(middle > breast))
    warning("One or more breast value are less than their middle values.
          Please check your data")
  return(middle/breast)
}


#' The reduction coefficient
#' @description The reduction coefficient is the ratio between the difference
#' in size at breast height and mid-height on the one hand, and the size at
#' breast height on the other. . It is thus the complement to 1 of the
#' coefficient of decrease.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Don't mix.
#' @return The reduction coefficient.
#' @examples reducecoef(30, 120)
#' reducecoef(middle = 40, breast = 90)
#' @export
reducecoef <- function(middle, breast){
  if(!is.numeric(middle))
    stop("'middle' should be numeric")

  if(!is.numeric(breast))
    stop("'breast' should be numeric")

  if(any(middle > breast))
    warning("One or more breast value are less than their middle values.
          Please check your data")
  r <- (breast - middle)/breast
  return(r)
}


#' Metric scrolling or decay
#' @description The average metric decay expresses the difference, in
#' centimeters per meter, between the diameter (or circumference) at breast
#' height and its diameter at mid-height of a stem related to the difference
#' between the height at mid-height and that at breast height.
#' @param dmh numeric, the diameter at middle height.
#' @param dbh numeric, the diameter at breast height.
#' @param mh numeric, the middle (or cut) height.
#' @param bh Either a numeric value standing for the breast height of all trees or a numeric vector standing for the breast height of each tree. Default is \code{1.3}.
#' @return Metric decay
#' @examples decreaseMetric(dmh = 40, dbh = 90, mh = 7)
#' decreaseMetric(45, 85, 9)
#' @export
decreaseMetric <- function(dmh, dbh, mh, bh = 1.3){
  if(!is.numeric(dbh))
    stop("'dbh' should be numeric")
  if(!is.numeric(dmh))
    stop("'dmh' should be numeric")
  if(!is.numeric(mh))
    stop("'mh' should be numeric")

  if(any(dmh > dbh))
    warning("One or more middle height diameter are greater than their DBH.
            Please check your data")
  if(any(mh <= bh))
    warning("One or more middle height are greater or equal to their breast height.
            Please check your data")
  d <- (dbh - dmh)/(mh - bh)
  return(d)
}

.huberMethod <- function(height, dm, circum, successive, log){

  if(is.null(dm) && is.null(circum))
    stop("Specify either 'dm' or 'circum'")
  else if(!is.null(dm) && !is.null(circum))
    warning("Don't specify both 'dm' (diameter) and 'circum' (circumference).
            Only 'dm' is considered.")

  if(is.null(dm))
    dm <- dbh(circum)
  v <- .25 * pi * dm**2 * height

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))

  return(v)
}
.smalianMethod <- function(height, do, ds, circumo, circums, successive,
                           log){
  # Don't specify mixture of args
  if(all(any(is.null(circumo), is.null(circums)),
         any(is.null(do), is.null(ds))))
    stop("Specify either both 'circumo' and 'circums' or both 'do' and 'ds'
           when using 'smalian' method.")

  else if(sum(!is.null(do), !is.null(ds), !is.null(circumo),
              !is.null(circums)) > 2)
    warning("Don't specify both diameters and circumferences.")
  # le && est inutile
  if(!is.null(do) && !is.null(ds))
    v <- .125 * pi * (do**2 + ds**2) * height
  else if(!is.null(circumo) && !is.null(circums))
    v <- .125 * pi * (dbh(circumo)**2 + dbh(circums)**2) * height

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}
.coneMethod <- function(height, do, ds, circumo, circums, successive, log){
  # Don't specify mixture of args
  if(all(any(is.null(circumo), is.null(circums)),
         any(is.null(do), is.null(ds))))
    stop("Specify either 'circumo' and 'circums' or 'do' and 'ds'
           when using 'cone' method.")

  else if(sum(!is.null(do), !is.null(ds), !is.null(circumo),
              !is.null(circums)) > 2)
    warning("Don't specify both diameters and circumferences.")
  # le && est inutile
  if(!is.null(do) && !is.null(ds))
    v <- pi * (do**2 + do * ds + ds**2) * height/12
  else if(!is.null(circumo) && !is.null(circums))
    v <- pi * (dbh(circumo)**2 + dbh(circumo) * dbh(circums) + dbh(circums)**2) *
        height/12
  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}
.newtonMethod <- function(height, do, dm, ds, circumo, circum, circums,
                          successive, log){
  if(all(any(is.null(circum), is.null(circumo), is.null(circums)),
         any(is.null(dm), is.null(do), is.null(ds))))
    stop("Specify either 'circum', 'circumo' and 'circums' or 'dm', 'do' and 'ds'
           when using 'newton' method.")
  #"Specify either only diameters or only circumferences."
  else if(sum(is.null(dm), !is.null(do), !is.null(ds), is.null(circum),
              !is.null(circumo), !is.null(circums)) > 3)
    warning("Don't specify both diameters and circumferences.")

  if(!is.null(dm) && !is.null(do) && !is.null(ds))
    v <- pi * (do**2 + 4 * dm**2 + ds**2) * height/24
  else if(!is.null(circum) && !is.null(circumo) && !is.null(circums))
    v <- pi * (dbh(circumo)**2 + 4 * dbh(circum)**2 + dbh(circums)**2) *
        height/24

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}


#' Tree stem and log Volume
#' @description Determining the volume of the log or of the tree.
#' @usage volume(height, dm, do, ds, circum, circumo, circums,
#'        method = "huber", successive = FALSE, log)
#' @param height numeric, stem (whole bole) length. When \code{successive} is "\code{TRUE}",
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
#' volume(height = 10, circum = 100, circumo = 200, circums = 110, method = "newton")
#' @return A numeric vector of logs or trees volume.
#' @details Using \code{method = cone} refers to truncated cone method.
#' @seealso \code{\link{shape}}, for tree real volume.
#' @export
volume <- function(height, dm = NULL, do = NULL, ds = NULL, circum = NULL,
                   circumo = NULL, circums = NULL, method = "huber",
                   successive = FALSE, log = NULL){
  if(!(method %in% c("huber", "smalian", "cone", "newton")))
    stop("'method' should be one of 'huber', 'smalian', 'cone', or 'newton'.")

  if(all(!successive, !is.null(log)))
    warning("Don't specify 'log' when 'successive' is not TRUE")
  if(method == "huber") return(.huberMethod(height = height, dm = dm,
                                           circum = circum, log = log,
                                           successive = successive))
  else if(method == "smalian") return(.smalianMethod(height = height, do = do,
                                                      circumo = circumo,
                                                      ds = ds,
                                                      circums = circums,
                                                      log = log,
                                                      successive = successive))

  else if (method == "cone") return(.coneMethod(height = height, do = do,
                                               ds = ds, circumo = circumo,
                                               circums = circums,
                                               successive = successive,
                                               log = log))
  else if (method == "newton") return(.newtonMethod(height = height, do = do,
                                                   dm = dm, ds = ds,
                                                   circumo = circumo,
                                                   circum = circum,
                                                   circums = circums,
                                                   successive = successive,
                                                   log = log))
}


#' The shape coefficient
#' @description The shape coefficient of the tree is the ratio of the actual
#' volume of the tree to the volume of a cylinder having as base the surface of
#' the section at 1.3 m (or a given breast height) and as length, the height of
#' the tree.
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
shape <- function(volume, height, dbh = NULL, basal = NULL){
  if(all(is.null(dbh), is.null(basal)))
    stop("Specify either 'dbh' or 'basal'")
  else if((!any(is.null(dbh), is.null(basal))))
    warning("Both of 'dbh' and 'basal' are specified. Only 'dbh' is considered.")

  if(!is.null(dbh))
    f <- volume/(basal_i(dbh = dbh) * height)
  else
    f <- volume/(basal * height)
  return(f)
}

#' Skewness coefficient
#' @param x numeric vector.
#' The skewness.
#' @examples data("Logging")
#' skewness(Logging$hauteur)
#' hist(Logging$hauteur,3)
#' @return The skewness coefficient.
#' @import stats
#' @export
skewness <- function(x){
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sk <- n * (sum(((x - m)/s)**3))/((n - 1) * (n - 2))
  return(sk)
}

#' Index of Blackman
#' @param density numeric vector of the density.
#' @return Index of Blackman.
#' @export
blackman <- function(density) {
  b <- var(density)/mean(density)
  return(b)
}

#' Index of Green
#' @param density numeric vector of the density.
#' @return Index of Green.
#' @export
green <- function(density) {
  g <- (blackman(density) - 1)/(length(density) - 1)
  return(g)
}
