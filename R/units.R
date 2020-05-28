#' Measurement Units
#'
#' @description Assigns or retrieves measurement units assigned to observed data.
#'
#' @param x Target object.
#' @param y Character string(s) specifying the name(s) of the variables or attributes to be assigned measurement units.
#' @param ... Other arguments (not used).
#' @param value Character string(s) specifying the units to be assigned.
#'
#' @details The \code{units} attribute may be erased by assigning a \code{NULL} value to it.
#'
#' @return An object with a \code{units} attribute attached to it.
#'
#' @section Functions:
#' \describe{
#'   \item{\code{units}}{Generic \code{units} extraction method.}
#'   \item{\code{units.default}}{Default \code{units} extraction method.}
#'   \item{\code{units<-}}{Generic \code{units} assignment method. See Examples for usage.}
#'   \item{\code{units<-.default}}{Default \code{units} assignment method. See Examples for usage.}
#' }
#' 
#' @examples
#' x <- data.frame(year = 2010:2014, measurement = rnorm(5))
#' units(x, "measurement") <- "centimeters"
#'
#' # Show attributes:
#' attributes(x)
#'
#' # Erase 'units' attribute:
#' units(x) <- NULL
#'
#' @export "units<-"
#' @export "units<-.default"
#' @export "units"
#' @export "units.default"
#'
#' @seealso \code{\link{metadata}}, \code{\link{key}}, \code{\link{description}}, \code{\link{format}}

#' @rdname units
units <- function(x, ...) UseMethod("units")

#' @rdname units
units.default <- function(x, ...) return(attr(x, "units"))

#' @rdname units
"units<-" <- function(x, ...) UseMethod("units<-")

#' @rdname units
"units<-.default" <- function(x, y, value, ...){
   if (missing(y)){
      # Extract attributes fields from 'value':
      if (!is.null(names(value))){
         units(x, names(value)) <- value
         return(x)
      }
      # Assign attribute:
      attr(x, "units") <- value
   }else{
      if (!is.character(y)) stop("Named argument must be a character string(s).")
      if (!all(y %in% names(x))) stop("Named argument must be in target object.")
      tmp <- attr(x, "units")
      if (is.null(value)){
         tmp <- tmp[setdiff(names(tmp), y)]
      }else{
         if ((length(value) == 1) & (length(y) > 1)) value <- rep(value, length(y))
         if (length(y) != length(value)) stop("Named arguments must be the same length as assigned values.")
         tmp[y] <- as.vector(value)
      }
      attr(x, "units") <- tmp
   }

   return(x)
}

