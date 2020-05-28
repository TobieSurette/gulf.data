#' Assign or Retrieve Data Format
#'
#' @description Functions to assigns or retrieve data format(s) to and from an object.
#'
#' @param x Target object.
#' @param y Character string(s) specifying the name(s) of the variables or attributes to be assigned data formats.
#' @param value Character string(s) specifying the data format to be assigned.
#' @param ... Other arguments (not used).
#'
#' @details Named argiments in the format that are not in the target object are ignored.
#'          The \code{format} attribute may be erased by assigning a \code{NULL} or empty 
#'          string \code{""} value to it.
#'
#' @return Object with a \code{format} attribute attached to it.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{format.default}}{Default \code{format} method.}
#'   \item{\code{format<-}}{Generic \code{format} assignment method.}
#'   \item{\code{format<-.default}}{Default \code{format} assignment method. See Examples for usage.}
#' }
#' 
#' @examples
#' x <- data.frame(year = 2010:2014, month = 6, day = 1:5, measurement = rnorm(5))
#' format(x, "year")  <- "YYYY"
#' format(x, "month") <- "MM"
#' format(x, "day")   <- "DD"
#'
#' # Equivalently:
#' format(x) <- c(year = "YYYY", month = "MM", day = "DD")
#' format(x, c("year", "month", "day")) <- c("YYYY", "MM", "DD")
#'
#' # Show attributes:
#' attributes(x)
#' format(x)
#'
#' # Erase 'format' attribute:
#' format(x) <- NULL
#'
#' @export format.default
#' @export "format<-"
#' @export "format<-.default"
#' 
#' @seealso \code{\link{metadata}}, \code{\link{key}}, \code{\link{description}}, \code{\link{units}}

#' @rdname format 
format.default <- function(x, ...){
   f <- attr(x, "format")
   if (!is.null(f)) return(f) else base::format(x, ...)
}

#' @rdname format 
"format<-" <- function(x, ...) UseMethod("format<-")

#' @rdname format 
"format<-.default" <- function(x, y, value, ...){
   if (missing(y)){
      # Extract attributes fields from 'value':
      if (!is.null(names(value))){
         format(x, names(value)) <- value
         return(x)
      }
      # Assign attribute:
      attr(x, "format") <- value
   }else{
      if (!is.character(y)) stop("Named argument must be a character string(s).")
     # if (!any(y %in% names(x))) stop("Named argument must be in target object.")
      tmp <- attr(x, "format")
      if (!is.null(value)) if (all(value == "")) value <- NULL
      if (is.null(value)){
         tmp <- tmp[setdiff(names(tmp), y)]
      }else{
         if ((length(value) == 1) & (length(y) > 1)) value <- rep(value, length(y))
         if (length(y) != length(value)) 
            stop("Named arguments must be the same length as assigned values.")
         i <- y %in% names(x)
         y <- y[i]; value <- value[i]
         tmp[y] <- as.vector(value)
      }
      attr(x, "format") <- tmp
   }

   return(x)
}

