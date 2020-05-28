#' Index Key Functions
#'
#' @description Functions to assign, retrieve or check an index key for a given object.
#'
#' @param x Object.
#' @param ... Other arguments (not used).
#' @param value Character string(s) specifying the index key to be assigned.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{key}}{Default \code{key} method.}
#'   \item{\code{key.default}}{Default \code{format} retrieval method. If no \code{key} attribute is 
#'                             defined, then a call to \code{\link[base]{format}} is made.}
#'   \item{\code{key<-}}{Generic \code{key} assignment method. See Examples for usage.}
#'   \item{\code{key<-.default}}{Default\code{key} assignment method. See Examples for usage.} 
#'   \item{\code{is.key}}{Default\code{is.key} method.}
#'   \item{\code{is.key.data.frame}}{Check validity of an index key for a \code{data.frame} object.}      
#' }
#' 
#' @examples
#' # Build sample data:
#' x <- data.frame(year = 1990:2014, measurement = rpois(25))
#' key(x) <- "year" # Assign key.
#' key(x) # Retrieve key.
#'
#  # Check index keys:
#' is.key(x, "measurement") # Generally FALSE
#' is.key(x, "year")
#' is.key(x)  # No need to specify 'year' since it is already defined.
#' 
#' @export key
#' @export key.default
#' @export "key<-"
#' @export "key<-.default"
#' @export is.key
#' @export is.key.data.frame
#'
#' @seealso \code{\link{metadata}}, \code{\link{description}}, \code{\link{units}}, \code{\link{format}}
#'

#' @rdname key
key <- function(x, ...) UseMethod("key")

#' @rdname key
key.default <- function(x, ...) return(attr(x, "key"))

#' @rdname key
"key<-" <- function(x, ...) UseMethod("key<-")

#' @rdname key
"key<-.default" <- function(x, value){
   if (!is.null(value)){
      if (!is.character(value)) stop("Key must contain variable name(s).")
      if (!all(value %in% names(x))) stop("Variable name(s) not in target object.")
   }

   # Assign key:
   attr(x, "key") <- value

   return(x)
}

#' @rdname key
is.key <- function(x, ...) UseMethod("is.key")

#' @rdname key
is.key.data.frame <- function(x, key, ...){
   if (missing(key)) if ("key" %in% names(attributes)) key <- attr(x, "key")
   if (missing(key)) stop("Index 'key' is unspecified.")
   key <- as.character(key)
   if (!all(key %in% names(x))) stop("Some 'key' variables are not in 'x'.")
   return (!any(duplicated(x[key])))
}
