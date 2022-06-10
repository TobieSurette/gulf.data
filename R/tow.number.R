#' @title Extract Tow Number 
#' 
#' @description Functions to extract or determine trawl tow numbers.
#' 
#' @param x Data object.
#' @param method Character string specifying the method to be used when determining the tow identification number for a 
#'               \code{probe} object. Available methods are \sQuote{time} and \sQuote{latlong}.
#' @param max.distance Numeric valiue specifying the maximum distance tolerance (in kilometers) when determining the tow 
#'                     identification number for a \code{probe} object from lat-lon coordinates.         
#'
#' @section Methods:
#' \describe{
#'    \item{\code{tow.number}}{Generic \code{tow.number} method.}
#'    \item{\code{tow.number.default}}{Default \code{tow.number} method.}
#'    \item{\code{tow.number.probe}}{Determine tow ID for a \code{probe} object.}
#' }
#' 

#' @export tow.number
tow.number <- function(x, ...) UseMethod("tow.number")

#' @describeIn tow.number Default 'tow.number' method.
#' @rawNamespace S3method(tow.number,default)
tow.number.default <- function(x, ...){
   # Find tow number field:
   ix <- grep("tow[._]*n[ou][mber]*", tolower(names(x)))
   if (length(ix) > 0) return(x[ix[1]])
   
   # Search attributes:
   a <- attributes(x)
   ix <- grep("tow[._]*n[ou][mber]*", tolower(names(a)))
   if (length(ix) > 0) return(a[[ix[1]]])
   
   return(NULL)
} 

#' @describeIn tow.number Default 'tow.number' method.
#' @rawNamespace S3method(tow.number,data.frame)
tow.number.data.frame <- function(x, ...){
   # Find data column:
   ix <- grep("tow[._]*n[ou][mber]*", tolower(names(x)))
   if (length(ix) > 0) return(x[, ix[1]])
   
   # Search attributes:
   a <- attributes(x)
   ix <- grep("tow[._]*n[ou][mber]*", tolower(names(a)))
   if (length(ix) > 0) return(a[[ix[1]]])
   
   return(NULL)
}


