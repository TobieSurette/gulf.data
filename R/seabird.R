#' \strong{Seabird} Data
#'
#' @description Functions to read, manipulate and analyze \strong{seabaird} probe data.
#'
#' @param x An \code{seabird} object, data file, survey year or keyword search term.
#' @param year Numeric value specifying the survey or project year(s).
#' @param full.names Logical value specifying whether to include file paths when searching for \strong{seabird} data files.
#' @param header \strong{seabird} file header information to be assigned as metadata.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param remove Character string specifying keyword(s) which, if found in the data file or path, are removed from the search results.
#'

#' @export
seabird <- function(x, ...) UseMethod("seabird")

#' @describeIn seabird Default \code{seabird} method. Create an \code{seabird} object. 
#' @rawNamespace S3method(seabird,default)
seabird.default <- function(x, ...){
   # Define as probe data object:
   #x <- probe(x, ...)
   
   # Add 'seabird' class tag:
   class(x) <- unique(c("seabird", class(x)))
   
   return(x)
}
