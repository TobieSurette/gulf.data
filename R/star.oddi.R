#' \strong{Star Oddi} Data
#'
#' @description Functions to read, manipulate and analyze \strong{star.oddi} trawl acoustic monitoring  probe data.
#'
#' @param x An \code{star.oddi} object, data file, survey year or keyword search term.
#' @param year Numeric value specifying the survey or project year(s).
#' @param full.names Logical value specifying whether to include file paths when searching for \strong{star.oddi} data files.
#' @param header \strong{star.oddi} file header information to be assigned as metadata.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param remove Character string specifying keyword(s) which, if found in the data file or path, are removed from the search results.
#'
#' @examples
#' # Star Oddi files for the 2020 snow crab survey:
#' locate.star.oddi(year = 2020)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.star.oddi("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.star.oddi(2018, tow.id = "GP354")
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#' plot(x)      # Graphical summary.
#' summary(x)   # Data summary.

#' @export
star.oddi <- function(x, ...) UseMethod("star.oddi")

#' @describeIn star.oddi Default \code{star.oddi} method. Create an \code{star.oddi} object. 
#' @rawNamespace S3method(star.oddi,default)
star.oddi.default <- function(x, ...){
   # Define as probe data object:
   #x <- probe(x, ...)
   
   # Add 'star.oddi' class tag:
   class(x) <- unique(c("star.oddi", class(x)))
   
   return(x)
}
