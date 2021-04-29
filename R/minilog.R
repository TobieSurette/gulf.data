#' \strong{Minilog} Data
#'
#' @description Functions to read, manipulate and analyze \strong{minilog} trawl acoustic monitoring  probe data.
#'
#' @param x An \code{minilog} object, data file, survey year or keyword search term.
#' @param year Numeric value specifying the survey or project year(s).
#' @param full.names Logical value specifying whether to include file paths when searching for \strong{minilog} data files.
#' @param header \strong{minilog} file header information to be assigned as metadata.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param remove Character string specifying keyword(s) which, if found in the data file or path, are removed from the search results.
#'
#' @examples
#' # minilog files for the 2020 snow crab survey:
#' locate.minilog(year = 2020)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.minilog("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.minilog(year = 2018, tow.id = "GP354F")
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#' summary(x)   # Data summary.
#' 
#' @seealso \link{locate.probe}, \link{read.minilog}

#' @export
minilog <- function(x, ...) UseMethod("minilog")

#' @describeIn minilog Default \code{minilog} method. Create an \code{minilog} object. 
#' @rawNamespace S3method(minilog,default)
minilog.default <- function(x, ...){
   # Define as probe data object:
   #x <- probe(x, ...)
   
   # Add 'esonar' class tag:
   class(x) <- unique(c("minilog", class(x)))
   
   return(x)
}
