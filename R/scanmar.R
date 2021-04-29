#' \strong{Scanmar} Data
#'
#' @description Create \strong{Scanmar} trawl acoustic monitoring probe data objects.
#'
#' @param x An \code{scanmar} object, data file, survey year or keyword search term.
#' @param header \strong{Scanmar} file header information to be assigned as metadata.
#'
#' @examples
#' # Scanmar files for the 2020 snow crab survey:
#' locate.scanmar(year = 1990)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.scanmar(1990, tow.id = 223)
#'
#' # Working example:
#' locate.scanmar(1990, tow.id = 223)
#' 
#' @seealso \link{locate.probe}, \link{read.scanmar}

#' @export
scanmar <- function(x, ...) UseMethod("scanmar")

#' @describeIn scanmar Create an \code{scanmar} object.
#' @export
scanmar.default <- function(x, ...){
   # Define as probe data object:
   #if ("hour" %in% names(x)) x <- probe(x, ...)
   
   # Define study project:
   gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")

   # Define measurement units:
   #gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   #gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   #gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"

   # Add 'scanmar' class tag:
   class(x) <- unique(c("scanmar", class(x)))
   
   return(x)
}
