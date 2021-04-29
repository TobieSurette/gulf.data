#' \strong{Netmind} Data
#'
#' @description Create \strong{Netmind} trawl acoustic monitoring probe data objects.
#'
#' @param x An \code{netmind} object, data file, survey year or keyword search term.
#' @param header \strong{Netmind} file header information to be assigned as metadata.
#'
#' @examples
#' # Netmind files for the 2020 snow crab survey:
#' locate.netmind(year = 1999)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.netmind(1999, tow.id = 223)
#'
#' # Working example:
#' locate.netmind(1999, tow.id = 223)
#' 
#' @seealso \link{scanmar}, \link{esonar}, \link{notus}

#' @export
netmind <- function(x, ...) UseMethod("netmind")

#' @describeIn netmind Create an \code{netmind} object.
#' @export
netmind.default <- function(x, ...){
   # Define as probe data object:
   #if ("hour" %in% names(x)) x <- probe(x, ...)
   
   # Define study project:
   gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")

   # Define measurement units:
   #gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   #gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   #gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"

   # Add 'netmind' class tag:
   class(x) <- unique(c("netmind", class(x)))
   
   return(x)
}
