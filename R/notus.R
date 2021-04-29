#' \strong{Notus} Data
#'
#' @description Create \strong{Notus} trawl acoustic monitoring probe data objects.
#'
#' @param x An \code{notus} object, data file, survey year or keyword search term.
#' @param header \strong{Notus} file header information to be assigned as metadata.
#'
#' @examples
#' # Notus files for the 2020 snow crab survey:
#' locate.notus(year = 2020, project == "nss")
#'
#' @seealso \link{scanmar}, \link{netmind}, \link{esonar}

#' @export
notus <- function(x, ...) UseMethod("notus")

#' @describeIn notus Create an \code{notus} object.
#' @export
notus.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Define study project:
   #gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")
   
   # Define measurement units:
   #gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   #gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   #gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"
   
   # Add 'notus' class tag:
   class(x) <- unique(c("notus", class(x)))
   
   return(x)
}