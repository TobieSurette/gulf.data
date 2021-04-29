#' \strong{eSonar} Data
#'
#' @description Create \strong{eSonar} trawl acoustic monitoring probe data objects.
#'
#' @param x An \code{esonar} object, data file, survey year or keyword search term.
#' @param header \strong{eSonar} file header information to be assigned as metadata.
#'
#' @examples
#' # eSonar files for the 2020 snow crab survey:
#' locate.esonar(year = 2020)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.esonar("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.esonar("GP001", year = 2020)
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#'
#' @seealso \link{scanmar}, \link{netmind}, \link{notus}

#' @export
esonar <- function(x, ...) UseMethod("esonar")

#' @describeIn esonar Create an \code{esonar} object.
#' @export
esonar.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Define study project:
   #gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")
   
   # Define measurement units:
   #gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   #gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   #gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"

   # Add 'esonar' class tag:
   class(x) <- unique(c("esonar", class(x)))
   
   return(x)
}
