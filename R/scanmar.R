#' \strong{Scanmar} Data
#'
#' @description Create \strong{scanmar} trawl acoustic monitoring probe data objects.
#'
#' @param x An \code{scanmar} object, data file, survey year or keyword search term.
#' @param header \strong{Scanmar} file header information to be assigned as metadata.
#'
#' @examples
#' locate.scanmar()
#' 
#' # Scanmar files for the 1990 snow crab survey:
#' locate.scanmar(year = 1990)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.scanmar("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.scanmar("GP001", year = 1990)
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#' plot(x)      # Graphical summary.
#' summary(x)   # Data summary.

#' @export
scanmar <- function(x, ...) UseMethod("scanmar")

#' @describeIn scanmar Create an \code{scanmar} object.
#' @export
scanmar.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Define study project:
   gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")
   
   # Define measurement units:
   gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"
   
   # Add 'scanmar' class tag:
   class(x) <- unique(c("scanmar", class(x)))
   
   return(x)
}
