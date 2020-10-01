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
#' plot(x)      # Graphical summary.
#' summary(x)   # Data summary.

#' @export
esonar <- function(x, ...) UseMethod("esonar")

#' @describeIn esonar Create an \code{esonar} object.
#' @export
esonar.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Define study project:
   gulf.metadata::project(x) <- gulf.metadata::project("snow crab survey")
   
   # Define measurement units:
   gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"

   # Add 'esonar' class tag:
   class(x) <- unique(c("esonar", class(x)))
   
   return(x)
}

#' @describeIn esonar Describe an \code{esonar} object.
#' @export
describe.esonar <- function(x, ...){
   if (is.null(header(x))) return(NULL)

   v <- list()
   v$ship.number <- header(x)$ShipNumber
   v$trip.number <- as.numeric(header(x)$TripNumber)
   v$tow.number  <- as.numeric(header(x)$TowNumber)
   v$comments    <- header(x)$Comments
   v$duration    <- as.numeric(max(time(x)) - min(time(x)))

   # Parse file name and path:
   str <- strsplit(header(x)$file, "/", fixed = TRUE)[[1]]
   v$file.name <- str[length(str)]
   if (length(str) == 1) v$path <- "" else v$path <- paste(str[1:(length(str)-1)], collapse = "/")

   str <- strsplit(tolower(v$file), ".", fixed = TRUE)[[1]]
   str <- toupper(str[1])
   v$tow.id <- str
   v$rows <- dim(x)[1]
   v$columns <- dim(x)[2]
   sensors <- unique(x$sensor)
   sensors <- sensors[sensors != ""]
   v$sensors <- paste("(", paste(sensors, collapse = ", "), ")", sep = "")
   t <- table(diff(time(x)))
   t <- t[t == max(t)]
   v$sampling.frequency <- as.numeric(names(t))

   return(v)
}

