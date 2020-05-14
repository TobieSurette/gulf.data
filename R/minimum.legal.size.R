#' Date Extraction and Conversion
#'
#' @description These are wrapper functions to extract a date field from an object, such as a character
#' string or a data frame.
#'
#' @param x Object.
#' @param year Date year.
#' @param month Date month.
#' @param day Date day.
#'
#' @return Generally a \code{POSIXct} or \code{POSIXt} object.
#'
#' @examples
#' date() # Base method.
#'
#' # Single day:
#' date(year = 2000, month = 9, day = 19)
#'
#' # Entire month:
#' date(year = 2000, month = 9, day = 1:30)
#'
#' # Sorts out YYYYMMDD and DDMMYYYY:
#' date(c("2000-01-19", "19-01-2000", "2000/01/19"))
#'
#' Apply to data frame:
#' x <- data.frame(Year = 2000, Month = 01, Day = 1:30)
#' date(x)
#'
#' @export date
#' @export date.default
#' @export date.character
#' @export date.data.frame
#'
minimum.legal.size <- function(x, ...) UseMethod("minimum.legal.size")

#' @describeIn minimum.legal.size Default minimum legal size function.
minimum.legal.size.default <- function(x, species, year, fishing.zone, sub.fishing.zone, ...){
   if (missing(x)){
      x <- read.csv(system.file("extdata", "minimum.legal.sizes.csv", package = "gulf.data"), header = TRUE, stringsAsFactors = FALSE)
      return(x)
   }
   
   if (missing(species)) stop("'species' must be specified.")
   if ((species == 2550) & missing(year)) stop("'year' must be specified for lobster.")
   
   if (all(is.na(x))) return(x)
}

mls <- minimum.legal.size

