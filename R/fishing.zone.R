#' @title Determine Fishing Zone
#'
#' @description Returns a fishing zone given a set of of coordinates.
#'
#' @param longitude Numerical vector of longitudes in decimal degree format.
#' @param latitude Numerical vector of latitudes in decimal degree format.
#' @param region Character value specifying the geographic region. This argument is passed onto the \code{\link[gulf]{fishing.zone.info}} function.
#' @param species Numerical scalar containing a species code.
#' @param method Character value specifying the method to be used to determine the fishing zone.
#'

#' @export fishing.zone
fishing.zone <- function(x, ...) UseMethod("fishing.zone")

#' @export zone
zone <- fishing.zone

#' @rawNamespace S3method(fishing.zone,logbook)
fishing.zone.logbook <- function(x, method = "observed", ...){
   # Parse method argument:
   method <- match.arg(tolower(method), c("observed", "allocation", "gps", "latlong"))
   if (method == "latlong") method <- "gps"

   # Initialize result variable:
   r <- rep("", nrow(x))

   # Coordinate method:
   if (method == "gps") r <- gulf.spatial::fishing.zone(x$longitude, x$latitude, species = 2526)

   # Observed values:
   if (method == "observed") r <- x$zone

   # Allocation code:
   if (method == "allocation"){
      v <- allocation(x$allocation.code, language = "english")

      # Zone 12:

      ix <- unique(c(grep("Zone 12 ", v), grep("Zone 12$", v), grep("Traditional", v)))
      r[ix] <- "12"

      # Zone 12E:
      ix <- unique(c(grep("Zone 12E", v)))
      r[ix] <- "12E"

      # Zone 12F:
      ix <- unique(c(grep("Zone 12F", v)))
      r[ix] <- "12F"

      # Area 19:
      ix <- unique(c(grep("Area 19", v)))
      r[ix] <- "19"
   }


   return(r)
}

