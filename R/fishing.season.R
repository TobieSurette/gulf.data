#' @title Fishing Season Dates
#' 
#' @description Functions to retrieve fishing season date information.
#'
#' @param x Character search string.
#' @param ... Other arguments (not used).
#' 
#' @examples 
#' fishing.season()                       # Complete fishing season table.
#' fishing.season(year = 2019, zone = 12) # Search for zone 12 season limits in 2019.

#' @export
fishing.season <- function(x, ...) UseMethod("fishing.season")

#' @describeIn fishing.season Default \code{fishing.season} method. Returns the complete fishing.season data table.
#' @export
fishing.season.default <- function(x, species = "snow crab", year, zone, ...){
   # Load data table:
   file <- locate(package = "gulf.data", file = "fishing.season.csv")
   v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   
   # Subset data:
   if (!missing(year)) v <- v[gulf.utils::year(gulf.utils::date(v$start.date)) %in% year, ]
   if (!missing(zone)) v <- v[v$zone %in% zone, ]
   v <- v[v$species %in% species, ]
   
   return(v)
}
