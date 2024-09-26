#' @title Trawl Wing Spread
#' 
#' @description Functions to extract trawl wing spread measurements.
#' 
#' @param x Data object containing trawl wing spread measurements.
#' @param ... Not used.
#' 
#' @examples 
#' x <- read.esonar(2020)
#' wingspread(x)
#' 
#' x <- read.scsset(2024, valid = 1)
#' wingspread(x)

#' @export wingspread
wingspread <- function(x, ...) UseMethod("wingspread")

#' @describeIn wingspread Fetch trawl wing spread measures from an \code{esonar} object.
#' @export
wingspread.esonar <- function(x, ...){
   v <- x$wingspread
   if (is.null(v)) v <- x$doorspread
   if (is.null(v)) v <- x$doormaster
   return(v)
} 

#' @describeIn wingspread Calculate the average wingspread for a snow crab survey tow.
#' @export
wingspread.scsset <- function(x, ...){
   v <- x$swept.area / (1000 * distance(x))
   return(v)
} 

#' @export
wing.spread <- wingspread
