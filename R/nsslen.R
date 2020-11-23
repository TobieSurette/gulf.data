#' Northumberland Strait Survey Set/Tow Data
#'
#' @description The \code{nsslen} class is a container for Northumberland Strait survey length data.
#'
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric vector specifying the survey years.
#'
#' @param year Survey year(s) to be loaded.
#'
#' @examples
#' # Create 'nsslen' object from a data frame:
#' x <- nsslen(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, number.caught = 144))
#' key(x)
#' 
#' # Read data:    
#' x <- read.nsslen()                 # Read all available data.
#' x <- read.nsslen(year = 2019)      # Read single year.
#' x <- read.nsslen(year = 2010:2015, species = 10) # Load cod data for range of years.
#' x <- read.nsslen(species = "Cod") # Load cod data using species name search for all years.
#' 
#' @seealso \code{\link{read.nssset}}, \code{\link{read.nsscat}}

#' @export
nsslen <- function(x, ...) UseMethod("nsslen")

#' @describeIn nsslen Create an \code{nsslen} object.
#' @export
nsslen.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "set.number", "experiment", "species", "size.class", "sex", "ratio", "length", "length.unit", "comment")
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), vars))
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nsslen()
   
   # Define class:
   class(x) <- unique(c("nsslen", class(x)))
   
   return(x)
}
