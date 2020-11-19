#' Northumberland Strait Survey Set/Tow Data
#'
#' @description The \code{nsscat} class is a container for Northumberland Strait survey set/tow data, 
#'              i.e. detailed information about individual tows performed during the annual Northumberland Strait
#'              survey.
#'
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric vector specifying the survey years.
#'
#' @param year Survey year(s) to be loaded.
#'
#' @examples
#' # Create 'nsscat' object from a data frame:
#' x <- nsscat(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, number.caught = 144))
#' key(x)
#' 
#' # Read data:    
#' x <- read.nsscat()                 # Read all available data.
#' x <- read.nsscat(year = 2019)      # Read single year.
#' x <- read.nsscat(year = 2010:2015, species = 10) # Load cod data for range of years.
#' x <- read.nsscat(species = "Cod") # Load cod data using species name search for all years.
#' 
#' # Attach catches to tow data:
#' x <- read.nssset(2019, experiment = 1)           # Read tow data.
#' y <- read.nsscat(2019, species = "plaice")       # Read American plaice data.
#' import(x, var = "number.caught", fill = 0) <- y  # Attach number of specimens caught.
#' 
#' @seealso \code{\link{read.nssset}},  \code{\link{read.nsscat}}

#' @export
nsscat <- function(x, ...) UseMethod("nsscat")

#' @describeIn nsscat Create an \code{nsscat} object.
#' @export
nsscat.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "set.number", "species", "cruise", "valid")
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), vars))
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nsscat()
   
   # Define class:
   class(x) <- unique(c("nsscat", class(x)))
   
   return(x)
}