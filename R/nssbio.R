#' Northumberland Strait Survey Biological Data
#'
#' @description The \code{nssbio} class is a container for Northumberland Strait survey biological data.
#'
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric vector specifying the survey years.
#'
#' @param year Survey year(s) to be loaded.
#'
#' @examples
#' # Create 'nssbio' object from a data frame:
#' x <- nssbio(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, specimen = 1, length = 10))
#' key(x)
#' 
#' # Read data:    
#' x <- read.nssbio()                 # Read all available data.
#' x <- read.nssbio(year = 2019)      # Read single year.
#' x <- read.nssbio(year = 2010:2015, species = 10) # Load cod data for range of years.
#' x <- read.nssbio(species = "Cod") # Load cod data using species name search for all years.
#' 
#' @seealso \code{\link{read.nssset}}, \code{\link{read.nsscat}}, \code{\link{read.nsslen}}

#' @export
nssbio <- function(x, ...) UseMethod("nssbio")

#' @describeIn nssbio Create an \code{nssbio} object.
#' @export
nssbio.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "cruise", "set.number",  "species", "specimen", "sex", "length", "weight")
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), c(vars, "comment")), "comment")
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nssbio()
   
   # Define class:
   class(x) <- unique(c("nssbio", class(x)))
   
   return(x)
}