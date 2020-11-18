#' Snow Crab Survey By-Catch Data
#' 
#' @description The \code{scscat} class is a container for Snow Crab Survey by-catch data, i.e. information 
#'              about species other than snow crab caught on snow crab annual surveys. 
#'              
#' @param x A \code{data.frame} object. When reading data, \code{x} may be a numeric vector specifying the 
#'          survey years to be loaded or an \code{\link{scsset}} object for which we want to read the 
#'          corresponding biological data.
#'                         
#' @param ... Other parameters (not used).
#' 
#' @examples
#' # Create 'scsbio' object with specified 'tow.number' and 'sex' fields:
#' x <- scscat(data.frame(tow.number = 1, species = 10, number.caught = 144))
#' 
#' # Read data:    
#' x <- read.scscat()                 # Read all available data.
#' x <- read.scscat(year = 2019)      # Read single year.
#' x <- read.scscat(year = 2010:2015) # Read range of years.
#' 
#' summary(x)

#' @export
scscat <- function(x, ...) UseMethod("scscat")

#' @describeIn scscat Create an \code{scscat} object.
#' @export
scscat.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::key(x) <- key.scscat()
   
   # Define class:
   class(x) <- unique(c("scscat", class(x))) 
   
   return(x)
}
