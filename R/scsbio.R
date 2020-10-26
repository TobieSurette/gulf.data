#' Snow Crab Survey Biological Data
#' 
#' @description The \code{scsbio} class is a container for Snow Crab Survey Biological data, i.e. information 
#'              about individual organisms sampled on the snow crab annual survey. 
#'              
#' @param x A \code{data.frame} object. When reading data, \code{x} may be a numeric 
#'          vector specifying the survey years to be loaded or an \code{\link{scsset}} object for which we want to
#'          read the corresponding biological data.
#'                         
#' @param ... Other parameters (not used).
#' 
#' @examples
#' # Create 'scsbio' object with specified 'tow.number' and 'sex' fields:
#' x <- scsbio(data.frame(tow.number = 1:10, sex = 1))
#' 
#' # Read data:    
#' x <- read.scsbio()                 # Read all available data.
#' x <- read.scsbio(year = 2019)      # Read single year.
#' x <- read.scsbio(year = 2010:2015) # Read range of years.
#' 
#' summary(x)

#' @export
scsbio <- function(x, ...) UseMethod("scsbio")

#' @describeIn scsbio Create an \code{scsbio} object.
#' @export
scsbio.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::units(x, c("carapace.width", "chela.height", "abdomen.width")) <- "millimeters"
   gulf.metadata::units(x, "weight") <- "grams"
   gulf.metadata::key(x) <- c("date", "tow.id", "crab.number")
   
   # Define class:
   class(x) <- unique(c("scsbio", class(x))) 
   
   return(x)
}
