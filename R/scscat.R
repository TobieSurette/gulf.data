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
#' # Create 'scscat' object from a data frame:
#' x <- scscat(data.frame(date = "2020-07-19", tow.id = "GP001F", tow.number = 1, species = 10, number.caught = 144))
#' key(x)
#' 
#' # Read data:    
#' x <- read.scscat()                 # Read all available data.
#' x <- read.scscat(year = 2019)      # Read single year.
#' x <- read.scscat(year = 2010:2015, species = 10) # Load cod data for range of years.
#' x <- read.scscat(species = "Cod") # Load cod data using species name search for all years.
#' 
#' # Attach catches to tow data:
#' x <- read.scsset(2020, valid = 1, survey = "regular") # Read tow data.
#' y <- read.scscat(2020, species = "plaice")            # Read American plaice data.
#' import(x, var = "number.caught", fill = 0) <- y       # Attach number of specimens caught.
#' 
#' @seealso \code{\link{read.scsset}},  \code{\link{read.scscat}}
#' 
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
