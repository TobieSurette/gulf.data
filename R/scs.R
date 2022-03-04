#' Snow Crab Survey Data Classes
#'
#' @description Class containers for snow crab survey data.
#'              
#' @param x A \code{data.frame} object containing data fields to be mapped onto a corresponding snow crab survey data object.
#'                         
#' @param ... Other parameters (not used).
#' 
#' @examples
#' # Create 'scscat' object from a data frame:
#' x <- scscat(data.frame(date = "2020-07-19", tow.id = "GP001F", tow.number = 1, species = 10, number.caught = 144))
#' key(x)
#' 
#' # Create 'scsbio' object with specified 'tow.number' and 'sex' fields:
#' x <- scsbio(data.frame(date = "2020-07-19",
#'                        tow.id = "GP001F",
#'                        crab.number = 1:10,
#'                        carapace.width = rnorm(10, 100, 5), 
#'                        chela.height = NA,
#'                        sex = 1))
#' 
#' @seealso \code{\link{read.scs}}
             
#' @describeIn scs Generic \code{scsset} method.
#' @export scsset
scsset <- function(x, ...) UseMethod("scsset")

#' @describeIn scs Generic \code{scscat} method.
#' @export
scscat <- function(x, ...) UseMethod("scscat")

#' @describeIn scs Generic \code{scsbio} method.
#' @export
scsbio <- function(x, ...) UseMethod("scsbio")

#' @describeIn scs Generic \code{scslen} method.
#' @export
scslen <- function(x, ...) UseMethod("scslen")

#' @describeIn scs Create an \code{scsset} object.
#' @export
scsset.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "year", "month", "day", "tow.id", "tow.number", "valid", "station.type", "zone",
             names(x)[grep("time", names(x))],
             names(x)[grep("longitude", names(x))], names(x)[grep("latitude", names(x))])
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), vars))
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::key(x) <- key.scsset()
   if ("swept.area" %in% names(x)) gulf.metadata::units(x, "swept.area") <- "square.meters"
   if ("depth" %in% names(x)) gulf.metadata::units(x, "depth") <- "fathoms"
   if ("warp" %in% names(x)) gulf.metadata::units(x, "warp") <- "fathoms"
   if ("bottom.temperature" %in% names(x)) gulf.metadata::units(x, "bottom.temperature") <- "degreesC"
   gulf.metadata::fmt(x, names(x)[grep("time", names(x))]) <- "hh:mm:ss"
   
   # Define class:
   class(x) <- unique(c("scsset", class(x)))

   return(x)
}

#' @describeIn scs Create an \code{scscat} object.
#' @export
scscat.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   x$tow.id <- tow.id.scscat(x)
   gulf.metadata::key(x) <- key.scscat()
   
   # Define class:
   class(x) <- unique(c("scscat", class(x))) 
   
   return(x)
}

#' @describeIn scs Create an \code{scsbio} object.
#' @export
scsbio.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   x$tow.id <- tow.id.scsbio(x)
   gulf.metadata::key(x) <- key.scsbio()
   if ("carapace.width" %in% names(x)) gulf.metadata::units(x, "carapace.width") <- "millimeters"
   if ("chela.height" %in% names(x)) gulf.metadata::units(x,  "chela.height") <- "millimeters"
   if ("abdomen.width" %in% names(x)) gulf.metadata::units(x, "abdomen.width") <- "millimeters"
   if ("weight" %in% names(x)) gulf.metadata::units(x, "weight") <- "grams"

   if ("tow.number" %in% names(x))  gulf.metadata::key(x) <- c("date", "tow.number", "crab.number")
   if ("tow.id" %in% names(x))      gulf.metadata::key(x) <- c("date", "tow.id", "crab.number")
   
   # Define class:
   class(x) <- unique(c("scsbio", class(x))) 
   
   return(x)
}

#' @describeIn scs Create an \code{scslen} object.
#' @export
scslen.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   x$tow.id <- tow.id.scslen(x)
   gulf.metadata::key(x) <- key.scslen()
   
   # Define class:
   class(x) <- unique(c("scslen", class(x))) 
   
   return(x)
}
