#' Northumberland Strait Survey Data Classes
#'
#' @description Class containers for snow crab survey data.
#'              
#' @param x A \code{data.frame} object containing data fields to be mapped onto a corresponding snow crab survey data object.
#'                         
#' @param ... Other parameters (not used).
#'
#' @examples
#' # Create 'nsscat' object from a data frame:
#' x <- nsscat(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, number.caught = 144))
#' key(x)
#' 
#' # Create 'nssbio' object from a data frame:
#' x <- nssbio(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, specimen = 1, length = 10))
#' key(x)
#' 
#' # Create 'nsslen' object from a data frame:
#' x <- nsslen(data.frame(date = "2020-07-19", cruise = "P555", set.number = 1, species = 10, size.class = 1, sex = 1, length = 10))
#' key(x)
#' 
#' @seealso \code{\link{read.nssset}},  \code{\link{read.nsscat}}

#' @describeIn nss Generic \code{nssset} method.
#' @export
nssset <- function(x, ...) UseMethod("nssset")

#' @export
nsscat <- function(x, ...) UseMethod("nsscat")

#' @export
nssbio <- function(x, ...) UseMethod("nssbio")

#' @export
nsslen <- function(x, ...) UseMethod("nsslen")

#' @describeIn nss Create an \code{nssset} object.
#' @export
nssset.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "start.time", "year", "month", "day", "cruise", "tow.number", "station",
             "stratum", "block.number", "valid", "experiment", "distance", "duration", 
             names(x)[grep("longitude", names(x))], names(x)[grep("latitude", names(x))])
   vars <- c(vars, setdiff(names(x), c(vars, "comment")), "comment")
   vars <- vars[vars %in% names(x)]
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nssset()
   if ("speed" %in% names(x)) gulf.metadata::units(x, "speed") <- "knots"
   if ("depth" %in% names(x)) gulf.metadata::units(x, "depth") <- "meters"
   if (length(grep("warp", names(x))) > 0) gulf.metadata::units(x, names(x)[grep("warp", names(x))]) <- "meters"
   if (length(grep("temperature", names(x))) > 0) gulf.metadata::units(x, names(x)[grep("temperature", names(x))]) <- "degreesC"
   if (length(grep("time", names(x))) > 0) gulf.metadata::fmt(x, names(x)[grep("time", names(x))]) <- "hh:mm:ss"
   
   # Define class:
   class(x) <- unique(c("nssset", class(x)))
   
   return(x)
}

#' @describeIn nss Create an \code{nsscat} object.
#' @export
nsscat.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "set.number", "species", "cruise", "valid")
   vars <- c(vars, setdiff(names(x), c(vars, "comment")), "comment")
   vars <- vars[vars %in% names(x)]
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nsscat()
   
   # Define class:
   class(x) <- unique(c("nsscat", class(x)))
   
   return(x)
}

#' @describeIn nss Create an \code{nssbio} object.
#' @export
nssbio.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "cruise", "set.number",  "species", "specimen", "sex", "length", "weight")
   vars <- c(vars, setdiff(names(x), c(vars, "comment")), "comment")
   vars <- vars[vars %in% names(x)]
   
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nssbio()
   
   # Define class:
   class(x) <- unique(c("nssbio", class(x)))
   
   return(x)
}

#' @describeIn nss Create an \code{nsslen} object.
#' @export
nsslen.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "set.number", "experiment", "species", "size.class", "sex", "ratio", "length", "length.unit")
   vars <- c(vars, setdiff(names(x), c(vars, "comment")), "comment")
   vars <- vars[vars %in% names(x)]
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nsslen()
   
   # Define class:
   class(x) <- unique(c("nsslen", class(x)))
   
   return(x)
}
