#' Northumberland Strait Survey Set/Tow Data
#'
#' @description The \code{nssset} class is a container for Northumberland Strait survey set/tow data, 
#'              i.e. detailed information about individual tows performed during the annual Northumberland Strait
#'              survey.
#'
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric vector specifying the survey years.
#'
#' @param year Survey year(s) to be loaded.
#'
#' @seealso \code{\link{read.nssset}}

#' @export
nssset <- function(x, ...) UseMethod("nssset")

#' @describeIn nssset Create an \code{nssset} object.
#' @export
nssset.default <- function(x, ...){
   # Re-order variables:
   vars <- c("date", "start.time", "year", "month", "day", "cruise", "tow.number", "station",
             "stratum", "block.number", "valid", "experiment", "distance", "duration", 
             names(x)[grep("longitude", names(x))], names(x)[grep("latitude", names(x))])
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), vars))
   x <- x[vars]
   
   # Define attributes:
   gulf.metadata::project(x) <- "nss"
   gulf.metadata::key(x) <- key.nssset()
   #gulf.metadata::units(x, "speed") <- "knots"
   gulf.metadata::units(x, names(x)[c(grep("depth", names(x)), grep("warp", names(x)))]) <- "meters"
   #gulf.metadata::units(x, names(x)[grep("temperature", names(x))]) <- "degreesC"
   gulf.metadata::fmt(x, names(x)[grep("time", names(x))]) <- "hh:mm:ss"
   
   # Define class:
   class(x) <- unique(c("nssset", class(x)))
   
   return(x)
}

