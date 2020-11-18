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
#' @seealso \code{\link{read.nsscat}}

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