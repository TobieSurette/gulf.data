#' Snow Crab Survey Set/Tow Data
#'
#' @description The \code{scsset} class is a container for Snow Crab Survey Set/Tow data, i.e.
#'              information about individual tows performed on the snow crab annual survey.
#'
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric
#'          vector specifying the survey years.
#'
#' @param year Survey year(s) to be loaded.
#'
#' @seealso \code{\link[gulf.data]{read.scsset}}

#' @seealso \code{\link{scsbio}}, \code{\link{tow.id}}

#' @export scsset
scsset <- function(x, ...) UseMethod("scsset")

#' @describeIn scsset Create an \code{scsset} object.
#' @export
scsset.default <- function(x, ...){
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::key(x) <- key.scsset()
   gulf.metadata::units(x, "swept.area") <- "square.meters"
   gulf.metadata::units(x, c("depth", "warp")) <- "fathoms"
   gulf.metadata::units(x, "bottom.temperature") <- "degreesC"
   gulf.metadata::fmt(x, names(x)[grep("time", names(x))]) <- "hh:mm:ss"

   # Reorder variables:
   vars <- c("date", "year", "month", "day", "tow.id", "tow.number", "valid", "zone",
             names(x)[grep("time", names(x))],
             names(x)[grep("longitude", names(x))], names(x)[grep("latitude", names(x))])
   vars <- vars[vars %in% names(x)]
   vars <- c(vars, setdiff(names(x), vars))
   x <- x[vars]
   
   # Define class:
   class(x) <- unique(c("scsset", class(x)))

   return(x)
}

