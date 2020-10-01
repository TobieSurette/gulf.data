#' \strong{gulf.data} Index Keys  
#' 
#' @description Define or extract index keys for various \code{gulf.data} objects.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2020)
#' key(x)
#' key.scsset()

#' @describeIn key Fetch \code{scsset} index key.
#' @export key.scsset
#' @export
key.scsset <- function(x, ...) if (missing(x)) return(c("date", "tow.id")) else return(attr(x, "key"))

#' @describeIn key Fetch \code{scsbio} index key.
#' @export key.scsbio
#' @export
key.scsbio <- function(x, ...) if (missing(x)) return(c("date", "tow.id", "crab.number")) else return(attr(x, "key"))

#' @describeIn key Fetch \code{star.oddi} index key.
#' @export key.star.oddi
#' @rawNamespace S3method(key,star.oddi)
key.star.oddi <- function(x, ...) if (missing(x)) return(c("date", "time")) else return(attr(x, "key"))

#' @describeIn key Fetch \code{esonar} index key.
#' @export key.esonar
#' @export
key.esonar <- function(x, ...) if (missing(x)) return(c("date", "time")) else return(attr(x, "key"))
