#' Clean or Standardize Data
#' 
#' @description Function to remove redundant elements from a data object.
#' 
#' @param x Data object.
#' 

#' @export
clean <- function(x, ...) UseMethod("clean")

#' @describeIn clean Standardize data for \code{scsbio} object. 
#' @export
clean.scsbio <- function(x, ...){
   ix <- unlist(lapply(x, function(x) return(all(is.na(x)))))
   x <- x[, !ix]
   return(x)
}
