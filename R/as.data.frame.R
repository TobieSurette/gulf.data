#' Convert to Data Frame
#' 
#' @description Convert \strong{gulf} data objects to data frames.
#' 
#' @param x Data object.
#' 

#' @describeIn as.data.frame Convert \code{scsset} snow crab survey biological set data object to a data frame.
#' @rawNamespace S3method(as.data.frame,scsset)
as.data.frame.scsset <- function(x, ...){
   class(x) <- "data.frame"
   return(x)
}

#' @describeIn as.data.frame Convert \code{scsbio} snow crab survey biological data object to a data frame.
#' @rawNamespace S3method(as.data.frame,scsbio)
as.data.frame.scsbio <- function(x, ...){
   class(x) <- "data.frame"
   return(x)
}

#' @describeIn as.data.frame Convert \code{probe} data object to a data frame.
#' @rawNamespace S3method(as.data.frame,probe)
as.data.frame.probe <- function(x, ...){
   class(x) <- "data.frame"
   return(x)
}
