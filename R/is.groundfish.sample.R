#' Groundfish Sampling?
#' 
#' @description Returns whether a survey tow was length-sampled for groundfish.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2010:2020)
#' is.groundfish.sample(x)

#' @export is.groundfish.sample
is.groundfish.sample <- function(x, ...){
   UseMethod("is.groundfish.sample")
}
  
#' @rawNamespace S3method(is.groundfish.sample,scsset)
is.groundfish.sample.scsset <- function(x, ...){
   years <- sort(unique(year(x)))
   y <- read.scslen(years)
   y <- unique(y[key(x)])
   v <- rep(FALSE, nrow(x))
   ix <- gulf.utils::match(y[key(x)], x[key(x)])
   v[ix] <- TRUE
   
   return(v)
}
