#' Trim Time Series Data
#' 
#' @description Removes time series data which lie beyond a specified time interval.
#' 
#' @param x Data object containing time series data.
#' 

#' @export
trim <- function(x, ...) UseMethod("trim")

#' @describeIn trim Removes time series data which lie beyond a specified time interval for a \code{data.frame} object.
#' @export 
trim.data.frame <- function(x, range, ...){
   t <- time(x)
   if (!is.null(t)){
      x <- x[which((t >= range[1]) & (t <= range[2])), ]
   }else{
      return(x)
   }
}
   
#' @describeIn trim Removes time series data which lie beyond start and end times for a \code{probe} object.
#' @export
trim.probe <- function(x, start.time, end.time, buffer = 0, ...){
   # Define start and end time:
   if (missing(start.time)) start.time <- start.time(x, ...)
   if (missing(end.time)) end.time <- end.time(x, ...)   
   
   x <- trim.data.frame(x, range = c(start.time - buffer, end.time + buffer), ...)
   
   return(x)
}

