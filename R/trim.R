#' Trim Time Series Data
#' 
#' @description Removes time series data which lie beyond a specified time interval.
#' 
#' @param x Data object containing time series data.
#' @param range Two-element vector specifying the time range of data beyond which data frame records will be removed. 
#'              \code{NA} or \code{+/-Inf} can be used to specify open bounds.
#' @param start.time,end.time Time bounds beyind which probe data are to be truncated. If left unspecified, 
#'                            start and end times are determined from the project study data, such as set/tow data.

#' @export
trim <- function(x, ...) UseMethod("trim")

#' @export 
trim.NULL <- function(x, ...) return(NULL)

#' @describeIn trim Removes time series data which lie beyond a specified time interval for a \code{data.frame} object.
#' @export 
trim.data.frame <- function(x, range, ...){
   t <- gulf.utils::time(x)
   if (!is.null(t)) x <- x[which((t >= range[1]) & (t <= range[2])), ] else return(x)
}
   
#' @describeIn trim Removes time series data which lie beyond start and end times for a \code{probe} object.
#' @export
trim.probe <- function(x, range, start.time, stop.time, buffer = 0, ...){
   # Define start and end time:
   if (!missing(range)){
      start.time <- range[1]
      stop.time <- range[2]
   }
   
   if (missing(start.time)) start.time <- time(x, "touchdown", ...)
   if (missing(stop.time) ) stop.time  <- time(x, "stop", ...)
   
   # Convert time from character to POSIX format:
   if (is.character(start.time)) start.time <- as.POSIXct(paste(unique(gulf.utils::date(x)), start.time))
   if (is.character(stop.time)) stop.time <- as.POSIXct(paste(unique(gulf.utils::date(x)), stop.time))
      

   # Trim data:
   x <- trim.data.frame(x, range = c(start.time - buffer, stop.time + buffer), ...)
   
   return(x)
}

