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
   t <- time(x)
   if (!is.null(t)) x <- x[which((t >= range[1]) & (t <= range[2])), ] else return(x)
}
   
#' @describeIn trim Removes time series data which lie beyond start and end times for a \code{probe} object.
#' @export
trim.probe <- function(x, start.time, end.time, buffer = 0, ...){
   # Define start and end time:
   if (missing(start.time)) start.time <- gulf.data::start.time(x, ...)
   if (missing(end.time)) stop.time <- gulf.data::stop.time(x, ...)
   x <- trim.data.frame(x, range = c(start.time - buffer, stop.time + buffer), ...)
   
   return(x)
}

