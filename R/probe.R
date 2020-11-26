#' Probe Data Class
#' 
#' @description Measurement probe data class definition and methods.
#'
#' @param x Probe data object.
#' @param header File header data.
#' @param buffer Extra time, in seconds, to be included beyond the start and end time specifications when truncating data.
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{probe}}{Generic \code{probe} method.}
#'    \item{\code{probe.default}}{Create a \code{probe} object.}
#' }
#' 
#' @seealso \code{\link{tow.id}}

#' @export
probe <- function(x, ...) UseMethod("probe")

#' @describeIn probe Create a \code{probe} class object.
#' @export
probe.default <- function(x, header, ...){
   # Store date and time stamp:
   v <- data.frame(date = as.character(gulf.utils::date(x)),
                   time = unlist(lapply(strsplit(as.character(gulf.utils::time(x)), " "), function(x) x[2])), 
                   stringsAsFactors = FALSE)
   
   # Add other variables:
   vars <- setdiff(names(x), c("date", "time", "year", "month", "day", "hour", "minute", "second"))
   v[vars] <- x[vars]
   
   # Define index key:
   key(v) <- c("date", "time")
   
   # Add header:
   if (!missing(header)) header(v) <- header
   
   # Assign additional arguments as attributes: 
   args <- list(...)
   if (length(args) > 0) for (i in 1:length(args)) attr(v, names(args)[i]) <- args[[i]]
   
   # Add date and time formats:
   fmt(v, "date") <- "YYYY-MM-DD"
   fmt(v, "time") <- "hh:mm:ss"
   
   class(v) <- unique(c("probe", class(v)))
   
   return(v)
}

