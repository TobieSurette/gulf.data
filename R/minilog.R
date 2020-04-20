#' Minilog Data
#' 
#' @description Minilog probes are used to record depth and temperature data. 
#' These are functions to create \code{minilog} objects.
#' 
#' @seealso read.minilog
#' 
#' @export minilog
#' @export minilog.default
#' @export minilog.data.frame

minilog <- function(x, ...) UseMethod("minilog")

#' @describeIn minilog Default \code{minilog} object.
minilog.default <- function(x, ...) if ("minilog" %in% class(x)) return(x) else stop("Not a 'minilog' object.")

#' @describeIn minilog Converts a data frame to a \code{minilog} object.
minilog.data.frame <- function(x, header, ...){
   # Remove non-minilog variables:
   names(x) <- tolower(names(x))
   vars <- c("date", "time", "temperature", "depth")
   x <- x[, intersect(vars, names(x))]

   # Check that date and time fields is defined:
   if (c("date", "time") %in% names(x)) stop("'date' and 'time' fields must be defined.")
   
   # Format 'header':
   if (missing(header)) header <- list()
   if (!is.list(header)) stop("'header' must be a list.")
   if (is.null(header$ID))            header$ID            <- "Minilog-TD"
   if (is.null(header$Serial.Number)) header$Serial.Number <- ""
   if (is.null(header$Start.Time))    header$Start.Time    <- x$time[1]
   if (is.null(header$Finish.Time))   header$Finish.Time   <- x$time[nrow(x)]
   period <- min(max(c(0, unique(as.numeric(diff(time(x)))))))
   if (is.null(header$Sample.Period)) header$Sample.Period <- paste0("00:00:0", period)
   
   # Strip non-data-frame attributes:
   attributes(x) <- attributes(x)[c("names", "row.names", "class")]

   # Assign header:
   attributes(x) <- c(attributes(x), header)

   # Minilog class identifier:
   class(x) <- union("minilog", class(x))

   return(x)
}
