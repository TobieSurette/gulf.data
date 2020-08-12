#' Minilog Data
#'
#' @description Functions to create, read, write, manipulate and analyze Minilog depth/temperature 
#'              probe data.
#' 
#' @param x Minilog data file
#' @param year Numeric value specifying the survey or project year(s).
#' @param survey,project Character string specifying the survey or project name. 
#'    The survey may refer to the September multi-species survey (\code{survey = "rv"},
#'    \code{survey = "sep"} or \code{survey = "September"}), the  Northumberland Strait 
#'    survey (\code{= "ns"}), the mobile Sentinel survey  \code{= "sentinel"}),
#'    or the snow crab survey (\code{= "sc"} or \code{= "snow crab"}). 
#' @param set.number,tow.id Numeric value or strings specifying the survey set number or tow ID.
#' @param set.card Data frame containing the survey tows which specify the Minilog data to be loaded.
#' @param path Logical value specifying whether to include the path in the Minilog files.
#' 
#' @examples 
#' # Minilog files for the 2000 September RV survey:
#' minilog.file(survey = "rv", year = 2000)
#'
#' # Use a set card extract for the first ten sets of 2012:
#' x <- read.gulf(year = 2012, survey = "sc")
#' minilog.file(x[1:10, ])
#' 
#' # Use a tow ID to extract file names for the snow crab survey 2006-2012:
#' minilog.file("GP001", year = 2006:2012)
#' 
#' x <- read.minilog(tow.id = "GP001F", year = 2010)
#' x <- read.minilog(survey = "rv", year = 2010)
#' 
#' # Plot Minilog data:
#' file <- system.file("extdata", "Minilog example.txt", package = "gulf.data")
#' x <- read.minilog(file)
#' plot(x)
#' 
#' @export minilog
#' 
#' @seealso \code{\link[gulf.data]{read.minilog}}
#' @seealso \code{\link[gulf.data]{header}}
#' 

minilog <- function(x, ...) UseMethod("minilog")

#' @describeIn minilog Default minilog object.
minilog.default <- function(x, ...) if ("minilog" %in% class(x)) return(x) else stop("Not a 'minilog' object.")

#' @describeIn minilog Convert data frame to minilog object.
minilog.data.frame <- function(x, header, ...){
   # Remove non-minilog variables:
   names(x) <- tolower(names(x))
   vars <- c("date", "time", "temperature", "depth")
   x <- x[, intersect(vars, names(x))]

   # Check that date and time fields is defined:
   if (!all(c("date", "time") %in% names(x))) stop("'date' and 'time' fields must be defined.")
   
   # Format 'header':
   if (!missing(header)){
      if (!is.list(header)) stop("'header' must be a list.")
      attributes(x) <- c(attributes(x), list(header = header))
   }
   
   # Index key:
   key(x) <- c("date", "time")
   
   # Minilog class identifier:
   class(x) <- union("minilog", class(x))

   # Attach metadata attributes:
   if ("temperature" %in% names(x)) units(x, "temperature") <- "degCelsius"
   if ("depth" %in% names(x)) units(x, "depth") <- "meter"
   
   return(x)
}

#' @describeIn minilog Plot minilog data object.
plot.minilog <- function(x, col, lwd = 1, ...){
   # Define default colours:
   if (missing(col)){
      cols <- "black"
      if (all(c("temperature", "depth") %in% names(x))) cols <- c("blue", "chartreuse3")
   }
   
   # Plot temperature:
   ylim <- c(0, 1.15 * max(x$temperature))
   plot(time(x), x$temperature, xaxs = "i", yaxs = "i",
        ylim = ylim, lwd = lwd, col = cols[1], yaxt = "n", xlab = "", ylab = "", type = "l", ...)
   
   axis(2, col = cols[1], col.axis = cols[1])
   mtext("Time", 1, 2, col = cols[1])
   mtext("Temperature(C)", 2, 2, col = cols[1])
      
   # Plot depth:
   if ("depth" %in% names(x)){
      r <- range(x$depth)
      r <- c(r[1] - 0.25, r[2] + 0.4)
      par(usr = c(par("usr")[1:2], r))
      lines(time(x), x$depth, lwd = lwd, col = cols[2], ...)
      axis(4, col = cols[2], col.axis = cols[2])
      mtext("Depth", 4, 2, col = cols[2])
   }
}
