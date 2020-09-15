#' Probe Data Class
#' 
#' @name probe
#' 
#' @description Measurement probe data class definition and methods.
#' 
#' @param x Probe data object.
#' @param header File header data.
#' @param start.time,end.time Time bounds beyind which probe data are to be truncated. If left unspecified, 
#'                            start and end times are determined from the project study data, such as set/tow data.
#' @param buffer Extra time, in seconds, to be included beyond the start and end time specifications when truncating data.
#' @param method Character string specifying the method to be used when determining the tow identification number for a 
#'               \code{probe} object. Available methods are \sQuote{time} and \sQuote{latlong}.
#' @param max.distance Numeric valiue specifying the maximum distance tolerance (in kilometers) when determining the tow 
#'                     identification number for a \code{probe} object from lat-lon coordinates.         
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{probe}}{Generic \code{probe} method.}
#'    \item{\code{probe.default}}{Create a \code{probe} object.}
#'    \item{\code{start.time.probe}}{Find start time for a \code{probe} data object.}
#'    \item{\code{end.time.probe}}{Find end time for a \code{probe} data object.}
#'    \item{\code{truncate.probe}}{Truncate \code{probe} data object to lie within start and end time limits.}
#'    \item{\code{tow.id.probe}}{Find tow ID number for a \code{probe} data object.}
#'    \item{\code{plot.probe}}{Graphically display a \code{probe} data object.}
#' }


#' @rdname probe
#' @export
probe <- function(x, ...) UseMethod("probe")

#' @rdname probe
#' @export
probe.default <- function(x, header, ....){
   # Store date and time stamp:
   v <- data.frame(date = as.character(gulf.utils::date(x)),
                   time = gulf.utils::time(x),
                   stringsAsFactors = FALSE)
   
   # Identify and store observations:
   remove <- c("date", "time", "year", "month", "day", "hour", "minute", "second")
   vars <- setdiff(names(x), remove)
   v[vars] <- x[vars]
   
   # Define index key:
   key(v) <- c("date", "time")
   
   # Add header:
   if (!missing(header)) header(x) <- header
   
   # Assign additional arguments as attributes: 
   args <- list(...)
   if (length(args) > 0) for (i in 1:length(args)) attr(x, names(args)[i]) <- args[[i]]
   
   class(v) <- unique(c("probe", class(v)))
}

#' @rdname probe
#' @export
start.time.probe <- function(x, ...){
   if (project(x) == "scs"){
      if ("year" %in% names(x)) year <- x$year else year <- attr(x, "year")
      if ("tow.id" %in% names(x)) tow.id <- x$tow.id else year <- attr(x, "tow.id")
      y <- data.frame(year = year, tow.id = tow.id, stringsAsFactors = FALSE)
      z <- read.scsset(year = unique(year))
      r <- start.time(z)[match(y[key(z)], unique(z[key(z)]))]
   }
 
   return(r)  
}

#' @rdname probe
#' @export
end.time.probe <- function(x, ...){
   if (project(x) == "scs"){
      if ("year" %in% names(x)) year <- x$year else year <- attr(x, "year")
      if ("tow.id" %in% names(x)) tow.id <- x$tow.id else year <- attr(x, "tow.id")
      y <- data.frame(year = year, tow.id = tow.id, stringsAsFactors = FALSE)
      z <- read.scsset(year = unique(year))
      r <- end.time(z)[match(y[key(z)], unique(z[key(z)]))]
   }
 
   return(r)  
}

#' @rdname probe
#' @export
truncate.probe <- function(x, start.time, end.time, buffer = 0, ...){
   # Define start and end time:
   if (missing(start.time)) start.time <- start.time(x, ...)
   if (missing(end.time)) end.time <- end.time(x, ...)   
 
   t <- time(x)
   index <- (t >= (start.time - buffer)) & (t >= (end.time + buffer))
   x <- x[index, ]
   
   return(x)
}

#' @rdname probe
#' @export
tow.id.probe <- function(x, method, max.distance = 500, ...){
   # Parse 'method' argument:
   if (!missing(method)) method <- match.arg(tolower(method), c("time", "latlong"))

   # Determine project study year:                                          
   year <- unique(x$year)
   if (is.null(year)) year <- attr(x, "year")
   if (is.null(year)) stop("Unable to determine study year.")
   if (length(year) > 1) stop("Multiple years in probe dataset.")
   
   # Load project sampling data:
   if (project(x) == "scs") y <- read.scsset(year)
   
   # Match using time stamps:
   if (method == "time") v <- y$tow.id[which.min(abs(mean(time(x)) - time(y)))]
   
   # Match using mean coordinate values:
   if (method == "latlong"){
      lat <- latitude(x)
      lon <- longitude(x)
      if (!is.null(lat) & !is.null(lon)){
         d <- distance(mean(lon, na.rm = TRUE), mean(lat, na.rm = TRUE), longitude(y), latitude(y))[1,]
         v <- y$tow.id[which.min(d) < max.distance]
      }
   }
   
   return(v)
}
   
#'@rdname probe
#'@export
plot.probe <- function(x, ...){
   vars <- setdiff(names(x), c("data", "time"))
   m <- kronecker(1:length(n), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0, cbind(0, m, 0), 0, 0)
   layout(m)
   par(mar = c(0,0,0,0))
   t <- time2min(time(x))
   for (i in 1:length(vars)){
      plot(t, x[, vars[i]])
      grid()
   }
}
