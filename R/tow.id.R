#' Extract Tow Identifier
#' 
#' @name tow.id
#' 
#' @description Functions to extract or determine trawl tow identifiers.
#' 
#' @param x Data object.
#' @param method Character string specifying the method to be used when determining the tow identification number for a 
#'               \code{probe} object. Available methods are \sQuote{time} and \sQuote{latlong}.
#' @param max.distance Numeric valiue specifying the maximum distance tolerance (in kilometers) when determining the tow 
#'                     identification number for a \code{probe} object from lat-lon coordinates.         
#'
#' @section Methods:
#' \describe{
#'    \item{\code{tow.id}}{Generic \code{tow.id} method.}
#'    \item{\code{tow.id.default}}{Default \code{tow.id} method.}
#'    \item{\code{tow.id.probe}}{Determine tow ID for a \code{probe} object.}
#' }
#' 

#' @rdname tow.id
#' @export tow.id
tow.id <- function(x, ...) UseMethod("tow.id")

#' @rdname tow.id
#' @rawNamespace S3method(tow.id,default)
tow.id.default <- function(x, ...){
   v <- attr(x, "tow.id")
   if (is.null(v)) if ("tow.id" %in% names(x)) v <- x$tow.id
   return(v)
} 

#' @rdname tow.id
#' @rawNamespace S3method(tow.id,probe)
tow.id.probe <- function(x, method, ...){
   if (missing(method)) return(tow.id.default(x,))
   
   # Parse 'method' argument:
   if (!missing(method)) method <- match.arg(tolower(method), c("time", "latlong"))
   
   # Determine project study year:                                          
   year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))
   if (is.null(year)) stop("Unable to determine study year.")
   if (length(year) > 1) stop("Multiple years in probe dataset.")
   
   # Load project sampling data:
   if (gulf.metadata::project(x) == "scs") y <- read.scsset(year)
   
   # Match using time stamps:
   if (method == "time"){
      t <- abs(difftime(mean(time(x)), start.time(y), units = "mins"))
      v <- y$tow.id[which.min(t)] 
   }
   
   # Match using mean coordinate values:
   if (method == "latlong"){
      lat <- lat(x)
      lon <- lon(x)
      if (!is.null(lat) & !is.null(lon)){
         d <- distance(mean(lon, na.rm = TRUE), mean(lat, na.rm = TRUE), lon(y), lat(y))[1,]
         v <- y$tow.id[which.min(d)] 
      }
   }
   
   return(v)
}

#' @rawNamespace S3method(tow.id,scsbio)
tow.id.scsbio <- function(x, ...){
   if (is.null(x$tow.id)) x$tow.id <- ""
   x$tow.id[is.na(x$tow.id)] <- ""
   ix <- x$tow.id == ""
   if (any(ix)){
      s <- read.scsset(year = as.numeric(unique(substr(unique(x$date[ix]), 1, 4))))
      x$tow.id[ix] <- s$tow.id[gulf.utils::match(x[ix, c("date", "tow.number")], s[c("date", "tow.number")])]
   }
   
   return(x$tow.id)
}
