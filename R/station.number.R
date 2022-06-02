#' @title Sampling Station Numbering
#' 
#' @description Functions to extract or determine trawl sampling station identification numbers. 
#'              A sampling station is to be interpreted as a particular location, e.g. if two tows share
#'              a station number they can be considered as having been sampled in the same location. 
#'              Station numbers can be assigned or determined based on spatial proximity.
#' 
#' @param longitude,latitude Numeric vector in decimal degrees.
#' @param time Numeric vector or time (\sQuote{POSIX}) objects.
#' @param distance.tolerance Distance (in kilometers) below which a pair of points is considered to belong to the same sampling station.
#' @param time.tolerance Numeric value specifying the time-difference below which a pair of points is considered 
#'                       to have occured at the same time. 
#'
#' @section Methods:
#' \describe{
#'    \item{\code{station.number}}{Generic \code{station.number} method.}
#'    \item{\code{station.number.default}}{Default \code{station.number} method.}
#' }
#' 

#' @export station.number
station.number <- function(x, ...) UseMethod("station.number")

#' @describeIn station.number Default 'station.number' method.
#' @rawNamespace S3method(station.number,default)
station.number.default <- function(longitude = NULL, latitude = NULL, time = NULL,
                                   distance.tolerance = NULL, ...){
   # Check length of coordinate vectors:
   if (length(longitude) != length(latitude))
      stop("'longitude' and 'latitude' vectors have inconsistent lengths.")
   
   # Check length of time vector:
   if (!is.null(time) & (length(time) != length(longitude)))
      stop("'time' and coordinate vectors must have the same lengths.")
   
   # Take absolute value of tolerance values:
   if (is.null(distance.tolerance)) stop("'distance.tolerance' must be specified.")
   distance.tolerance <- abs(distance.tolerance)
   if (!is.null(time.tolerance)) time.tolerance <- abs(time.tolerance)
   
   # Check whether there are NA values in the coordinates:
   if (any(is.na(longitude) | is.na(latitude))) stop("There are NA values in the input coordinates.")
   
   # Cluster by proximate distance:
   tree <- stats::hclust(stats::dist(gulf.spatial::deg2km(longitude, latitude)))
   if (any(tree$height < distance.tolerance)){
      k <- min(which(rev(tree$height) < distance.tolerance))
      k <- max(k, 2)
      V <- stats::cutree(tree, k = k)
   }else{
      V <- 1:length(longitude)
   }
   
   # Cluster by proximate time:
   if (!is.null(time.tolerance)){
      tree <- stats::hclust(stats::dist(as.numeric(time) / 60))
      if (any(tree$height < time.tolerance)){
         k <- min(which(rev(tree$height) < time.tolerance))
         k <- max(k, 2)
         V <- paste(V, "-", stats::cutree(tree, k = k))
         V <- match(V, unique(V))
      }else{
         V <- 1:length(longitude)
      }
   }
   
   return(V)
}

#' Assign a Station Number
#'
#' Assigns a station number for each set in the September Research Vessel
#' survey.  This function groups sets into unique sampling sites. This function
#' is used to treat various types of repeated samples (e.g. repeat sets,
#' comparative sets, etc...)
#'
#'
#' @param x A \sQuote{gulf.set} object.
#' @param method A character value specifying the method to used for fetching
#' or assigning station numbers. Accepted methods are either observed or
#' hard-coded values (\sQuote{observed}) or latitude-longitude coordinates
#' (\sQuote{calculated}, the default value).
#' @param distance.tolerance A positive numeric value stating the maximum
#' distance in kilometers beyond which proximate tows would not be considered
#' to be at a common sampling station. The default is \code{2}.
#' @param time.tolerance A positive numeric value stating the maximum time
#' interval in minutes beyond which proximate tows would not be considered to
#' be considered as comparative tows. The default is \code{10}.
#' @param list() Further arguments which are passed onto the
#' \code{\link[gulf.data]{station.number}} method.
#' @return A numeric vector of station numbers for each set in \sQuote{x}.
#' @examples
#'
#'    # Read September comparative survey data for 2005:
#'    x <- read.gulf.set(year = 2021, password="R_GulfPKG_19")
#'    x <- x[which(x$experiment != 3),]
#'
#'    # group the sampling stations using hard-coded fixed station assignment:
#'    x$set.number.observed <- station.number(x, method = "observed")
#'
#'    # group the sampling stations using spatio-temporal proximity
#'    x$set.number.calculated <- station.number(x, method = "calculated", distance.tolerance = 2, time.tolerance=10)
#'    

