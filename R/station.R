#' Return a Station Number
#'
#' @description Assign or retrieve a sampling station identifier.
#'
#' @param longitude,latitude Coordinates in decimal degrees.
#' @param time Numeric or (\sQuote{POSIX}) time objects.
#' @param distance.tolerance Numeric value specifying the distance (in kilometers) below which a pair of points
#'                           is considered to belong to the same group or station.
#' @param time.tolerance Numeric value specifying the time-difference below which a pair of points
#'                       is considered to belong to the same group or station. The units are in minutes
#'                       if the \code{time} argument belongs to a \sQuote{POSIX} class.

#' @describeIn station
#' @export
station <- function(x, ...){
   # STATION - Generic 'station' method.
   UseMethod("station")
}

#' @describeIn station
#' @export
station.number.default <- function(longitude = NULL, latitude = NULL, time = NULL,
                                   distance.tolerance = NULL, time.tolerance = NULL, ...){
   # STATION.NUMBER.DEFAULT - Assigns a group identifier to groups of coordinate points.

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
   tree <- hclust(dist(deg2km(longitude, latitude)))
   if (any(tree$height < distance.tolerance)){
      k <- min(which(rev(tree$height) < distance.tolerance))
      k <- max(k, 2)
      V <- cutree(tree, k = k)
   }else{
      V <- 1:length(longitude) # x$set.number[x$year == years[i]]
   }

   # Cluster by proximate time:
   if (!is.null(time.tolerance)){
      tree <- hclust(dist(as.numeric(time) / 60))
      if (any(tree$height < time.tolerance)){
         k <- min(which(rev(tree$height) < time.tolerance))
         k <- max(k, 2)
         V <- paste(V, "-", cutree(tree, k = k))
         V <- match(V, unique(V))
      }else{
         V <- 1:length(longitude) # x$set.number[x$year == years[i]]
      }
   }

   # Calculate all pairwise distances between 'longitude' and 'latitude':
   #d <- distance(longitude, latitude)

   # Convert POSIX class to minutes:
   #if (!is.null(time)){
   #   if (all(substr(class(time[1]), 1, 5) == "POSIX")){
   #      time <- as.numeric(time)
   #      time <- time / 60
   #   }
   #   t <- as.matrix(dist(time))
   #}

   # Extract pairs of indices of points which are grouped together:
   #if (is.null(time)){
   #   index <- (d < distance.tolerance)
   #}else{
   #   index <- (d < distance.tolerance) & (t < time.tolerance)
   #}

   # Cluster using 'index' adjacency matrix:
   #group <- cluster(index)

   return(V)
}

#' @export
station.nssset <- function(x, y, tolerance = NULL, method = "latlon"){
   # STATION.NUMBER.NSSET - Returns the nearest Northumberland Strait master
   #   station number for a given coordinate point.

   # Check 'method' argument:
   method <- tolower(method)
   method <- match.arg(method, c("observed", "latlon"))

   # Fetch nearest master station:
   if (method == "latlon"){
      # Extract coordinates if 'x' is an 'nsset' object.
      if ("nsset" %in% class(x)){
         y <- latitude(x)
         x <- longitude(x)
      }

      # Check 'x' argument:
      if (!is.numeric(x)) stop("'x' must be either a numeric vector or an 'nsset' object.")

      if (any(is.na(x) | is.na(y)))
         stop("There are NA values in the input coordinates.")

      # Load NS master station list:
      data(ns.stations)

      # Calculate all pairwise distances between 'x' and 'y' and the complete NS master station list:
      d <- distance(x, y, ns.stations[, "longitude"], ns.stations[, "latitude"], pairwise = TRUE)

      # Find the minimum distance in each row:
      min.d <- apply(d, 1, min)

      # Create matrix of repeated vectors
      index <- repvec(min.d, ncol = dim(ns.stations)[1]) == d

      which2 <- function(x) which(x)[1]

      # Identify the index of each station:
      index <- apply(index, 1, which2)

      # Extract station numbers from the NS master station list:
      result <- ns.stations[index, "station.number"]

      # Set results with minimum distances beyond the threshold value to NA:
      if (!is.null(tolerance)){
         tolerance <- abs(tolerance)
         result[min.d > tolerance] <- NA
      }
   }

   # Fetch observed value:
   if (method == "observed"){
      # Check 'x' argument:
      if (!("nsset" %in% class(x)))
         stop("'x' must be either an 'nsset' object if observed station numbers are required.")

      result <- x$station.number
   }

   return(result)
}
