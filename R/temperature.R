#' @title Water Temperatures
#' 
#' @description Functions to extract of estimate water temperatures.
#' 
#' @param x Year.
#' @param year Year.
#' @param longitude,latitude Coordinates at which water temperatures are to be estimated. Units in decimal degrees.
#' @param depth Water depth at which temperatures are to be retrieved. Units are in meters. Alternatively, 
#'              setting \code{depth = 'surface'} will return surface temperatures and setting \code{depth = 'bottom'}
#'              will return bottom temperatures.
#' @param radius.search Search radius in meters used to average out temperatures within a range of the specied depth.
#' 
#' @examples 
#' # Load all temperature data as a 3-D matrix:
#' z <- temperature(2019)
#' 
#' z <- temperature(2019, depth = "surface")
#' 
#' # Load snow crab survey data:
#' s <- read.scsset(2019, valid = 1, survey = "regular")
#' 
#' # Get bottom temperatures using snow crab survey coordinates:
#' z <- temperature(2019, depth = "bottom", longitude = lon(s), latitude = lat(s))
#' 
#' # Read temperatures for the top 50 meters at the snow crab survey's coordinates:
#' z <- temperature(2019, depth = 0:50, longitude = lon(s), latitude = lat(s))
#' image(1:nrow(s), as.numeric(dimnames(z)$depth), z, xlab = "Sampling station", ylab = "Depth(m)")

#' @export
temperature <- function(x, ...) UseMethod("temperature")

#' @rawNamespace S3method(temperature,data.frame)
temperature.data.frame <- function(x, ...){
   names(x) <- tolower(names(x))
   if ("temperature" %in% names(x)) return(x$temperature)
   if (length(grep("temperature", names(x))) > 0) return(x[, grep("temperature", names(x))[1]])
   return(NULL)
}

#' @export
temperature.default <- function(x, year, longitude, latitude, depth, radius.search = 0, polygon, polygon.longitude, polygon.latitude, ...){
   
   # Parse input arguments:
   if (!missing(x) & missing(year)) if (is.numeric(x)) year <-x
   if (missing(year)) stop("'year' must be specified.")
   if (length(year) != 1) stop("'year' must be a single value.")
   
   # Locate data file:
   file <- locate(keywords = c("water", "temperature", "september", year), package = "gulf.data")
   if (length(file) == 1){
      load(file)
   }else{
      stop("Unable to locate temperature data file.")
   }
   
   if (!missing(depth)){
      if (is.character(depth)){
         depth <- match.arg(depth, c("surface", "bottom"))
         
         # Surface temperatures:
         if (depth == "surface") depth <- 0:round(radius.search)
      
         # Bottom temperatures:
         if (depth == "bottom"){
            fun <- function(x){
               ix <- which(!is.na(x))
               if (length(ix) > 1) return(x[max(ix)]) else return(NA) 
            } 
            x <- apply(x, 1:2, fun)
            tmp <- dimnames(x)
            dim(x) <- c(dim(x), 1)
            dimnames(x) <- list(longitude = tmp[[1]], latitude = tmp[[2]], depth = NULL)
         }
      }
      
      # Look up numeric depths:
      if (is.numeric(depth)){
         depth <- as.character(round(depth))
         depth <- depth[which(as.character(depth) %in% dimnames(x)[[3]])]
         x <- x[,,depth, drop = FALSE]
      }
      
   }
   
   # Interpolate temperature data at specified coordinates:
   if (!missing(longitude) & !missing(latitude)){
      if (length(longitude) != length(latitude)) stop("'longitude' and 'latitude' must be the same length.")
      
      lons <- as.numeric(dimnames(x)[[1]])
      lats <- as.numeric(dimnames(x)[[2]])
      res <- matrix(NA, nrow = length(longitude), ncol = dim(x)[3])
      for (i in 1:dim(x)[3]){
         zz <- x[,,i]
         zz[is.na(zz)] <- -1000000  # Because function does not take NA values.
         res[,i] <- akima::bilinear(x = lons, y = lats, z = zz, x0 = longitude, y0 = latitude)$z
      }
      res[res < -2] <- NA
      dimnames(res) <- list(NULL,  depth = dimnames(x)[[3]])
      x <- res
   }
      
   if (ncol(x) == 1) x <- x[, 1]
   
   return(x)   
}

#' @export bottom.temperature
bottom.temperature  <- function(x, ...) return(temperature.default(depth = "bottom", ...))

#' @export surface.temperature
surface.temperature  <- function(x, ...) return(temperature.default(depth = 0, ...))


