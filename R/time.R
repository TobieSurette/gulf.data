#' Trawl Event Times
#' 
#' @description These are functions for extracting the times at which various events occur during trawling. 
#'              For example, there are the start and stop times determined by on-board observers or trawl touchdown and 
#'              liftoff times.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2020)
#' start.time(x)
#' 
#' x <- read.esonar(2020, tow.id = "GP354F")
#' start.time(x)
#' stop.time(x)

#' @describeIn time Generic \code{start.time} method. This is the time at which trawling is considered to have begun. 
#' @export start.time
start.time <- function(x, ...) UseMethod("start.time")

#' @describeIn time Generic \code{stop.time} method. This is the time at which trawling is considered to have stopped. 
#' @export stop.time
stop.time <- function(x, ...) UseMethod("stop.time")

#' @describeIn time Generic \code{touchdown.time} method. This is the time at which the trawl makes its initial bottom contact. 
#' @export touchdown.time
touchdown.time <- function(x, ...) UseMethod("touchdown.time")

#' @describeIn time Generic \code{liftoff.time} method. This is the time at which the trawl makes is lifted off the bottom for hauling. 
#' @export liftoff.time
liftoff.time <- function(x, ...) UseMethod("liftoff.time")

#' @describeIn time Generic \code{haul.time} method. This is the time at which the trawl is considered to have been hauled. 
#' @export haul.time
haul.time <- function(x, ...) UseMethod("haul.time")

# @describeIn time Extract the trawl start time for snow crab set data.
#' @rawNamespace S3method(start.time,scsset)
start.time.scsset <- function(x, ...){
   v <- rep("", nrow(x))
   index <- which((deblank(x$start.time) != "")  &  !is.na(x$start.time))
   v[index] <- x$start.time[index]
   index <- which((v == "") & (deblank(x$start.time.logbook) != "")  &  !is.na(x$start.time.logbook))
   v[index] <- x$start.time.logbook[index]
   v <- as.POSIXct(paste(as.character(gulf.utils::date(x)), v))
   return(v)
}

# @describeIn time Extract the trawl stop time for snow crab set data.
#' @rawNamespace S3method(stop.time,scsset)
stop.time.scsset <- function(x, ...){
   v <- rep("", nrow(x))
   index <- which((deblank(x$end.time) != "")  &  !is.na(x$end.time))
   v[index] <- x$end.time[index]
   index <- which((v == "") & (deblank(x$end.time.logbook) != "")  &  !is.na(x$end.time.logbook))
   v[index] <- x$end.time.logbook[index]
   v <- as.POSIXct(paste(as.character(gulf.utils::date(x)), v))
   return(v)
}

# @describeIn time Extract start time for \code{probe} data.
#' @rawNamespace S3method(start.time,probe)
start.time.probe <- function(x, ...){
   if (gulf.metadata::project(x) == "scs"){
      year <- as.numeric(format(gulf.utils::date(unique(x$date)), format="%Y"))
      y <- data.frame(date = as.character(unique(gulf.utils::date(x))), tow.id = tow.id(x), stringsAsFactors = FALSE)
      z <- read.scsset(year)
      r <- start.time(z[match(y[key(z)], z[key(z)]), ])
   }
   
   return(r)  
}

# @describeIn time Extract stop time for \code{probe} data.
#' @rawNamespace S3method(stop.time,probe) 
stop.time.probe <- function(x, ...){
   if (gulf.metadata::project(x) == "scs"){
      year <- as.numeric(format(gulf.utils::date(unique(x$date)), format="%Y"))
      y <- data.frame(date = as.character(unique(gulf.utils::date(x))), tow.id = tow.id(x), stringsAsFactors = FALSE)
      z <- read.scsset(year)
      r <- end.time(z[match(y[key(z)], z[key(z)]), ])
   }
   
   return(r)  
}
