#' @title Trip Identifiers
#' 
#' @description Return unique identifiers for each fishing trip in a data set.
#' 
#' @param x Object containing trip information.
#' 
#' @examples 
#' x <- read.scsset(2019)
#' trip(x)
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{trip}}{Generic \code{trip} method.}
#'    \item{\code{trip.scsset}}{Determine snow crab survey trip using an analysis of date sequences.}
#' }
#' 
#' @seealso \code{\link{scsset}}

#' @rdname trip
#' @export trip
trip <- function(x) UseMethod("trip")

#' @rdname trip
#' @export 
trip.scsset <- function(x){
   v <- rep(FALSE, nrow(x))
   v[c(1, which(diff(gulf.utils::julian(base::sort(gulf.utils::date(x)))) > 2)+1)] <- TRUE
   v <- cumsum(v)

   # Corrections for SCS 2022:
   v[which((as.POSIXct(x$date) >= as.POSIXct("2022-08-10")) & (as.POSIXct(x$date) <= as.POSIXct("2022-08-17")))] <- 3
   v[(as.POSIXct(x$date) >= as.POSIXct("2022-08-23")) & (as.POSIXct(x$date) <= as.POSIXct("2022-08-26"))] <- 4
   v[(year(x) == 2022) & (as.POSIXct(x$date) >= as.POSIXct("2022-09-01"))] <- 5
   
   # Corrections for SCS 2023:
   v[which((year(x) == 2023) & (v == 4))] <- 3
   v[which((year(x) == 2023) & (v == 5))] <- 4
   v[which((year(x) == 2023) & (v == 6))] <- 5
   
   return(v)
}

#' @rdname trip
#' @export 
trip.scsset <- function(x){
   v <- rep(FALSE, nrow(x))
   v[c(1, which(diff(gulf.utils::julian(base::sort(gulf.utils::date(x)))) > 2)+1)] <- TRUE
   v <- cumsum(v)
   
   # Corrections for SCS 2022:
   v[which((as.POSIXct(x$date) >= as.POSIXct("2022-08-10")) & (as.POSIXct(x$date) <= as.POSIXct("2022-08-17")))] <- 3
   v[(as.POSIXct(x$date) >= as.POSIXct("2022-08-23")) & (as.POSIXct(x$date) <= as.POSIXct("2022-08-26"))] <- 4
   v[(year(x) == 2022) & (as.POSIXct(x$date) >= as.POSIXct("2022-09-01"))] <- 5
   
   # Corrections for SCS 2023:
   v[which((year(x) == 2023) & (v == 4))] <- 3
   v[which((year(x) == 2023) & (v == 5))] <- 4
   v[which((year(x) == 2023) & (v == 6))] <- 5
   
   return(v)
}

#' @rdname trip
#' @export 
trip.scobs <- function(x) return(x$trip.number)

#' @rdname trip
#' @export 
trip.logbook <- function(x){
   # Initialize trip vector:
   v <- rep("", nrow(x))
   
   # Define logbook date field as landed date:
   x$date <- date(x$date.landed)
   
   # Load observer data for corresponding year:
   year <- as.numeric(unique(substr(x$date, 1, 4)))
   y <- read.scobs(year, type = "sea", source = "r", echo = FALSE)
   y$date <- date(y)
   
   # Build observer trip table:
   trips <- aggregate(y["date"], by = y[c("trip.number", "cfvn", "vessel", "zone", "weight")], max)
   trips <- sort(trips, by = c("cfvn", "date"))
   
   # Loop over trips and attempt to identify corresponding logbook records:
   for (i in 1:nrow(trips)){
      ix <- which(x$cfvn == trips$cfvn[i])
      if (length(ix) > 0){
         dates <- sort(unique(date(x$date[ix])))
         iy <- which(trips$date[i] <= dates)
         if (length(iy) > 0){
            iy <- min(iy)
            iy <- c(max(iy-1, 1), iy)
            dates <- dates[iy]
            dates[is.na(dates)] <- NULL
            v[ix][which(x$date[ix] > dates[1] & x$date[ix] <= dates[2])] <- trips$trip.number[i]
         }
      }
   }
   
   return(v)
}

