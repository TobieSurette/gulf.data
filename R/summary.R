#' \code{gulf.data} Summaries
#' 
#' @description Functions that generate data summaries for various types of data objects.
#' 
#' @param by Character string(s) specifying which variables to group by when summarizing.
#' @param category Biological category string(s). See \code{\link{category}} for more details.
#' @param weight Logical value specifying whether to return a summary by weights rather than counts.
#' 
#' @examples 
#' 
 
#' @describeIn summary Generate summaries for snow crab survey set data.
#' @export
summary.scsset <- function(x, ...){
   # Initialize result variable:
   res <- NULL

   for (i in 1:nrow(x)){
      # Read probe files:
      esonar   <- trim(read.esonar(x[i, ]))
      headline <- trim(read.star.oddi(x[i, ], probe = "headline"))
      tilt     <- trim(read.star.oddi(x[i, ], probe = "footrope"))

      # Calculate minimum distance between eSonar coordinates and survey logbook coordinates:
      esonar.distance <- NA
      if (length(esonar) > 0) esonar.distance <- min(distance(longitude(x[i,]), latitude(x[i,]), esonar$longitude, esonar$latitude))

      # Counts of data records:
      tmp <- data.frame(date = x$date[i],
                        tow.number = x$tow.number[i],
                        valid = x$valid[i],
                        esonar = n(esonar),
                        minilog = n(minilog),
                        headline = n(headline),
                        tilt = n(tilt),
                        esonar.distance = esonar.distance)

      # Apped results:
      res <- rbind(res, tmp)
   }

   return(res)
}

#' @describeIn summary Generate summaries for snow crab survey biological data.
#' @export
summary.scsbio <- function(x, category, ...){
   # Parse input arguments:
   if (!missing(category)) if (!is.character(category)) stop("'category' must be a vector of character strings.")
   if (!is.character(by)) stop("'by' must be a vector of character strings.")

   # Get catch summary:
   if (!missing(category)) return(catch(x, category = category, ...))  

   # Print data summary:
   describe(x)

   cat("\nData Summary : \n")
   cat(paste0("                             Crab : ", nrow(x), "\n"))
   cat(paste0("                            Males : ", sum(is.category(x, "M"), na.rm = TRUE), "\n"))
   cat(paste0("                          Females : ", sum(is.category(x, "F"), na.rm = TRUE), "\n"))
   cat(paste0("                     Mature Males : ", sum(is.category(x, "MM"), na.rm = TRUE), "\n"))
   cat(paste0("                   Immature Males : ", sum(is.category(x, "MI"), na.rm = TRUE), "\n"))
   cat(paste0("                Legal-sized Males : ", sum(is.category(x, "MGE95"), na.rm = TRUE), "\n"))
   cat(paste0("               Male Skip-Moulters : ", sum(is.category(x, "MISC345"), na.rm = TRUE), "\n"))
   cat(paste0("   Legal-sized Male Skip-Moulters : ", sum(is.category(x, "MIGE95SC345"), na.rm = TRUE), "\n"))
   cat(paste0("                 Commercial Males : ", sum(is.category(x, "COM"), na.rm = TRUE), "\n"))
   cat(paste0("              Commercial Recruits : ", sum(is.category(x, "TMMSC12GE95"), na.rm = TRUE), "\n"))
   cat(paste0("             Commercial Residuals : ", sum(is.category(x, "TMMSC345GE95"), na.rm = TRUE), "\n"))
   cat(paste0("                   Mature Females : ", sum(is.category(x, "FM"), na.rm = TRUE), "\n"))
   cat(paste0("                 Immature Females : ", sum(is.category(x, "FI"), na.rm = TRUE), "\n"))
}

#' @describeIn summary Generate data summary for \strong{eSonar} data.
#' @export
summary.esonar <- function(x, year, trim = TRUE, ...){
   # Load E-Sonar data:
   if (missing(x) & !missing(year)) x <- read.esonar(year = year)

   # Read tow data:
   y <- read.scsset(year = unique(x$year), print = FALSE)

   # Trim data:
   if (truncate) x <- truncate(x, ...)

   # Define aggregating variables:
   vars <- c("year", "month", "day", "tow.id", "tow.number")
   res <- aggregate(list(speed = x$speed), x[vars], mean, na.rm = TRUE)
   res <- cbind(res, aggregate(list(heading = x$heading), x[vars], mean, na.rm = TRUE)["heading"])
   res <- cbind(res, aggregate(list(longitude.start = x$longitude), x[vars], min, na.rm = TRUE)["longitude.start"])
   res <- cbind(res, aggregate(list(latitude.start = x$latitude), x[vars], min, na.rm = TRUE)["latitude.start"])
   res <- cbind(res, aggregate(list(longitude.end = x$longitude), x[vars], max, na.rm = TRUE)["longitude.end"])
   res <- cbind(res, aggregate(list(latitude.end = x$latitude), x[vars], max, na.rm = TRUE)["latitude.end"])

   # Attach tow validity
   index <- match.data.frame(res[c("date", "tow.id")], y[c("date", "tow.id")])
   res$valid <- y$valid[index]
   res$tow.number.logbook <- y$tow.number[index]

   # Calculate tow distance:
   res$distance       <- distance(res$longitude.start, res$latitude.start, res$longitude.end,        res$latitude.end, pairwise = FALSE)
   res$distance.start <- distance(res$longitude.start, res$latitude.start, y$longitude.start.logbook[index], y$latitude.start.logbook[index], pairwise = FALSE)
   res$distance.end   <- distance(res$longitude.end,   res$latitude.end,   y$longitude.end.logbook[index],   y$latitude.end.logbook[index], pairwise = FALSE)

   # Net measurements:
   res <- cbind(res, aggregate(list(depth = x$depth), x[vars], mean, na.rm = TRUE)["depth"])
   res <- cbind(res, aggregate(list(wingspread = x$doormaster), x[vars], mean, na.rm = TRUE)["wingspread"])
   res <- cbind(res, aggregate(list(headline = x$headline), x[vars], mean, na.rm = TRUE)["headline"])

   # Number of non-NA observations:
   fun <- function(x) return(sum(!is.na(x)))
   res <- cbind(res, aggregate(list(n.depth = x$depth), x[vars], fun)["n.depth"])
   res <- cbind(res, aggregate(list(n.wingspread = x$doormaster), x[vars], fun)["n.wingspread"])
   res <- cbind(res, aggregate(list(n.headline = x$headline), x[vars], fun)["n.headline"])
   res <- cbind(res, aggregate(list(n = x$depth), x[vars], length)["n"])

   # Calculate sampling rate:
   x$time <- time2sec(time(x))

   # Calculate length of data interval:
   res$duration <- aggregate(list(x = x$time), x[vars], function(x) return(diff(range(x))))$x + 1

   # Calculate data sampling rate:
   fun <- function(x){
      d <- table(diff(x))
      return(as.numeric(names(d[d == max(d)]))[1])
   }
   res <- cbind(res, aggregate(list(sampling.rate = x$time), x[vars], fun)["sampling.rate"])

   # Round-off results:
   rvars <- c("heading", "depth", "wingspread", "headline")
   if (round){
      res[rvars] <- round(res[rvars], 1)
      res[c("speed")] <- round(res[c("speed")], 2)
      res[c("distance", "distance.start", "distance.end")] <- round(res[c("distance", "distance.start", "distance.end")], 3)
   }

   # Sort results:
   vars <- c("year", "month", "day", "tow.number", "tow.id")
   res <- sort(res, by = vars)
   rownames(res) <- NULL

   return(res)
}

