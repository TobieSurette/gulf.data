#' @title Load Snow Crab Fishery Data:
#'
#' @description Function to load logbook and slip landings data from the southern Gulf of Saint Lawrence snow crab fishery.
#'
#' @param x Fishery year or file name.
#' @param year Fishery year.
#' @param file File name to be loaded. File format is assumed to be comma-separated (i.e. .csv).
#' @param path File path containing comma-separated fishery logbook files (i.e. .csv).
#'

#' @export read.sc.logbook
read.sc.logbook <- function(x, year, file, path = "//ent.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/AquaRes_Common/Crab/Databases/Logbooks Excel/"){
   if (!missing(x)){
      if (is.numeric(x) & missing(file))   year <- x
      if (is.character(x) & missing(file)) file <- x
   }
   if (!missing(year) & missing(file)) file <- paste0(path, "logbook ", year, ".csv")

   # Load file and format variable names:
   x <- read.csv(file)
   names(x) <- gsub("[.]+", ".", tolower(names(x)))
   names(x) <- gsub("lon.dec", "longitude", names(x))
   names(x) <- gsub("lat.dec", "latitude", names(x))
   names(x) <- gsub("lat.pas.modifier", "lat.str", names(x))
   names(x) <- gsub("long.pas.modifier", "lon.str", names(x))

   # Convert numeric variables:
   vars <- c("longitude", "latitude")
   for (i in 1:length(vars)) x[, vars[i]] <- as.numeric(x[, vars[i]])

   # Coordinate range checks:
   x$longitude <- -abs(x$longitude)
   x$longitude[which(x$longitude < -67 | x$longitude > -59)] <- NA
   x$latitude[which(x$latitude < 45 | x$latitude > 49.5)] <- NA

   # Depth-coordinate filter:
   x$depth <- depth(x$longitude, x$latitude)  # Determine depth from coordinates.
   ix <- (x$depth < 40) | (x$depth > 200)
   x$longitude[ix] <- NA
   x$latitude[ix] <- NA

   # Effort corrections:
   x[, "x.trap.day"] <- as.numeric(gsub(",", "", x[, "x.trap.day"]))
   x$slip.prop.day[grep("VALUE", x$slip.prop.day)] <- ""
   x$slip.prop.day <- as.numeric(x$slip.prop.day)

   # Coordinate fixes:
   ix <- which(is.na(x$longitude) & !is.na(x$lon.str) &  (floor(log10(x$lon.str)) == 10))
   x$longitude[ix] <- -abs(x$lon.str[ix] / 10^9)
   ix <- which(is.na(x$latitude) & !is.na(x$lat.str) & (floor(log10(x$lat.str)) == 10))
   x$latitude[ix] <- abs(x$lat.str[ix] / 10^9)
   ix <- which(is.na(x$longitude) & !is.na(x$lon.str) & (floor(log10(x$lon.str)) == 5))
   x$longitude[ix] <- -dmm2deg(x$lon.str[ix] / 10^2)
   ix <- which(is.na(x$latitude) & !is.na(x$lat.str) & (floor(log10(x$lat.str)) == 5))
   x$latitude[ix] <- dmm2deg(x$lat.str[ix] / 10^2)

   # Loran C conversions:
   ix <- which((is.na(x$longitude) | is.na(x$latitude)) & (!is.na(x$lon.str) & !is.na(x$lat.str)))
   ix <- ix[x$lon.str[ix] < 35000]
   if (length(ix) > 0){
      tmp <- loran2deg(x$lat.str[ix], x$lon.str[ix])
      x$longitude[ix] <- tmp$long
      x$latitude[ix] <- tmp$lat
   }

   # Fix grid names:
   x$grid <- gsub(" +", "", x$grid)
   x$grid[which(nchar(x$grid) <= 3)] <- ""

   # Re-apply depth-coordinate and range filter:
   x$depth <- depth(x$longitude, x$latitude)  # Determine depth from coordinates.
   ix <- (x$depth < 40) | (x$depth > 200)
   x$longitude[ix] <- NA
   x$latitude[ix] <- NA
   x$longitude[which(x$longitude < -67 | x$longitude > -59)] <- NA
   x$latitude[which(x$latitude < 45 | x$latitude > 49.5)] <- NA

   return(x)
}
