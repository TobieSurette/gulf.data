#' @title Check Data
#' 
#' @description Check data for errors or inconsistencies.
#' 
#' @param x Target object.
#' 
#' @examples
#' x <- read.logbook(2021)
#' check(x)

#' @export
"check" <- function(x, ...) UseMethod("check")

#' @export
check.logbook <- function(x, ...){
   cat(paste0(nrow(x), " x ", ncol(x), " 'logbook' object:\n"))
   
   # Missing coordinates:
   ix <- which(is.na(x$longitude) | is.na(x$latitude) | (x$longitude == 0) | (x$latitude == 0))
   if (length(ix) > 0) cat(paste0("            Records with missing coordinates : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   # Coordinates range check:
   ix <- which((x$longitude > -59) | (x$longitude < -66) | (x$latitude < 44) | (x$latitude > 49))
   if (length(ix) > 0) cat(paste0("      Records with coordinates outside range : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   # Depth coordinates range check:
   d <- gulf.spatial::depth(x$longitude, x$latitude) 
   ix <- which(d < 45 | d > 200)
   if (length(ix) > 0) cat(paste0("      Records with coordinates at odd depths : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Trap hauls:
   ix <- which((is.na(x$trap.day) | (x$trap.day == 0)) & (x$slip.prop.day > 0))
   if (length(ix) > 0) cat(paste0("             Records with missing trap hauls : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Soak time:
   ix <- which(is.na(x$soak.time) | (x$soak.time <= 0))
   if (length(ix) > 0) cat(paste0("             Records with missing soak times : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Allocation codes:
   ix <- which(is.na(x$allocation.code) | (x$allocation.code == 0))
   if (length(ix) > 0) cat(paste0("                 Records no allocation codes : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Depth check:
   ix <- which(x$depth < 45 | x$depth > 200)
   if (length(ix) > 0) cat(paste0("                     Records with odd depths : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # CFVN check:
   ix <- which(is.na(x$cfvn))
   if (length(ix) > 0) cat(paste0("                   Records with missing CFVN : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
  
   # Grid check:
   ix <- which((x$longitude >= -66) & (x$longitude <= -59) & (x$latitude >= 44) & (x$latitude <= 49))
   d <- gulf.spatial::deg2grid(x$longitude[ix], x$latitude[ix])
   iy <- which(x$grid.calc[ix] != d)
   ix <- ix[iy]
   if (length(ix) > 0) cat(paste0("                  Records with invalid grids : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Missing grids:
   ix <- which((x$grid.calc == "") | is.na(x$grid.calc) | nchar(x$grid.calc) != 4)
   if (length(ix) > 0) cat(paste0("                  Records with missing grids : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   
   # Check dates:
   ix <- which(is.na(x$date.caught) | nchar(x$date.caught) != 10)
   if (length(ix) > 0) cat(paste0("            Records with missing date caught : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   ix <- which(is.na(x$date.landed) | nchar(x$date.landed) != 10)
   if (length(ix) > 0) cat(paste0("            Records with missing date landed : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
   ix <- which(is.na(x$date.sailed) | nchar(x$date.sailed) != 10)
   if (length(ix) > 0) cat(paste0("            Records with missing date sailed : ", length(ix), " (", round(100 * sum(x$slip.prop.day[ix], na.rm = TRUE) / sum(x$slip.prop.day, na.rm = TRUE), 1), "% of landings)\n"))
}
