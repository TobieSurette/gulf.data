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

#' @export
check.scsbio <- function(x, ...){
   
   # Initialize result variable:
   res <- rbind(res, data.frame(index = ix, msg))
   
   # Check that biological records have corresponding 
   x$tow.id <- tow.id(x)
   vars <- c("date", "tow.id")
   years <- sort(unique(year(unique(x$date))))
   for (i in 1:length(years)){
      ix <- which(year(x) == years[i])
      s <- read.scsset(years[i], valid = 1)
      iy <- match(x[ix, vars], s[vars])
      if (any(is.na(iy))){
         tmp <- unique(x[ix[which(is.na(iy))], vars])
         msg <- NULL
         for (j in 1:nrow(tmp)){
            msg[j] <- paste0("Biological record with date ", tmp$date[j], ", tow ID '", tmp$tow.id[j], "' has no corresponding snow crab tow.")
            cat(paste0(msg[i], "\n"))
         }
      }
   }
   
   # Males with female gonad colour measurements:
   ix <- which((b$sex == 1) & !is.na(b$gonad.colour))
   if (length(ix) > 0){
      msg <- NULL
      for (i in 1:length(ix)){
         msg[i] <- paste0("Crab with ", x$date[ix[i]], ", and tow ID '", x$tow.id[ix[i]], "' is a male with gonad colour ", x$gonad.colour[ix[i]], ".")
         cat(paste0(msg[i], "\n"))
      }
      res <- rbind(res, data.frame(index = ix, msg))
   }
   
   # Males with female egg colour measurements:
   ix <- which((b$sex == 1) & !is.na(b$egg.colour))
   if (length(ix) > 0){
      msg <- NULL
      for (i in 1:length(ix)){
         msg[i] <- paste0("Crab with ", x$date[ix[i]], ", and tow ID '", x$tow.id[ix[i]], "' is a male with egg colour ", x$egg.colour[ix[i]], ".")
         cat(paste0(msg[i], "\n"))
      }
      res <- rbind(res, data.frame(index = ix, msg))
   }
   
   # Males with female egg remaining measurements:
   x$eggs.remaining[x$eggs.remaining == "*"] <- ""
   x$eggs.remaining <- as.numeric(x$eggs.remaining)
   ix <- which((x$sex == 1) & !is.na(x$egg.remaining))
   if (length(ix) > 0){
      msg <- NULL
      for (i in 1:length(ix)){
         msg[i] <- paste0("Crab with ", x$date[ix[i]], ", and tow ID '", x$tow.id[ix[i]], "' is a male with eggs remaining ", x$egg.remaining[ix[i]], ".")
         cat(paste0(msg[i], "\n"))
      }
      res <- rbind(res, data.frame(index = ix, msg))
   }
   
   # Females with chela height measurements:
   ix <- which((x$sex == 2) & !is.na(x$chela.height))
   if (length(ix) > 0){
      msg <- NULL
      for (i in 1:length(ix)){
         msg[i] <- paste0("Crab with ", x$date[ix[i]], ", and tow ID '", x$tow.id[ix[i]], "' is a female with chela height ", x$chela.height[ix[i]], ".")
         cat(paste0(msg[i], "\n"))
      }
      res <- rbind(res, data.frame(index = ix, msg))
   }
   
   invisible(res)
}
