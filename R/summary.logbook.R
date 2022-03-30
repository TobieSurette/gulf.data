#' @title Summarize Fishery Logbook Data
#' 
#' @description Generates a summary of fishery logbook data.
#' 

#' @export
summary.logbook <- function(x, by = c("cfvn"), resolution){
   # Round off coordinates:
   if (!missing(resolution)){
      x$lon <- round(x$latitude / resolution) * resolution
      x$lat <- round(x$longitude / resolution) * resolution
      by <- c(by, c("lon", "lat"))
   }

   # Calculate grid statistics:
   grid <- aggregate(list(landings = x[, "slip.prop.day"]), by = x[by], sum, na.rm = TRUE)
   grid$effort <- aggregate(list(effort = x[, "trap.day"]), by = x[by], sum, na.rm = TRUE)$effort
   grid$cpue <- grid$landings / grid$effort
   
   # Calculate grid areas:
   if (!missing(resolution)){
      grid$area <- NA
      lats <- sort(unique(grid$lat))
      for (i in 1:length(lats)){
         yy <- c(lats[i] - resolution/2, lats[i] + resolution/2)
         xx <- c(-64 - resolution/2, -64 + resolution/2) # Area does not change with longitude.
         xx <- c(xx[1], xx[1], xx[2], xx[2])
         yy <- c(yy[1], yy[2], yy[2], yy[1])
         tmp <- gulf.spatial::deg2km(xx, yy)
         p <- gulf.graphics::as.polygon(tmp$x, tmp$y)
         grid$area[grid$lat == lats[i]] <- gulf.graphics::area(p)
      }      
   }
   
   return(grid)
}
