#' @title Fishing fleet
#' 
#' @description
#' Functions to extract or determine fishing fleet.
#' 
#' @param x Data object.

#' @export
fleet <- function(x, ...) UseMethod("fleet")

#' @export fleet.logbook
fleet.logbook <- function(x, ...){
   if (!("allocation.code" %in% names(x))){
      if ("status.number" %in% names(x)) x$allocation.code <- x$status.number
   }
      
   # Fix and parse allocation codes:
   if ("allocation.code" %in% names(x)){
      # Fix allocation codes:
      if (is.character(x$allocation.code)) x$allocation.code <- as.numeric(substr(x$allocation.code, 1, 5))
      ix <- which(log10(x$allocation.code) > 5)
      x$allocation.code[ix] <- floor(x$allocation.code[ix] / (10 ^ (floor(log10(x$allocation.code[ix]))-4)))
      
      # Convert to licence description:
      x$allocation <- allocation(x$allocation.code)
      
      # Fill-in empty allocation categories:
      ix <- which(is.na(x$allocation))
      if (length(ix) > 0){
         # Ad hoc corrections:
         x$allocation[which(is.na(x$allocation) & (x$fleet == "SNOW CRAB - EEL GROUND"))] <- "Indigenous"
         
         x$allocation[intersect(ix, which(x$licence.holder == "OPILIO (UPM / MFU) INC"))] <- "New access"
         x$allocation[intersect(ix, grep("first nation", tolower(x$licence.holder)))]     <- "Indigenous"
         x$allocation[intersect(ix, which(x$zone == "19"))]                               <- "Zone 19"
         x$allocation[intersect(ix, which((x$zone == "12") & x$province == "PE"))]        <- "Zone 25-26" 
         x$allocation[intersect(ix, which(x$licence.holder == "INVERNESS SOUTH FISHERMEN'S ASSOCIATION"))] <- "Zone 18"
      }
   }else{
      x$allocation <- ""
      if ("status" %in% names(x)){
         x$allocation[grep("native", x$status)]       <- "indigenous"
         x$allocation[grep("autochtones", x$status)]  <- "indigenous"
         x$allocation[grep("permanent", x$status)]    <- "traditional"
         x$allocation[grep("temporary", x$status)]    <- "new access"
      }
   }
   
   # Zone corrections:
   x$zone[toupper(x$allocation) == "ZONE 12E FISHERS"] <- "12E"
   x$zone[toupper(x$allocation) == "ZONE 12F (QUEBEC; SHARING)"] <- "12F"
   
   # Define fishing fleet:
   fleet <- rep("", nrow(x))

   # Zone-based assignments:
   fleet[grep("zone 18", tolower(x$allocation))]     <- "Zone 18"
   fleet[grep("zone 25", tolower(x$allocation))]     <- "zone 25-26"
   fleet[x$zone == "18"]                             <- "Zone 18"
   fleet[x$zone == "19"]                             <- "Zone 19"
   fleet[toupper(x$zone) %in% c("12E", "E")]         <- "Zone 12E"
   fleet[toupper(x$zone) %in% c("12F", "F")]         <- "Zone 12F"
   fleet[x$zone == "25"]                             <- "Zone 25-26"
   fleet[x$zone == "26"]                             <- "Zone 25-26"
   
   # Allocation and status-based assignments:   
   fleet[intersect(grep("new access", tolower(x$allocation)), which((x$zone == "12") & (fleet == "")))]  <- "New Access"
   fleet[intersect(grep("indigenous", tolower(x$allocation)), which((x$zone == "12") & (fleet == "")))]  <- "Indigenous"
   fleet[intersect(grep("traditional", tolower(x$allocation)), which((x$zone == "12") & (fleet == "")))] <- "Traditional"
   
   # One-off code corrections:
   if ("allocation.code" %in% names(x)){
      fleet[which((x$allocation.code %in% 70551:70558) & (x$zone == "12"))]            <- "New Access"
      fleet[which((x$allocation.code %in% c(70576, 70578, 70579)) & (x$zone == "12"))] <- "New Access"   
   }
   fleet[which(x$province == "PEI" & x$status == "permanent")]                      <- "zone 25-26"
   fleet[which((fleet == "") & (x$zone == "12") & (x$status %in% c("autochtones", "native")))] <- "Indigenous"
   fleet[which((fleet == "") & (substr(x$date.caught, 1, 4) == "2011") & (x$province == "QC") & (x$zone == "12"))] <- "Traditional"
   
   return(fleet)
}

#' @export
fleet.scobs <- function(x, ...){
   # Clean-up cfvn:
   x$cfvn <- as.numeric(x$cfvn)
   
   # Loop over years:
   years <- sort(unique(x$year))
   if (length(years) > 1) stop("Only programmed for a single year.")
   for (i in 1:length(years)){
      y <- read.logbook(years[i])
      y <- y[which(toupper(y$zone) %in% c("12", "25", "26", "18", "19", "E", "F", "12E", "12F")), ]
      y <- y[which(y$zone != "" | is.na(y$zone)), ]
      y <- compress(y)
      y$fleet <- fleet(y)
      
      # First perform match using cfvn only:
      tmp  <- aggregate(y["fleet"], by = y["cfvn"], unique)
      tmp <- tmp[unlist(lapply(tmp$fleet, length)) == 1, ]
      tmp$fleet <- unlist(lapply(tmp$fleet, function(x) x[1]))
      ix <- match(x$cfvn, tmp$cfvn)
      x$fleet <- tmp$fleet[ix]
      
      # Use cfvn and zone:
      tmp  <- aggregate(y["fleet"], by = y[c("zone", "cfvn")], unique)
      tmp <- tmp[unlist(lapply(tmp$fleet, length)) == 1, ]
      tmp$fleet <- unlist(lapply(tmp$fleet, function(x) x[1]))
      ix <- which(is.na(x$fleet))
      iy <- match(x[ix, c("zone", "cfvn")], tmp[c("zone", "cfvn")])
      x$fleet[ix] <- tmp$fleet[iy]
      
      # Use trip-matched records:
      ix <- which(is.na(x$fleet))
      if (length(ix) > 0){
         y$trip.number <- trip(y)
         iy <- match(x$trip.number[ix], y$trip.number)
         x$fleet[ix] <- y$fleet[iy]
      }
   }

   return(x$fleet)
}


