fleet.logbook <- function(x){
   
   if (!("allocation.code" %in% names(x))){
      if ("status.number" %in% names(x)) x$allocation.code <- x$status.number
   }
      
   x$zone[x$allocation == "Zone 12E Fishers"] <- "12E"
   
   if ("allocation.code" %in% names(x)){
      # Fix allocation codes:
      if (is.character(x$allocation.code)) x$allocation.code <- as.numeric(substr(x$allocation.code, 1, 5))
      ix <- which(log10(x$allocation.code) > 5)
      x$allocation.code[ix] <- floor(x$allocation.code[ix] / (10 ^ (floor(log10(x$allocation.code[ix]))-4)))
      
      # Convert to licence description:
      x$allocation <- allocation(x$allocation.code)
      
      ix <- which(is.na(x$allocation))
      
      if (length(ix) > 0){
         # Ad hoc corrections:
         x$allocation[which(is.na(x$allocation) & (x$fleet == "SNOW CRAB - EEL GROUND"))] <- "Indigenous"
         
         x$allocation[intersect(ix, which(x$licence.holder == "OPILIO (UPM / MFU) INC"))] <- "New access"
         x$allocation[intersect(ix, grep("first nation", tolower(x$licence.holder)))]     <- "Indigenous"
         x$allocation[intersect(ix, which(x$zone == "19"))]                               <- "Zone 19"
         x$allocation[intersect(ix, which((x$zone == "12") & x$province == "PE"))]        <- "zone 25-26" 
         x$allocation[intersect(ix, which(x$licence.holder == "INVERNESS SOUTH FISHERMEN'S ASSOCIATION"))] <- "Zone 18"
      }
   }else{
      x$allocation <- ""
      if ("status" %in% names(x)){
         x$allocation[grep("native", x$status)]    <- "indigenous"
         x$allocation[grep("permanent", x$status)] <- "traditional"
         x$allocation[grep("temporary", x$status)] <- "new access"
      }
   }

   
   fleet <- rep("", nrow(x))
   fleet[grep("new access", tolower(x$allocation))]  <- "New Access"
   fleet[grep("indigenous", tolower(x$allocation))]  <- "Indigenous"
   fleet[grep("traditional", tolower(x$allocation))] <- "Traditional"
   fleet[grep("zone 18", tolower(x$allocation))]     <- "Zone 18"
   fleet[grep("zone 25", tolower(x$allocation))]     <- "zone 25-26"
   fleet[x$zone == "18"]                             <- "Zone 18"
   fleet[x$zone == "19"]                             <- "Zone 19"
   fleet[x$zone == "12E"]                            <- "Zone 12E"
   fleet[x$zone == "12F"]                            <- "Zone 12F"
   fleet[x$zone == "25"]                             <- "zone 25-26"
   fleet[x$zone == "26"]                             <- "zone 25-26"
   
   return(fleet)
}

