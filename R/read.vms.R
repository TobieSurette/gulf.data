#' @title Read Vessel Moniroting System (VMS) Data
#' 
#' @description Functions to access and update vessel monitoring system data.
#' 
#' @param year Fishery year.
#' @param month Fishery month.
#' @param dsn Registered data source.
#' @param uid User identification.
#' @param password User password.
#'

#' @export read.vms
read.vms <- function(year, month = 1:12, dsn = "VMS", uid = "GULF_SCIENCE", password){
   # Open channel:
   channel <- RODBC::odbcConnect(dsn, uid, password)

   # Parse and format input parameters:
   month <- intersect(month, 1:12)
   month <- gsub(" ", "0", formatC(month, width = 2))

   # Read data:
   x <- NULL
   for (i in 1:length(month)){
      # Build query:
      query <- paste0("SELECT VR_NUMBER, LATITUDE, LONGITUDE, POSITION_UTC_DATE FROM VMS.VMS_GULF_POSITIONS_4RST WHERE to_char(POSITION_UTC_DATE, 'YYYY') = ", 
                      year, " AND to_char(POSITION_UTC_DATE, 'MM') = ", months[i])
   
      # Fetch and concatenate data:
      x <- rbind(x, sqlQuery(channel, query, believeNRows = FALSE))
   }
   
   # Close channel:
   RODBC::odbcClose(channel)

   # Format names:
   names(x) <- gsub("VR_NUMBER", "cfvn", names(x))
   names(x) <- gsub("LATITUDE", "latitude", names(x))
   names(x) <- gsub("LONGITUDE", "longitude", names(x))
   names(x) <- gsub("POSITION_UTC_DATE", "date", names(x))
   x <- x[c("cfvn", "date", "longitude", "latitude")]

   return(x)
}

#' @export update.vms
update.vms <- function(..., path = "A:/VMS project/VMS daily points/"){
   # Read data:
   x <- read.vms(...)
   
   # Write to file: 
   file <- paste0(path, "VMS points ", year, ".csv")
   write.table(x, file = file, row.names = FALSE, sep = ",")
   save(x, file = gsub(".csv", ".rdata", file))
}
