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
read.vms <- function(year, cfvn, month = 1:12, source = "file", 
                     dsn = options("gulf.oracle")[[1]]$vms$dsn, 
                     uid = options("gulf.oracle")[[1]]$vms$uid, password,
                     path = options("gulf.path")[[1]]$snow.crab$vms){
   if (source == "file"){
      # Find files:
      files <- gulf.utils::locate(keywords = as.character(year), path = path)
      
      # Find specified 'cfvn':
      if (!missing(cfvn)){
         ix <- NULL
         for (i in 1:length(cfvn)) ix <- c(ix, grep(cfvn[i], files))
         files <- files[ix]
      }
      
      if (length(files) == 0) return(NULL)
      
      # Read files:
      r <- NULL
      if (length(files) > 0){
         for (i in 1:length(files)){
            load(files[i])
            r <- rbind(r, x)
         }
         
         # Format date fields:
         r$time <- substr(r$date, 12, 19)
         r$date <- substr(r$date, 1, 10)         
      }

      return(r)
   }
   
   if (source == "oracle"){
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

   return(NULL)
}

#' @export update.vms
update.vms <- function(..., path = options("gulf.path")[[1]]$snow.crab$vms){
   # Read data:
   y <- read.vms(...)
   
   # Write to file: 
   years <- sort(unique(year(unique(y$date))))
   for (i in 1:length(years)){
      path.year <- paste0(options("gulf.path")[[1]]$snow.crab$vms, years[i])
      if (!file.exists(path.year)) dir.create(path.year)
      cfvs <- sort(unique(y$cfvn))
      for (j in 1:length(cfvs)){
         x <- y[which(y$cfvn == cfvs[j]), ]
         save(x, file = paste0(path.year, "/", cfvs[j], ".Rdata"))
      }
   }
}
