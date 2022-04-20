#' @title Read Vessel Moniroting System (VMS) Data
#' 
#' @description Functions to access and update vessel monitoring system data.
#' 
#' @param year Fishery year.
#' @param month Fishery month.
#' @param dsn Registered data source.
#' @param uid User identification.
#' @param password User password.
#' @param echo Logical value specifying whether to report information while reading data.

#' @export read.vms
read.vms <- function(year, cfvn, month = 1:12, source = "file", 
                     dsn = options("gulf.oracle")[[1]]$vms$dsn, 
                     uid = options("gulf.oracle")[[1]]$vms$uid, password,
                     path = options("gulf.path")[[1]]$snow.crab$vms, 
                     echo = TRUE){
   if (source == "file"){
      # Find files:
      files <- NULL
      if (!missing(cfvn)){
         #for (i in 1:length(cfvn)) files <- c(files, list.files(path = paste0(path, year), pattern = paste0("^", cfvn[i], ".Rdata"), full.names = TRUE))
         files <- paste0(path, year, "/", cfvn, ".Rdata")
         files <- files[file.exists(files)]
      }else{
         for (i in 1:length(year)) files <- c(files, gulf.utils::locate(path = paste0(path, year[i])))
      }
      
      if (length(files) == 0) return(NULL)
      
      # Read files:
      r <- list()
      if (length(files) > 0){
         for (i in 1:length(files)){
            str <- paste0("Reading file ", i, " of ", length(files), " : '", files[i], "'\n")
            if (echo) cat(str)
            load(files[i])
            x$date <- as.character(x$date)
            r[[i]] <- x
         }
      }   
         
      # Collapse files to single data frame:
      while (length(r) > 1){
         #print(length(r))
         for (i in seq(2, length(r), by = 2)){
            r[[i]] <- rbind(r[[i]], r[[i-1]])
            r[i-1] <- list(NULL)
         }
         r <- r[!unlist(lapply(r, is.null))] 
      }
      if (length(r) == 1) r <- r[[1]]
      
      # Format date fields:
      r$time <- substr(r$date, 12, 19)
      r$date <- substr(r$date, 1, 10)         
      
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
         cat(paste0("Reading VMS data : '", month.name[as.numeric(month[i])], "'\n"))
         
         # Build query:
         query <- paste0("SELECT REGION, VR_NUMBER, LATITUDE, LONGITUDE, POSITION_UTC_DATE, SPEED_KNOTS FROM VMS.VMS_GULF_POSITIONS_4RST WHERE to_char(POSITION_UTC_DATE, 'YYYY') = ", year, " AND to_char(POSITION_UTC_DATE, 'MM') = ", month[i])
         #query <- paste0("SELECT * FROM VMS.VMS_GULF_POSITIONS_4RST WHERE to_char(POSITION_UTC_DATE, 'YYYY') = ", year, " AND to_char(POSITION_UTC_DATE, 'MM') = ", month[i])
         
         # Fetch and concatenate data:
         x <- rbind(x, RODBC::sqlQuery(channel, query, believeNRows = FALSE))
      }
      
      # Close channel:
      RODBC::odbcClose(channel)
      
      # Format names:
      names(x) <- gsub("REGION", "region", names(x))
      names(x) <- gsub("VR_NUMBER", "cfvn", names(x))
      names(x) <- gsub("LATITUDE", "latitude", names(x))
      names(x) <- gsub("LONGITUDE", "longitude", names(x))
      names(x) <- gsub("POSITION_UTC_DATE", "date", names(x))
      x <- x[c("region", "cfvn", "date", "longitude", "latitude")]
      x$date <- as.character(x$date)
      
      return(x)
   }

   return(NULL)
}

#' @export update.vms
update.vms <- function(..., path = options("gulf.path")[[1]]$snow.crab$vms){
   # Read data:
   y <- read.vms(source = "oracle", ...)
   
   # Write to file: 
   years <- as.numeric(unique(substr(unique(y$date), 1, 4)))
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
