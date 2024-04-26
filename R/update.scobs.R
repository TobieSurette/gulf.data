#' @title Update Snow Crab Observer Data
#' 
#' @description Update snow crab observer data from Oracle database download.
#' 
#' @param year Year.
#' @param zone Fishing zone identifier.
#' @param path Data path location.
#' @param type Sampling type ('sea' or 'port').
#' @param Rfile Logical value specifying whether output to Rdata format.
#' @param csv Logical value specifying whether to write to CSV data format.
#' @param password Oracle password.
#' @param ... Not used.
#' 

#' @export update.scobs
update.scobs <- function(year, zone, path = options("gulf.path")[[1]]$snow.crab$observer, type = c("sea", "port"), source = "oracle", Rfile = TRUE, csv = TRUE, ...){
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   # Parse 'zone' argument:
   if (missing(zone)) zone <- c("12", "E", "F", "18", "19")
   zone <- toupper(zone)
   zone <- gsub("12E", "E", zone)
   zone <- gsub("12F", "F", zone)
   
   # Loop over years:
   for (i in 1:length(year)){
      for (j in 1:length(zone)){
         for (k in 1:length(type)){
            # Upload data:
            x <- read.scobs(year = year[i], zone = zone[j], type = type[k], source = source, ...)
            
            # Create target directory:
            if (!file.exists(paste0(path, year[i])))         dir.create(paste0(path, year[i]))
            
            # Check that data path is writable:
            if (!Sys.chmod(path = path)) stop(paste("Unable to write to: ", path))
            
            # Build file name:
            if (zone[j] %in% c("E", "F")) str <- paste0("12", zone[j]) else str <- zone[j]
            if (type[k] == "sea")  str <- paste0(str, "S") 
            if (type[k] == "port") str <- paste0(str, "P")
            
            # Write data:
            if (Rfile){
               if (!file.exists(paste0(path, year[i], "/R")))   dir.create(paste0(path, year[i], "/R"))
               save(x, file = paste0(path, year[i], "/R/", str, year[i], ".Rdata"))
            } 
            if (csv){
               if (!file.exists(paste0(path, year[i], "/csv"))) dir.create(paste0(path, year[i], "/csv"))
               write.table(x, file = paste0(path, year[i], "/csv/", str, year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
            }   
         }
      }
   }
}
