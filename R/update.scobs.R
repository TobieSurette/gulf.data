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
#' @param database Oracle database name.
#' @param username Oracle user name.
#' @param password Oracle password.

#' @export update.scobs
update.scobs <- function(year, zone, path = "//ent.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/AquaRes_Common/Crab/Offshore Crab Common/", 
                         type = "sea", Rfile = TRUE, csv = TRUE, ...){
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   if (missing(zone)) zone <- c("12", "E", "F", "18", "19")
   zone <- toupper(zone)
   zone <- gsub("12E", "E", zone)
   zone <- gsub("12F", "F", zone)
   
   flag <- FALSE
   
   # Loop over years:
   for (i in 1:length(year)){
      for (j in 1:length(zone)){
         path <- paste0(path, "Fishing Year ", year[i], "/Observer Data/Observer A/Sud du golfe/", "Zone ", zone[j])
   
         writeable <- Sys.chmod(path = path)
         if (!writeable) stop(paste("Unable to write to: ", path))
         
         # Upload data:
         x <- read.scobs(year = year[i], zone = zone[j], type = type, source = "oracle", ...)
         
         # Build file name:
         if (zone[j] %in% c("E", "F")) str <- paste0("12", zone[j]) else str <- zone[j]
         if (type == "sea") str <- paste0(str, "S") else str <- paste0(str, "P")
         
         # Write data:
         if (Rfile) save(x, file = paste0(path, "/", str, year[i], ".Rdata"))
         if (csv) write.table(x, file = paste0(path, "/", str, year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
      }
   }
}
