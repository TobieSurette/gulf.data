# "http://dmapps/en/scuba/reports/dive.log.xlsx", to = "dive log.xlsx")
# "http://dmapps/en/scuba/reports/section/?year=2021") # Read in transect section table:
# "http://dmapps/en/scuba/reports/dive/?year=2021") # Read dives table:
# "http://dmapps/en/scuba/reports/outing/?year=2021") # Read outings table:
# "http://dmapps/en/scuba/reports/observations/?year=2018") # Read biological data:
# "http://dmapps/en/scuba/reports/scuba_transect/?year=2021") # Scuba transect table:

#' @param year Study year(s).
#' @param table Character string specifying which data table to read. Options are \sQuote{outings}, \sQuote{dives},
#'              \sQuote{sections}, \sQuote{biological} or \sQuote{observations}.
#' @param compress Logical value specifying whether to automatically remove empty data columns or rows.
#' @param source Data source.
#'                           
#' @examples 
#' x <- read.scuba(2012:2014)  # Read all SCUBA data tables for the 2012 to 2014 seasons.
#' x <- read.scuba(2021)       # Read all SCUBA data tables for the 2021 season.
#' 
#' x <- read.scuba(2021, table = "section") 

read.scuba <- function(year, table, compress = TRUE, source = "dmapps"){
   # Define Scuba data path:
   path <- "http://dmapps/en/scuba/reports/"
   
   # Process 'table' argument:
   if (!missing(table)){
      table <- match.arg(tolower(table), c("sections", "transects", "dives", "biological", "observations"))
      if (table == "biological") table <- "observations"
   } 
   
   # Read outings table:
   if (!missing(year)){
      outings <- NULL
      for (i in 1:length(year)) outings <- rbind(outings, read.csv(paste0(path, "outing/?year=", year[i])))
   }else{
      outings <- read.csv(path, "outing/?year")
   }
   if (compress){
      # Remove empty columns:
      outings <- gulf.utils::compress(outings)
      
      # Remove empty rows:
      outings <- outings[which(outings$transect != ""), ]
   } 
   if (nrow(outings) > 0){
      outings$date <- substr(outings$datetime, 1, 10)
      outings$time <- substr(outings$datetime, 12, 19)
      names(outings) <- gsub("^id$", "outing.id", names(outings))
      names(outings) <- gsub("_", ".", names(outings))
      
      # Re-order variables:
      start <- c("outing.id", "transect", "date", "time")
      end <- c("weather.notes", "comment")
      outings <- outings[, c(start, setdiff(names(outings), c(start, end)), end)]
      
      # Remove empty rows:
      remove <- c("datetime", "transect.id")
      outings <- outings[, setdiff(names(outings), remove)]   
   }

   # Read dive table:
   if (!missing(year)){
      dives <- NULL
      for (i in 1:length(year)) dives <- rbind(dives, read.csv(paste0(path, "dive/?year=", year[i])))
   }else{
      dives <- read.csv(path, "dive/?year")
   }
   # Remove empty columns:
   if (compress) dives <- gulf.utils::compress(dives)
   if (nrow(dives) > 0){
      # Format variable names:
      names(dives) <- gsub("sample", "outing", names(dives))
      names(dives) <- gsub("_", ".", names(dives))
      names(dives) <- gsub("^id$", "dive.id", names(dives))
      
      # Format date and time:
      dives$date <- substr(dives$start.descent, 1, 10)
      dives$start.time <- substr(dives$start.descent, 12, 19)
      dives$bottom.time.mins <- dives$bottom.time
      
      # Re-order variables:
      start <- c("dive.id", "outing.id", "transect", "diver", "date", "start.time", "bottom.time.mins")
      end <- c("comment")
      dives <- dives[, c(start, setdiff(names(dives), c(start, end)), end)]
      
      # Remove empty rows:
      remove <- c("created.by", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "updated.by.id", "diver.id", "transect.id", "outing", "start.descent", "bottom.time")
      dives <- dives[, setdiff(names(dives), remove)]     
   }

   # Collate data tables:   
   res <- list(outings = outings, dives = dives)
   
   return(res)
}





   

