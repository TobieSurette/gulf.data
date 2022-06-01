#' @title Read Scuba Survey Data
#' 
#' @description Functions to access data from the SCUBA transect survey in the southern Gulf of Saint Lawrence.
#' 
#' @param year Study year(s).
#' @param table Character string specifying which data table to read. Options are \sQuote{outings}, \sQuote{dives}, \sQuote{sections}, \sQuote{biological} or \sQuote{observations}.
#' @param compress Logical value specifying whether to automatically remove empty data columns or rows.
#' @param source Data source.
#'                           
#' @examples 
#' x <- read.scuba(2012:2014)  # Read all SCUBA data tables for the 2012 to 2014 seasons.
#' x <- read.scuba(2021)       # Read all SCUBA data tables for the 2021 season.
#' 
#' x <- read.scuba(2021, table = "section") # Read transect sectio data from the 2021 season.


# "http://dmapps/en/scuba/reports/dive.log.xlsx", to = "dive log.xlsx")
# "http://dmapps/en/scuba/reports/section/?year=2021" # Read in transect section table:
# "http://dmapps/en/scuba/reports/dive/?year=2021" # Read dives table:
# "http://dmapps/en/scuba/reports/outing/?year=2021" # Read outings table:
# "http://dmapps/en/scuba/reports/observations/?year=2018" # Read biological data:
# "http://dmapps/en/scuba/reports/scuba_transect/?year=2021" # Scuba transect table:

#' @export read.scuba
read.scuba <- function(year, table, compress = TRUE, source = "dmapps"){
   # Define Scuba data path:
   path <- paste0(options()$gulf.path$lobster$scuba, "reports/")
   
   # Process 'table' argument:
   if (!missing(table)){
      table <- match.arg(tolower(table), c("sections", "transects", "dives", "biological", "observations"))
      if (table == "biological") table <- "observations"
   } 
   
   
   outings <- read.csv("http://dmapps/en/scuba/reports/outing/?year")
   dives <- read.csv("http://dmapps/en/scuba/reports/dive/?year")
   
   # Build transect index table:
   transects <- read.csv("http://dmapps/en/scuba/reports/dive/?year")
   names(transects) <- gsub("_", ".", names(transects))
   transects$date <- substr(transects$start.descent, 1, 10)
   transects <- unique(transects[c("transect", "transect.id")])
   transects <- transects[!is.na(transects$transect.id), ]
   transects$region <- unlist(lapply(strsplit(gsub(")", "", transects$transect), "[(]"), function(x) x[length(x)]))

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
      outings$region <- transects$region[match(outings$transect, transects$transect)]
      outings$region[is.na(outings$region)] <- ""
      
      # Re-order variables:
      start <- c("region", "outing.id", "transect", "date", "time")
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
      dives$region <- transects$region[match(dives$transect, transects$transect)]
      dives$region[is.na(dives$region)] <- ""
      
      # Remove empty rows:
      remove <- c("created.by", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "updated.by.id", "diver.id", "transect.id", "outing", "start.descent", "bottom.time")
      dives <- dives[, setdiff(names(dives), remove)]   
      
      # Re-order variables:
      start <- c("region", "dive.id", "outing.id", "transect", "diver", "date", "start.time", "bottom.time.mins")
      end <- c("comment")
      dives <- dives[, c(start, setdiff(names(dives), c(start, end)), end)]
   }
   
   # Read section table:
   if (!missing(year)){
      sections <- NULL
      for (i in 1:length(year)) sections <- rbind(sections, read.csv(paste0(path, "section/?year=", year[i])))
   }else{
      sections <- read.csv(path, "section/?year")
   }
   # Remove empty columns:
   if (compress) sections <- gulf.utils::compress(sections)
   if (nrow(sections) > 0){
      # Format variable names:
      names(sections) <- gsub("sample", "outing", names(sections))
      names(sections) <- gsub("_", ".", names(sections))
      names(sections) <- gsub("^id$", "section.id", names(sections))
      names(sections) <- gsub("^transect$", "transect.id", names(sections))
  
      sections$region <- transects$region[match(sections$transect.id, transects$transect.id)]
      sections$region[is.na(sections$region)] <- ""
      
      # Format 'comment field:
      sections$comment[is.na(sections$comment)] <- ""
      sections$comment <- tolower(sections$comment)
      
      # Remove empty rows:
      remove <- c("created.by", "dive", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "interval.display", "outing", "updated.by.id", "diver.id", "outing", "start.descent", "bottom.time")
      sections <- sections[, setdiff(names(sections), remove)]  
      
      # Re-order variables:
      start <- c("region", "outing.id", "dive.id", "section.id", "transect.id", "diver", "date", "side.display")
      end <- c("comment")
      sections <- sections[, c(start, setdiff(names(sections), c(start, end)), end)]
      
      # Identify complex or uncompleted sections:
      ix <- sort(unique(c(grep("complex", sections$comment), 
                          grep("compl[ea]te", sections$comment), 
                          grep("skip", sections$comment), 
                          grep("scrap", sections$comment),
                          grep("no habitat", sections$comment),
                          grep("no observations", sections$comment))))
      sections$completed <- TRUE
      sections$completed[ix] <- FALSE
      
      # Define visibility:
      sections$visibility <- "Good"
      ix <- sort(unique(c(grep("alg", sections$comment), 
                          grep("kelp", sections$comment))))
      sections$comment[ix] <- "Poor"
      ix <- grep("visib", sections$comment)
      sections$visibility[ix] <- "Poor"
   }
   
   # Read biological data:
   if (!missing(year)){
      observations <- NULL
      for (i in 1:length(year)) observations <- rbind(observations, read.csv(paste0(path, "observations/?year=", year[i])))
   }else{
      observations <- read.csv(path, "observations/?year")
   }
   # Remove empty columns:
   if (compress) observations <- gulf.utils::compress(observations)
   if (nrow(observations) > 0){
      # Format variable names:
      names(observations) <- gsub("sample", "outing", names(observations))
      names(observations) <- gsub("_", ".", names(observations))
      names(observations) <- gsub("^id$", "observation.id", names(observations))
      names(observations) <- gsub("^transect$", "transect.id", names(observations))
 
      # Format 'comment field:
      observations$comment[is.na(observations$comment)] <- ""
      observations$comment <- tolower(observations$comment)     
      
      # Remove empty rows:
      remove <- c("created.by", "dive", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "interval.display", "outing", "updated.by.id", "diver.id", "outing", "start.descent", "bottom.time")
      observations <- observations[, setdiff(names(observations), remove)]  
      
      # Format empty string values:
      observations$sex[is.na(observations$sex)] <- ""
      observations$egg.status[is.na(observations$egg.status)] <- ""
      observations$side.display[is.na(observations$side.display)] <- ""
   }
   
   # Collate data tables:   
   res <- list(outings = outings, 
               dives = dives, 
               sections = sections)
   
   return(res)
}





   

