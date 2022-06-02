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
   
   # Build transect index table:
   transects <- sort(unique(read.csv("http://dmapps/en/scuba/reports/outing/?year")$transect))
   transects <- data.frame(transect = transects)
   transects <- transects[transects$transect != "", , drop = FALSE]
   transects$region <- unlist(lapply(strsplit(gsub(")", "", transects$transect), "[(]"), function(x) x[length(x)]))
   transects$name <- transects$transect
   transects$number <- as.numeric(unlist(lapply(strsplit(gsub("T-", "", transects$transect), " "), function(x) x[1])))
   transects <- transects[, setdiff(names(transects), "transect")]
   transects <- transects[, c("region", "number", "name")]
   
   # Read outings table:
   if (!missing(year)){
      outings <- NULL
      for (i in 1:length(year)) outings <- rbind(outings, read.csv(paste0(path, "outing/?year=", year[i])))
   }else{
      outings <- read.csv(path, "outing/?year")
   }
   if (nrow(outings) > 0){
      outings$date <- substr(outings$datetime, 1, 10)
      outings$time <- substr(outings$datetime, 12, 19)
      names(outings) <- gsub("^id$", "outing.id", names(outings))
      names(outings) <- gsub("_", ".", names(outings))
      names(outings) <- gsub("^transect$", "transect.name", names(outings))
      outings$region <- transects$region[match(outings$transect.name, transects$name)]
      outings$region[is.na(outings$region)] <- ""
      
      # Re-order variables:
      start <- c("region", "date", "time", "outing.id", "transect.name")
      end <- c("weather.notes", "comment")
      outings <- outings[, c(start, setdiff(names(outings), c(start, end)), end)]
      
      # Remove empty rows:
      remove <- c("datetime", "transect.id")
      outings <- outings[, setdiff(names(outings), remove)]   
      
      if (compress){
         # Remove empty columns:
         outings <- gulf.utils::compress(outings)
         
         # Remove empty rows:
         outings <- outings[which(outings$transect != ""), ]
      } 
   }

   # Read dive table:
   if (!missing(year)){
      dives <- NULL
      for (i in 1:length(year)) dives <- rbind(dives, read.csv(paste0(path, "dive/?year=", year[i])))
   }else{
      dives <- read.csv(path, "dive/?year")
   }
   if (nrow(dives) > 0){
      # Format variable names:
      names(dives) <- gsub("sample", "outing", names(dives))
      names(dives) <- gsub("_", ".", names(dives))
      names(dives) <- gsub("^id$", "dive.id", names(dives))
      
      # Format date and time:
      dives$date <- substr(dives$start.descent, 1, 10)
      dives$start.time <- substr(dives$start.descent, 12, 19)
      dives$bottom.time.mins <- dives$bottom.time
      names(dives) <- gsub("^transect$", "transect.name", names(dives))
      dives$region <- transects$region[match(dives$transect.name, transects$name)]
      dives$region[is.na(dives$region)] <- ""
      
      # Remove empty rows:
      remove <- c("created.by", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "updated.by.id", "diver.id", "transect.id", "outing", "start.descent", "bottom.time")
      dives <- dives[, setdiff(names(dives), remove)]   
      
      # Re-order variables:
      start <- c("region", "date", "start.time", "dive.id", "outing.id", "transect.name", "diver", "bottom.time.mins")
      end <- c("comment")
      dives <- dives[, c(start, setdiff(names(dives), c(start, end)), end)]
      
      # Remove empty columns:
      if (compress) dives <- gulf.utils::compress(dives)
   }
   
   # Read section table:
   if (!missing(year)){
      sections <- NULL
      for (i in 1:length(year)) sections <- rbind(sections, read.csv(paste0(path, "section/?year=", year[i])))
   }else{
      sections <- read.csv(path, "section/?year")
   }
   if (nrow(sections) > 0){
      # Format variable names:
      names(sections) <- gsub("sample", "outing", names(sections))
      names(sections) <- gsub("_", ".", names(sections))
      names(sections) <- gsub("^id$", "section.id", names(sections))
      names(sections) <- gsub("^transect$", "transect.number", names(sections))
      
      tmp <- sections[c("region", "transect.number")]
      names(tmp) <- c("region", "number")
      ix <- match(tmp, transects[c("region", "number")])
      sections$transect.name <- transects$name[ix]
      sections$transect.name[is.na(sections$transect.name)] <- ""
      
      # Format 'comment field:
      sections$comment[is.na(sections$comment)] <- ""
      sections$comment <- tolower(sections$comment)
      
      # Remove empty rows:
      remove <- c("created.by", "dive", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "interval.display", "outing", "updated.by.id", "diver.id", "outing", "start.descent", "bottom.time")
      sections <- sections[, setdiff(names(sections), remove)]  
      
      # Re-order variables:
      start <- c("region", "date", "outing.id", "transect.name", "diver", "dive.id", "section.id", "side.display")
      end <- c("comment")
      sections <- sections[, c(start, setdiff(names(sections), c(start, end)), end)]
      
      # Identify complex or uncompleted sections:
      ix <- sort(unique(c(grep("complex", sections$comment), 
                          grep("compl[ea]te", sections$comment), 
                          grep("skip", sections$comment), 
                          grep("scrap", sections$comment),
                          grep("scrap", sections$comment),
                          grep("bigs* *roc", sections$comment),
                          grep("gros*e* *roc", sections$comment),
                          grep("not +done", sections$comment))))
      sections$completed <- TRUE
      sections$completed[ix] <- FALSE
      
      #grep("no +habitat", sections$comment)
      #grep("no +observations", sections$comment)
      
      # Define visibility:
      sections$visibility <- "Good"
      #ix <- sort(unique(c(grep("alg", sections$comment), 
      #                    grep("seaweed", sections$comment), 
      #                    grep("kelp", sections$comment))))
      #sections$visibility[ix] <- "Poor"
      ix <- sort(unique(c(grep("visi[bl]", sections$comment),
                          grep("bad +viz", sections$comment),
                          grep("no +viz", sections$comment))))
      sections$visibility[ix] <- "Poor"
      
      # Remove empty columns:
      if (compress) sections <- gulf.utils::compress(sections)
   }
   
   # Read biological data:
   if (!missing(year)){
      observations <- NULL
      for (i in 1:length(year)) observations <- rbind(observations, read.csv(paste0(path, "observations/?year=", year[i])))
   }else{
      observations <- read.csv(path, "observations/?year")
   }
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
                  "interval.display", "transect.id", "outing", "updated.by.id", "diver.id", "species.id", "outing", "start.descent", "bottom.time")
      observations <- observations[, setdiff(names(observations), remove)]  
      
      # Format empty string values:
      observations$sex[is.na(observations$sex)] <- ""
      observations$egg.status[is.na(observations$egg.status)] <- ""
      observations$side.display[is.na(observations$side.display)] <- ""
      
      # Import index variables from sections table: 
      ix <- match(observations$section.id, sections$section.id)
      observations$dive.id       <- sections$dive.id[ix]
      observations$transect.name <- sections$transect.name[ix]
      observations$diver         <- sections$diver[ix]
      
      # Re-order variables:
      start <- c("region", "date", "observation.id", "outing.id", "transect.name", "dive.id", "diver", "section.id", "section", "side.display", "species")
      end <- c("comment")
      observations <- observations[, c(start, setdiff(names(observations), c(start, end)), end)]
      
      # Remove empty columns:
      if (compress) observations <- gulf.utils::compress(observations)
   }
   
   # Collate data tables:   
   res <- list(outings = outings, 
               dives = dives, 
               sections = sections, 
               observations = observations)
   
   return(res)
}
