#' @title Read Scuba Survey Data
#' 
#' @description Functions to access data from the SCUBA transect survey in the southern Gulf of Saint Lawrence.
#' 
#' @param year Study year(s).
#' @param table Character string specifying which data table to read. Options are \sQuote{outings}, \sQuote{dives}, \sQuote{sections}, \sQuote{biological} or \sQuote{observations}.
#' @param compress Logical value specifying whether to automatically remove empty data columns or rows.
#' @param source Data source.
#' @param echo Logical value specifying whether to report progress as data tables are being read. 
#'                           
#' @examples 
#' x <- read.scuba(2012:2014)  # Read all SCUBA data tables for the 2012 to 2014 seasons.
#' x <- read.scuba(2021)       # Read all SCUBA data tables for the 2021 season.
#' 
#' x <- read.scuba(2021, table = "section") # Read transect sectio data from the 2021 season.


# "http://dmapps/en/scuba/reports/dive.log.xlsx", to = "dive log.xlsx")
# "http://dmapps/en/scuba/reports/section/?year=2021"        # Read in transect section table:
# "http://dmapps/en/scuba/reports/dive/?year=2021"           # Read dives table:
# "http://dmapps/en/scuba/reports/outing/?year=2021"         # Read outings table:
# "http://dmapps/en/scuba/reports/observations/?year=2018"   # Read biological data:
# "http://dmapps/en/scuba/reports/transects/?year=2021"      # Scuba transect table:

#' @export read.scuba
read.scuba <- function(year, table, compress = TRUE, source = "dmapps", echo = TRUE){
   # Define Scuba data path:
   path <- paste0(options()$gulf.path$lobster$scuba, "reports/")
   
   # Process 'table' argument:
   if (!missing(table)){
      table <- match.arg(tolower(table), c("sections", "transects", "dives", "biological", "observations"))
      if (table == "biological") table <- "observations"
   } 
   
   # Format outings table:
   if (echo) cat(paste0("Reading outing table.\n"))
   outings <- read.csv(paste0(path, "outing/?year"))

   # Format variables:
   outings$date <- substr(outings$datetime, 1, 10)
   outings$time <- substr(outings$datetime, 12, 19)
   names(outings) <- gsub("^id$", "outing.id", names(outings))
   names(outings) <- gsub("_", ".", names(outings))
   names(outings) <- gsub("^transect$", "transect.name", names(outings))
   outings$region <- unlist(lapply(strsplit(gsub("[)]", "", outings$transect.name), "[(]"), function(x) x[2]))
   outings$is.upm <- ifelse(outings$is.upm == "False", FALSE, ifelse(outings$is.upm == "True", TRUE, as.logical(NA)))
   outings$is.training <- ifelse(outings$is.training == "False", FALSE, ifelse(outings$is.training == "True", TRUE, as.logical(NA)))
   outings$longitude.start   <- outings$start.longitude
   outings$latitude.start    <- outings$start.latitude
   outings$longitude.end     <- outings$end.longitude
   outings$latitude.end      <- outings$end.latitude
   
   # Re-order variables:
   start <- c("region", "date", "time", "outing.id", "transect.name", "transect.id")
   end <- c("weather.notes", "comment")
   outings <- outings[, c(start, setdiff(names(outings), c(start, end)), end)]
   
   # Remove empty rows:
   remove <- c("datetime", "start.latitude.d", "start.latitude.mm", 
               "start.longitude.d", "start.longitude.mm", "end.latitude.d", "end.latitude.mm",
               "end.longitude.d", "end.longitude.mm", "start.latitude", "start.longitude",
               "end.latitude", "end.longitude") 
   outings <- outings[, setdiff(names(outings), remove)]   

   # Load and format transect table:
   if (echo) cat(paste0("Reading transect table.\n"))
   transects <- read.csv("http://dmapps/en/scuba/reports/transects/?year=2021")      
   transects$region            <- gsub(" [(]PE[)]", "", transects$region)
   transects$longitude.start   <- transects$start_longitude
   transects$latitude.start    <- transects$start_latitude
   transects$longitude.end     <- transects$end_longitude
   transects$latitude.end      <- transects$end_latitude
   transects$transect.name.old <- transects$old_name
   transects$transect.id       <- transects$id
   transects$description       <- transects$description_en
   transects$transect.number   <- transects$name
   remove <- c("description_en", "description_fr", "start_latitude_d", "start_latitude_mm",
               "start_longitude_d", "start_longitude_mm", "end_latitude_d", "end_latitude_mm",
               "end_longitude_d",    "end_longitude_mm", "start_longitude", "end_latitude",
               "end_longitude", "old_name", "start_latitude", "region_id", "id", "name")
   transects <- transects[, setdiff(names(transects), remove)]
   names(transects) <- gsub("_", ".", names(transects))
   ix <- match(transects$transect.id, outings$transect.id)
   transects$transect.name <- outings$transect.name[ix]
   start <- c("region", "transect.name", "transect.id",  "transect.name.old", "transect.number")
   end <- c("description")
   transects <- transects[, c(start, setdiff(names(transects), c(start, end)), end)]

   # Subset by year:
   if (!missing(year)) outings <- outings[year(outings$date) %in% year, ]
   
   # Read dive table:
   if (!missing(year)){
      dives <- NULL
      for (i in 1:length(year)){
         if (echo) cat(paste0("Reading dive table for ", year[i], ".\n"))
         dives <- rbind(dives, read.csv(paste0(path, "dive/?year=", year[i])))
      } 
   }else{
      dives <- read.csv(paste0(path, "dive/?year"))
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
      
      # Fill-in missing dates:
      ix <- which(is.na(dives$date))
      iy <- match(dives$outing.id[ix], outings$outing.id)
      dives$date[ix] <- outings$date[iy]
      
      # Format field names:
      names(dives) <- gsub("^transect$", "transect.name", names(dives))
      
      # Format region:
      #dives$region <- transects$region[match(dives$transect.name, transects$transect.name)]
      dives$region <- unlist(lapply(strsplit(gsub("[)]", "", dives$transect.name), "[(]"), function(x) x[2]))
      dives$region[is.na(dives$region)] <- ""

      # Add UPM and training flags:
      ix <- match(dives$outing.id, outings$outing.id)
      dives$is.upm <- as.logical(outings$is.upm[ix])
      dives$is.training <- as.logical(outings$is.training[ix])

      # Format logical variables:
      dives$was.seeded <- ifelse(dives$was.seeded == "False", FALSE, ifelse(dives$was.seeded == "True", TRUE, as.logical(NA)))
      
      # Remove empty rows:
      remove <- c("created.by", "created.at", "updated.by", "updated.at", "sample", "created.by.id", 
                  "updated.by.id", "diver.id", "transect.id", "outing", "start.descent", "bottom.time")
      dives <- dives[, setdiff(names(dives), remove)]   
      
      # Re-order variables:
      start <- c("region", "date", "start.time", "dive.id", "outing.id", "transect.name", "diver", "bottom.time.mins")
      end <- c("comment")
      dives <- dives[, c(start, setdiff(names(dives), c(start, end)), end)]
      
      # Training dive corrections:
      ix <- grep("train", tolower(dives$comment))
      dives$is.training[ix] <- TRUE
   }
   
   # Read section table:
   if (!missing(year)){
      sections <- NULL
      for (i in 1:length(year)){
         if (echo) cat(paste0("Reading section table for ", year[i], ".\n"))
         sections <- rbind(sections, read.csv(paste0(path, "section/?year=", year[i])))
      } 
   }else{
      sections <- read.csv(paste0(path, "section/?year"))
   }
   if (nrow(sections) > 0){
      # Format variable names:
      names(sections) <- gsub("sample", "outing", names(sections))
      names(sections) <- gsub("_", ".", names(sections))
      names(sections) <- gsub("^id$", "section.id", names(sections))
      names(sections) <- gsub("^transect$", "transect.number", names(sections))
      
      # Get transect name:
      sections$transect.name <- outings$transect.name[match(sections$outing.id, outings$outing.id)]
      sections$transect.name[is.na(sections$transect.name)] <- ""
      
      # Format 'comment field:
      sections$comment[is.na(sections$comment)] <- ""
      sections$comment <- tolower(sections$comment)
      
      # Add UPM, training, and seeding experiment info:
      ix <- match(sections$dive.id, dives$dive.id)
      sections$is.upm      <- as.logical(dives$is.upm[ix])
      sections$is.training <- as.logical(dives$is.training[ix])
      sections$was.seeded  <- dives$was.seeded[ix]
      
      # Format region variable:
      sections$region <- gsub(" [(]PE[)]", "", sections$region)
      sections$region[is.na(sections$region)] <- ""
      
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
   }
   
   # Read biological data:
   if (!missing(year)){
      observations <- NULL
      for (i in 1:length(year)){
         if (echo) cat(paste0("Reading observation table for ", year[i], ".\n"))
         observations <- rbind(observations, read.csv(paste0(path, "observations/?year=", year[i])))
      } 
   }else{
      observations <- read.csv(paste0(path, "observations/?year"))
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
      
      # Import variables from sections table: 
      ix <- match(observations$section.id, sections$section.id)
      observations$is.upm        <- as.logical(sections$is.upm[ix])
      observations$is.training   <- as.logical(sections$is.training[ix])
      observations$dive.id       <- sections$dive.id[ix]
      observations$transect.name <- sections$transect.name[ix]
      observations$diver         <- sections$diver[ix]

      # Format region variable:
      observations$region <- gsub(" [(]PE[)]", "", observations$region)
      observations$region[is.na(observations$region)] <- ""
      
      # Re-order variables:
      start <- c("region", "date", "observation.id", "outing.id", "transect.name", "dive.id", "diver", "section.id", "section", "side.display", "species")
      end <- c("comment")
      observations <- observations[, c(start, setdiff(names(observations), c(start, end)), end)]
   }
   
   if (compress){
      # Remove empty columns:
      outings      <- gulf.utils::compress(outings)
      dives        <- gulf.utils::compress(dives)
      sections     <- gulf.utils::compress(sections)
      observations <- gulf.utils::compress(observations)
      
      # Remove empty rows:
      outings <- outings[which(outings$transect.name != ""), ]
   } 

   # Collate data tables:   
   res <- list(transects = transects,
               outings = outings, 
               dives = dives, 
               sections = sections, 
               observations = observations)
   
   return(res)
}
