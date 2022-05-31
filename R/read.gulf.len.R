#' @title Read Survey Length-Frequency Data
#' 
#' @description Read trawl length-frequency data from various groundfish, crustacean and pelagic surveys.
#' 
#' @param year Survey year.
#' @param species Species name or code. 
#' @param survey Survey name.
#' @param dsn Data source name.
#' @param uid User identification.
#' @param password Oracle database password.
#' 

#' @export read.gulf.len
read.gulf.len <- function(year, species, survey = "rv", source = "oracle", 
                          dsn = options("gulf.oracle")[[1]]$rvs$dsn, 
                          uid = options("gulf.oracle")[[1]]$rvs$uid, ...){
   
   # Define keywords for Oracle query:
   keywords <- c("ECOSYSTEM", "NORTHUMBERLAND", "SENTINEL", "INSHORE", "JANUARY", "JUVENILE", "SEASONAL", "HISTORICAL", "SCALLOP")
   names(keywords) <- c("rvs", "nss", "sens", "ins", "jans", "juvs", "seas", "his", "scas")
   
   # Parse 'survey' arguments:
   survey <- keywords[gulf.metadata::project(survey)]
   
   # Parse 'species' argument:
   if (!missing(species)) if (is.character(species)) species <- gulf.data::species(species)
   
   # Build SQL query:
   query <- paste("select * from GLF_GROUNDFISH.V_GSCARD_TYPE_L_", survey, sep = "")
   query <- paste(query, "where extract(year from SDATE) in", paste("(", paste(year, collapse = ","), ")", sep = ""))
   if (!missing(species)) query <- paste0(query, " AND SPECIES in (", paste(species, collapse = ","), ")")
   
   # Read data:
   x <- read.oracle(query = query, dsn = dsn, uid = uid, ...)
   
   # Format table names:
   names(x) <- gsub("_", ".", tolower(names(x)))
   names(x) <- gsub("[.]no$", ".number", names(x))
   names(x) <- gsub("^no[.]", "number.", names(x))
   names(x) <- gsub("sdate", "date", names(x))
   names(x) <- gsub("remarks", "comment", names(x))
   x$ratio[x$ratio == "0"] <- "1"
   names(x) <- gsub("number[.]len", "freq", names(x))
   names(x) <- gsub("grp", "group", names(x))  
   names(x) <- gsub("start.len", "start.length", names(x))  
   names(x) <- gsub("len.int", "length.interval", names(x))  
   
   # Format data and time:
   x$date <- substr(x$date, 1, 10)
   
   # Convert data to numeric:
   for (i in 1:ncol(x)) if (all((gsub("[0-9.]", "", x[,i]) == "") | is.na(x[,i]))) x[,i] <- as.numeric(x[,i])
   
   # Buffer frequencies with zeroes:
   fvars <- names(x)[grep("freq", names(x))]
   for (i in 1:length(fvars)) x[is.na(x[, fvars[i]]), fvars[i]] <- 0
   
   # Format length interval:
   x$length.interval <- 1 / x$length.interval
   
   # Format comments:
   if ("comment" %in% names(x)) x$comment[is.na(x$comment)] <- "" else x$comment <- ""
   
   # Delete irrelevant or empty variables:  
   delete <- c("card.type", "gsseriesid", "inf.id", "stratum", "repeat.number", "uarea", "experiment", 
               "cfvn", "bottom", "trip.number", "strat.2", "station", "det.id", "card.image")
   x <- x[, setdiff(names(x), delete)] 
   
   class(x) <- c("gulf.len", class(x))
   gulf.metadata::key(x) <- c("date", "vessel.code", "cruise.number", "set.number", "species", "group", "start.length")
   
   return(x) 
}
