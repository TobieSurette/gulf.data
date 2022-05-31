#' @title Read Survey Catch Data
#' 
#' @description Read trawl catch data from various groundfish, crustacean and pelagic surveys.
#' 
#' @param year Survey year.
#' @param species Species name or code. 
#' @param survey Survey name.
#' @param dsn Data source name.
#' @param uid User identification.
#' @param password Oracle database password.
#' 

#' @export read.gulf.cat
read.gulf.cat <- function(year, species, survey = "rv", source = "oracle", 
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
   query <- paste("select * from GLF_GROUNDFISH.V_GSCARD_TYPE_C_", survey, sep = "")
   query <- paste(query, "where extract(year from SDATE) in", paste("(", paste(year, collapse = ","), ")", sep = ""))
   if (!missing(species)) query <- paste0(query, " AND SPECIES in (", paste(species, collapse = ","), ")")
   
   # Read data:
   x <- read.oracle(query = query, dsn = dsn, uid = uid, ...)
   
   # Format table names:
   names(x) <- gsub("_", ".", tolower(names(x)))
   names(x) <- gsub("[.]no$", ".number", names(x))
   names(x) <- gsub("^no[.]", "number.", names(x))
   names(x) <- gsub("^wt[.]", "weight.", names(x))
   names(x) <- gsub("sdate", "date", names(x))
   names(x) <- gsub("bskt.caught", "number.basket", names(x)) 
   names(x) <- gsub("remarks", "comment", names(x))
   
   # Format data and time:
   x$date <- substr(x$date, 1, 10)
   
   # Format comments:
   if ("comment" %in% names(x)) x$comment[is.na(x$comment)] <- "" else x$comment <- ""
   
   # Convert data to numeric:
   for (i in 1:ncol(x)) if (all((gsub("[0-9.]", "", x[,i]) == "") | is.na(x[,i]))) x[,i] <- as.numeric(x[,i])
   
   # Delete irrelevant or empty variables:  
   keep <- c("date", "vessel.code", "cruise.number", "set.number", "species", "number.caught", "weight.caught", "weight.sampled", "number.basket", "comment")
   x <- x[, intersect(keep, names(x))]
   
   class(x) <- c("gulf.cat", class(x))
   gulf.metadata::key(x) <- c("date", "vessel.code", "cruise.number", "set.number", "species")   
   
   return(x) 
}
