#' @title Read Survey Set Data
#' 
#' @description Read trawl set data from various groundfish, crustacean and pelagic surveys.
#' 
#' @param year Survey year.
#' @param survey Survey name.
#' @param dsn Data source name.
#' @param uid User identification.
#' @param password Oracle database password.
#' 
#' @examples
#'
#'    # Read the 2020 set cards:
#'    x <- read.gulf.set(year = 2020)
#' 
#' @export read.gulf.set
read.gulf.set <- function(year, survey = "rv", 
                          dsn = options("gulf.oracle")[[1]]$rvs$dsn, 
                          uid = options("gulf.oracle")[[1]]$rvs$uid, ...){
   
   # Define keywords for Oracle query:
   keywords <- c("ECOSYSTEM", "NORTHUMBERLAND", "SENTINEL", "INSHORE", "JANUARY", "JUVENILE", "SEASONAL", "HISTORICAL", "SCALLOP")
   names(keywords) <- c("rvs", "nss", "sens", "ins", "jans", "juvs", "seas", "his", "scas")
   
   # Parse 'survey' arguments:
   survey <- keywords[gulf.metadata::project(survey)]
   
   # Build SQL query:
   query <- paste("select * from GLF_GROUNDFISH.V_GSCARD_TYPE_S_", survey, sep = "")
   query <- paste(query, "where extract(year from SDATE) in", paste("(", paste(year, collapse = ","), ")", sep = ""))
   
   # Read data:
   x <- read.oracle(query = query, dsn = dsn, uid = uid, ...)
   
   # Format table names:
   names(x) <- gsub("_", ".", tolower(names(x)))
   names(x) <- gsub("[.]no$", ".number", names(x))
   names(x) <- gsub("sdate", "date", names(x))
   names(x) <- gsub("uarea", "unit.area", names(x))
   names(x) <- gsub("time[.]beg", "start.time", names(x))
   names(x) <- gsub("time[.]dur", "duration", names(x))
   names(x) <- gsub("^aux$", "auxiliary", names(x))
   names(x) <- gsub("obt[.]sp", "speed.method", names(x))
   names(x) <- gsub("start[.]lat", "latitude.start", names(x))
   names(x) <- gsub("start[.]long", "longitude.start", names(x))
   names(x) <- gsub("end[.]lat", "latitude.end", names(x))
   names(x) <- gsub("end[.]long", "longitude.end", names(x))
   names(x) <- gsub("obt[.]dis", "distance.method", names(x))
   names(x) <- gsub("wind[.]dir", "wind.direction", names(x))
   names(x) <- gsub("temp[.]sur", "surface.temperature", names(x))
   names(x) <- gsub("temp[.]bot", "bottom.temperature", names(x))   
   names(x) <- gsub("salin[.]bot", "bottom.salinity", names(x)) 
   names(x) <- gsub("bt[.]slide", "btslide", names(x)) 
   names(x) <- gsub("hydro[.]stn", "hydrostation", names(x)) 
   names(x) <- gsub("^bottom$", "bottom.type", names(x))  
   names(x) <- gsub("no[.]fish", "species.fish.number", names(x))  
   names(x) <- gsub("no[.]inverts", "species.invertebrate.number", names(x))  
   names(x) <- gsub("weight[.]cat", "catch.total.weight", names(x))  
   names(x) <- gsub("warpout1", "warp.out.port", names(x))  
   names(x) <- gsub("warpout2", "warp.out.starboard", names(x))  
   names(x) <- gsub("trip[.]number", "expedition.number", names(x))  
   names(x) <- gsub("^station$", "station.number", names(x))  
   names(x) <- gsub("strat[.]2", "block.number", names(x))  
   names(x) <- gsub("remarks", "comment", names(x))
   
   # Format data and time:
   x$date <- substr(x$date, 1, 10)
   x$start.time <- gsub(" ", "0", formatC(x$start.time, width = 4))
   x$start.time <- paste0(substr(x$start.time,1,2), ":", substr(x$start.time,3,4), ":00")
   
   # Convert data to numeric:
   for (i in 1:ncol(x)) if (all((gsub("[0-9.]", "", x[,i]) == "") | is.na(x[,i]))) x[,i] <- as.numeric(x[,i])

   # Format coordinates:
   x$longitude.start <- -abs(gulf.spatial::dmm2deg(x$longitude.start))
   x$longitude.end   <- -abs(gulf.spatial::dmm2deg(x$longitude.end))
   x$latitude.start  <- gulf.spatial::dmm2deg(x$latitude.start)
   x$latitude.end    <- gulf.spatial::dmm2deg(x$latitude.end)

   # Set zero coordinate values to zero:
   x$longitude.start[x$longitude.start == 0] <- NA
   x$longitude.end[x$longitude.end == 0] <- NA  
   x$latitude.start[x$latitude.start == 0] <- NA
   x$latitude.end[x$latitude.end == 0] <- NA
   
   # Delete irrelevant or empty variables:   
   delete <- c("card.type", "inf.id", "card.image", "species.fish.number", "species.invertebrate.number", 
               "catch.total.weight", "repeat.number", "location.repeat.cnt", "gsseriesid")
   x <- x[, setdiff(names(x), delete)]
   x <- gulf.utils::compress(x, unique = FALSE)
   
   # Format comments:
   x$comment[is.na(x$comment)] <- ""
   
   class(x) <- c("gulf.set", class(x))
   gulf.metadata::key(x) <- c("date", "vessel.code", "cruise.number", "set.number")  
   
   return(x) 
}

