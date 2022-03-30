#' @title Read Survey Data
#' 
#' @description Read trawl set data from various groundfish and lobster surveys.
#' 
#' @param year Survey year.
#' @param survey Survey name.
#' @param source Data source name.
#' 

#' @export read.gulf.set
read.gulf.set <- function(year, survey = "rv", source = "oracle", 
                          dsn = options("gulf.oracle")[[1]]$rvs$dsn, 
                          uid = options("gulf.oracle")[[1]]$rvs$uid, ...){
   
   # Define keywords for Oracle query:
   keywords <- c("ECOSYSTEM", "NORTHUMBERLAND", "SENTINEL", "INSHORE", "JANUARY", "JUVENILE", "SEASONAL", "HISTORICAL", "SCALLOP")
   names(keywords) <- c("rvs", "nss", "sens", "ins", "jans", "juvs", "seas", "his", "scas")
   
   # Parse 'survey' arguments:
   survey <- keywords[project(survey)]
   
   # Build SQL query:
   query <- paste("select * from GLF_GROUNDFISH.V_GSCARD_TYPE_S_", survey, sep = "")
   query <- paste(query, "where extract(year from SDATE) in", paste("(", paste(year, collapse = ","), ")", sep = ""))
   
   # Read data:
   x <- read.oracle(dsn = dsn, uid = uid, ...)
   
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
   
   # Set zero coordinate values to zero:
   x$longitude.start[x$longitude.start == 0] <- NA
   x$longitude.end[x$longitude.end == 0] <- NA  
   x$latitude.start[x$latitude.start == 0] <- NA
   x$latitude.end[x$latitude.end == 0] <- NA
 
   return(x) 
}

