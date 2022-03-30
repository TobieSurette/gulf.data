#' @title Load Fishery Logbook Data
#'
#' @description Function to load logbook and slip landings data from the southern Gulf of Saint Lawrence fisheries.
#'
#' @param x Fishery year or file name.
#' @param year Fishery year.
#' @param file File name to be loaded. File format is assumed to be comma-separated (i.e. .csv).
#' @param path File path containing comma-separated fishery logbook files (i.e. .csv).
#'

#' @export read.logbook
read.logbook <- function(x, species = "snow crab", ...){
   # Parse 'species' argument:
   species <- species(species)[1]
   
   z <- NULL
   
   # Read data:
   if (species == 2526) z <- read.sc.logbook(x, ...)
      
   class(z) <- c("logbook", class(z))
   
   return(z)
} 

#' @export read.sc.logbook
read.sc.logbook <- function(x, year, file, path = options("gulf.path")[[1]]$snow.crab$logbook, ...){
   if (!missing(x)){
      if (is.numeric(x) & missing(file))   year <- x
      if (is.character(x) & missing(file)) file <- x
   }
   if (!missing(year) & missing(file)) file <- paste0(path, "logbook ", year, ".csv")

   # Load file and format variable names:
   x <- read.csv(file)

   # Field name fixes:
   names(x) <- gsub("[.]+", ".", tolower(names(x)))
   names(x) <- gsub("[_]+", ".", tolower(names(x)))
   names(x) <- gsub("lon.dec", "longitude", names(x))
   names(x) <- gsub("lat.dec", "latitude", names(x))
   names(x) <- gsub("lat.pas.modifier", "lat.str", names(x))
   names(x) <- gsub("long.pas.modifier", "lon.str", names(x))
   names(x) <- gsub("traps", "trap", names(x))
   names(x) <- gsub("grid[.]good", "grid", names(x))
   names(x) <- gsub("[.]$", "", names(x))
   names(x) <- gsub("^x[.]", "", names(x))
   names(x)[names(x) == "prov"] <- "province"
   names(x) <- gsub("^pue", "cpue", names(x)) 
   if (sum(names(x) == "grid") > 1) names(x)[grep("grid", names(x))[2]] <- "grid.calc"
   names(x)[names(x) == "slip"] <- "slip.number"
   names(x)[names(x) == "no.de.formulaire"] <- "slip.number"
   names(x) <- gsub("^cfv$", "cfvn", names(x)) 
   if (!("zone" %in% names(x)) & ("zone.corrected" %in% names(x))) names(x) <- gsub("zone.corrected", "zone", names(x))
   names(x) <- gsub("list[.]sub[.]fleet", "fleet", names(x))
   names(x) <- gsub("list[.]quota", "allocation.code", names(x)) 
   
   # Convert numeric variables:
   vars <- c("longitude", "latitude")
   for (i in 1:length(vars)) x[, vars[i]] <- as.numeric(x[, vars[i]])

   # Coordinate range checks:
   x$longitude <- -abs(x$longitude)
   x$longitude[which(x$longitude < -67 | x$longitude > -59)] <- NA
   x$latitude[which(x$latitude < 45 | x$latitude > 49.5)] <- NA

   # Depth-coordinate filter:
   x$depth <- gulf.spatial::depth(x$longitude, x$latitude)  # Determine depth from coordinates.
   ix <- (x$depth < 40) | (x$depth > 200)
   x$longitude[ix] <- NA
   x$latitude[ix] <- NA

   # Effort corrections:
   x[, "trap.day"] <- as.numeric(gsub(",", "", x[, "trap.day"]))
   x$slip.prop.day[grep("VALUE", x$slip.prop.day)] <- ""
   x$slip.prop.day <- as.numeric(x$slip.prop.day)

   # Coordinate fixes:
   if ("lon.str" %in% names(x)){
      ix <- which(is.na(x$longitude) & !is.na(x$lon.str) &  (floor(log10(x$lon.str)) == 10))
      x$longitude[ix] <- -abs(x$lon.str[ix] / 10^9)
      ix <- which(is.na(x$longitude) & !is.na(x$lon.str) & (floor(log10(x$lon.str)) == 5))
      x$longitude[ix] <- -gulf.spatial::dmm2deg(x$lon.str[ix] / 10^2)   
   }
   if ("lat.str" %in% names(x)){
      ix <- which(is.na(x$latitude) & !is.na(x$lat.str) & (floor(log10(x$lat.str)) == 10))
      x$latitude[ix] <- abs(x$lat.str[ix] / 10^9)
      ix <- which(is.na(x$latitude) & !is.na(x$lat.str) & (floor(log10(x$lat.str)) == 5))
      x$latitude[ix] <- gulf.spatial::dmm2deg(x$lat.str[ix] / 10^2)
   }
   
   # Loran C conversions:
   if (all(c("lon.str", "lat.str") %in% names(x))){
      ix <- which((is.na(x$longitude) | is.na(x$latitude)) & (!is.na(x$lon.str) & !is.na(x$lat.str)))
      ix <- ix[x$lon.str[ix] < 35000]
      if (length(ix) > 0){
         tmp <- gulf.spatial::loran2deg(x$lat.str[ix], x$lon.str[ix])
         x$longitude[ix] <- tmp$long
         x$latitude[ix] <- tmp$lat
      }
   }

   # Fix grid names:
   x$grid.calc[grep("VALUE", x$grid.calc)] <- ""
   if (is.null(x$grid) & !is.null(x$grid.calc)) x$grid <- x$grid.calc
   x$grid <- gsub(" ", "", x$grid)
   x$grid[grep("VALUE", x$grid)] <- ""
   x$grid[x$grid == "0"] <- ""
   x$grid <- gsub(" +", "", x$grid)
   x$grid[which(nchar(x$grid) <= 3)] <- ""
   x$grid[!(substr(x$grid, 1, 1) %in% c("G", "H"))] <- ""
   if ("grid.fish" %in% names(x)){
      x$grid.fish <- deblank(x$grid.fish)
      x$grid.fish[x$grid.fish == "0"] <- ""      
   }

   # Re-apply depth-coordinate and range filter:
   x$depth <- gulf.spatial::depth(x$longitude, x$latitude)  # Determine depth from coordinates.
   ix <- (x$depth < 40) | (x$depth > 200)
   x$longitude[ix] <- NA
   x$latitude[ix] <- NA
   x$longitude[which(x$longitude < -67 | x$longitude > -59)] <- NA
   x$latitude[which(x$latitude < 45 | x$latitude > 49.5)] <- NA

   # Fix slip:
   x$slip.number <- gsub(" ", "", x$slip.number)
   
   # Define date formatting function:
   format.date <- function(x){
      t <- table(nchar(x))
      if (max(as.numeric(names(t))) == 8){
         x <- paste0(substr(x,1,4), "-", substr(x,5,6), "-", substr(x,7,8))
         x[nchar(x) != 10] <- ""
      } 
      
      y <- x
      x <- unique(x[which(!is.na(x) & (x != ""))])
      ix <- match(y, x)
      x <- gsub("/", "-", x)
      a <- as.numeric(unlist(lapply(strsplit(x, "-"), function(x) x[1])))
      b <- as.numeric(unlist(lapply(strsplit(x, "-"), function(x) x[2])))
      c <- as.numeric(unlist(lapply(strsplit(x, "-"), function(x) x[3])))
      if (any(c > 1900)){
         year <- c
         if (any(a > 12)){
            day <- a
            month <- b
         }else{
            day <- b
            month <- a
         } 
      }else{
         year <- a
         if (any(b > 12)){
            day <- b
            month <- c
         }else{
            day <- c
            month <- b
         } 
      }
      date <- paste0(year, "-", formatC(month, width = 2), "-", formatC(day, width = 2))
      date <- gsub(" ", "0", date)
      y <- date[ix]
      y[is.na(y)] <- ""
      
      return(y)
   }
   
   # Province correction:
   x$province[grep("que", tolower(x$province))] <- "QC"
   
   # Soak time:
   if ("ti" %in% names(x)) x$t1 <- x$ti
   
   if (year == 2020){
      x$t1[x$t1 == "4,080"] <- 48
      x$t1 <- round(as.numeric(x$t1))
   }
   if (!("soak.time" %in% names(x)) & ("t1" %in% names(x))) x$soak.time <- x$t1
   if (year == 2021){
      ix <- which((x$province == "QC") & (x$soak.time > 60))
      x$soak.time[ix] <- x$soak.time[ix] / 10
   }
   if ("soak.time" %in% names(x)){
      x$soak.time[x$soak.time == 0] <- NA
   } 
   
   # Fix date fields:
   x$date.landed <- format.date(x$date.landed)
   x$date.caught <- format.date(x$date.caught)
   x$date.sailed <- format.date(x$date.sailed)
   
   x$licence.holder <- gsub("[.]", " ", x$licence.holder)
   x$licence.holder <- gsub(" +", " ", x$licence.holder)
   x$licence.holder <- deblank(x$licence.holder)
   x$licence.holder <- gsub("P\\?CH", "PECH", x$licence.holder)
   
   x$vessel.name <- gsub(" +", " ", x$vessel.name)
   x$vessel.name <- deblank(x$vessel.name)
   
   # Re-order variables:
   vars <- c("cfvn", "vessel.name", "zone", "province", "fleet", "licence.holder", "licence.id", "allocation.code", "slip.number", "trip.id",
             "date.caught", "date.landed", "date.sailed", "longitude", "latitude", "grid", "grid.calc", "amt.landed.kg", 
             "trap.day", "trap.trip", "soak.time")
   vars <- intersect(vars, names(x))
   x <- x[, c(vars, setdiff(names(x), vars))]
   
   # Delete obsolete variables:   
   delete <- c('sequence.jf')
   x <- x[, setdiff(names(x), delete)]
   x <- gulf.utils::compress(x)
   
   return(x)
}
