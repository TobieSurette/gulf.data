#' @title Load Fishery Logbook Data
#' @rdname read.logbook
#' 
#' @description Function to load logbook and slip landings data from the southern Gulf of Saint Lawrence fisheries.
#'
#' @param x Fishery year or file name.
#' @param year Fishery year.
#' @param file File name to be loaded. File format is assumed to be comma-separated (i.e. .csv).
#' @param path File path containing comma-separated fishery logbook files (i.e. .csv).
#' @param echo Logical value specifying whether to report files to the R console as they're being read.  
#' @param ... Not used.

#' @export read.logbook
read.logbook <- function(x, species = "snow crab", source = "r", ...){
   if (!missing(species)){
      if (is.numeric(species)) species <- tolower(species(species)[1])
      species <- match.arg(tolower(species), "snow crab")
   }
   
   # Read data:
   z <- NULL
   if (species == "snow crab"){
      z <- read.sc.logbook(x, ...)
   }else{
      z <- read.ziff(x, source = source, ...)
   }
      
   class(z) <- c("logbook", class(z))
   
   return(z)
} 

#' @rdname read.logbook 
#' @export read.ziff
read.ziff <- function(x, file, echo = TRUE, ...){
   # Find data files:
   files <- NULL
   if (missing(x) & missing(file)) files <- locate.ziff(x, ...)
   if (!missing(x)) if (is.character(x)) files <- x
   if (!missing(file)) files <- file
   if (length(files) == 0) return(NULL)
   
   # Read files:
   r <- NULL
   for (i in 1:length(files)){
      if (echo) cat(paste0("Reading : '", files[i], "'\n"))
      type <- tolower(unlist(lapply(strsplit(files[i], "[.]"), function(x) x[length(x)])))
      if (type == "new"){
         read.ziff.ascii(files[i], ...)
      }
      if (type %in% c("rda", "rdata")){
         load(files[i])  
      }
      r <- rbind(r, x)
   }

   return(r)
}

#' @rdname read.logbook
#' @export read.sc.logbook
read.sc.logbook <- function(x, year, file, path = options("gulf.path")[[1]]$snow.crab$logbook, ...){
   if (!missing(x)){
      if (is.numeric(x) & missing(file))   year <- x
      if (is.character(x) & missing(file)) file <- x
   }
   if (!missing(year) & missing(file)) file <- paste0(path, "logbook ", year, ".csv")

   # Load file and format variable names:
   x <- read.csv(file, skip = 1, header = FALSE)

   # Field name fixes:
   fields <- readLines(file, n = 1)
   fields <- tolower(strsplit(fields, ",")[[1]])
   fields <- gsub("^[ #]+", "", fields)
   fields <- gsub(" ", ".", fields)
   fields <- gsub("[.]+", ".", fields)
   fields <- gsub("[_]+", ".", fields)
   fields <- gsub("lon.dec", "longitude", fields)
   fields <- gsub("lat.dec", "latitude", fields)
   fields <- gsub("lat.pas.modifier", "lat.str", fields)
   fields <- gsub("long.pas.modifier", "lon.str", fields)
   fields <- gsub("traps", "trap", fields)
   fields <- gsub("grid[.]good", "grid", fields)
   fields <- gsub("[.]$", "", fields)
   fields <- gsub("^x[.]", "", fields)
   fields[fields == "prov"] <- "province"
   fields <- gsub("^pue", "cpue", fields) 
   if (sum(fields == "grid") > 1) fields[grep("grid", fields)[2]] <- "grid.calc"
   fields[fields == "slip"] <- "slip.number"
   fields[fields == "no.de.formulaire"] <- "slip.number"
   fields <- gsub("^cfv$", "cfvn", fields) 
   if (!("zone" %in% fields) & ("zone.corrected" %in% fields)) fields <- gsub("zone.corrected", "zone", fields)
   fields <- gsub("list[.]sub[.]fleet", "fleet", fields)
   fields <- gsub("list[.]quota", "allocation.code", fields) 
   fields <- gsub("#", "number", fields)
   fields <- gsub("[(]kg[)]", "kg", fields)
   fields <- gsub("[.]-[.]", ".", fields)
   fields <- gsub("zone.de.gestion", "zone", fields)
   fields <- gsub("numero.groupe.contingent", "allocation", fields)
   fields <- gsub("numero.formulaire", "slip.number", fields)
   fields <- gsub("date.debarquement", "date.landed", fields)
   fields <- gsub("date.capture", "date.caught", fields)
   fields <- gsub("nom.intervenant", "licence.holder", fields) 
   names(x) <- fields
   
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
   x$trap.day <- as.numeric(gsub(",", "", x$trap.day))
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
         x$longitude[ix] <- -abs(tmp$long)
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
      ix <- which((x$grid == "") & (x$grid.fish != "") & (nchar(x$grid.fish) == 4))
      x$grid[ix] <- x$grid.fish[ix]
   }
   if ("grid.calc" %in% names(x)){
      x$grid.calc[x$grid.calc == "0"] <- ""
      x$grid.calc <- gsub(" +", "", x$grid.calc)
      x$grid.calc[which((nchar(x$grid.calc) <= 3) | (nchar(x$grid.calc) > 4))] <- ""
      x$grid.calc[!(substr(x$grid.calc, 1, 1) %in% c("G", "H"))] <- ""
      ix <- which((x$grid == "") & (x$grid.calc != ""))
      x$grid[ix] <- x$grid.calc[ix]
   }
   # Fill-in missing grids using other recorded grids by the same vessel:
   cfvs <- sort(unique(x$cfvn[x$grid == ""]))
   for (i in 1:length(cfvs)){
      ix <- which(x$cfvn == cfvs[i])
      if (!all(x$grid[ix] == "")){
         tab <- table(x$grid[ix])
         tab <- tab[names(tab) != ""]
         x$grid[ix[x$grid[ix] == ""]] <- names(which.max(tab))
      }
   }
   
   # Re-apply depth-coordinate and range filter:
   x$depth <- gulf.spatial::depth(x$longitude, x$latitude)  # Determine depth from coordinates.
   ix <- (x$depth < 40) | (x$depth > 200)
   x$longitude[ix] <- NA
   x$latitude[ix] <- NA
   x$longitude[which(x$longitude < -67 | x$longitude > -59)] <- NA
   x$latitude[which(x$latitude < 45 | x$latitude > 49.5)] <- NA

   # Fix slip mumber:
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
   
   if (is.character(x$t1)){
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
   if (!is.null(x$date.landed)) x$date.landed <- format.date(x$date.landed)
   if (!is.null(x$date.caught)) x$date.caught <- format.date(x$date.caught)
   if (!is.null(x$date.sailed)) x$date.sailed <- format.date(x$date.sailed)
   
   x$licence.holder <- gsub("[.]", " ", x$licence.holder)
   x$licence.holder <- gsub(" +", " ", x$licence.holder)
   x$licence.holder <- deblank(x$licence.holder)
   x$licence.holder <- gsub("P\\?CH", "PECH", x$licence.holder)
   x$licence.holder <- gsub("QU\\?BEC", "QUEBEC", x$licence.holder)
   x$licence.holder <- gsub("GASP\\?SIE", "GASPESIE", x$licence.holder)
   
   x$vessel.name <- gsub(" +", " ", x$vessel.name)
   x$vessel.name <- deblank(x$vessel.name)
   
   # Re-order variables:
   vars <- c("cfvn", "vessel.name", "zone", "province", "fleet", "licence.holder", "licence.id", "allocation.code", "slip.number", "trip.id",
             "date.caught", "date.landed", "date.sailed", "longitude", "latitude", "grid", "grid.calc", "amt.landed.kg", 
             "trap.day", "trap.trip", "soak.time")
   x <- gulf.utils::compress(x)
   vars <- intersect(vars, names(x))
   x <- x[, c(vars, setdiff(names(x), vars))]
   
   # Delete obsolete variables:   
   delete <- c('sequence.jf')
   x <- x[, setdiff(names(x), delete)]
   x <- gulf.utils::compress(x)
   
   # Clean up Excel codes:
   for (i in 1:ncol(x)){
      if (is.character(x[,i])){
         x[,i] <- gsub("#VALUE!", "", x[,i])
         x[,i] <- gsub("#DIV/0!", "", x[,i])
      }
   }
   
   return(x)
}

#' @rdname read.logbook
#' @export read.ziff.ascii
read.ziff.ascii <- function(x, convert = TRUE, as.character = FALSE, ...){
   # Define variable names, formats, fill characters and descriptions:
   fmt = c("cfvn",                   "A6",       
           "vessel.tonnage",         "I5",      
           "vessel.tonnage.class",   "I1",     
           "vessel.length",          "I3",     
           "vessel.length.class",    "I1",    
           "horsepower",             "I5",     
           "horsepower.class",       "I1",    
           "homeport",               "I5",     
           "district",               "I3",       
           "gulf.based",             "I1",      
           "company",                "I4",      
           "report",                 "A7",      
           "trip",                   "I2",      
           "date.landed",            "A8",       
           "nafo.area",              "A3",       
           "nafo.division",          "A3",       
           "gear.code",              "I2",       
           "gear.category",          "I1",       
           "port.landed",            "I5",      
           "port.district",          "I3",       
           "company.code",           "I4",      
           "community.code",         "I5",       
           "species.group",          "I1",     
           "species.code",           "I3",   
           "species.size",           "I1",       
           "species.form",           "I2",      
           "species.grade",          "A1",       
           "measurement.unit",       "A1",      
           "landed.quantity",        "I9",       
           "landed.value",           "F9.2",    
           "round.weight",           "I9",       
           "effort.flag",            "A1",       
           "main.species.caught",    "A3",       
           "main.species.sought",    "A3",      
           "mile200",                "I1",       
           "trip.fraction",          "F3.1",    
           "days.at.sea",            "F3.1",     
           "days.on.ground",         "F3.1",    
           "days.fished",            "F4.2",    
           "hours.fished",           "F4.1",    
           "gear.amount",            "I4",      
           "date.caught",            "A8",      
           "depth.code",             "A1",      
           "region.id",              "A1",      
           "latitude",               "F8.4",    
           "longitude",              "F8.4",    
           "comment",                "A44")
   
   # Read file: 
   y <- utils::read.fortran(file = x, format = fmt[seq(2,length(fmt),2)], comment.char = "") 
   names(y) <- fmt[seq(1,length(fmt),2)]
   
   # Remove dots from empty integer data fields:
   if (!as.character){
      ix <- which(substr(fmt[seq(2,length(fmt),2)],1,1) == "I")
      for (i in 1:length(ix)){
         y[, ix[i]] <- gsub(".", " ", y[, ix[i]], fixed = TRUE)
      }
   }
   
   # Convert to user-friendly format:
   if (convert & !as.character){
      names(y) <- gsub("nafo.area", "nafo.subdivision", names(y))
      
      y$longitude[y$longitude == 0] <- NA
      y$latitude[y$latitude == 0]   <- NA
      y$nafo.division               <- gsub(" ", "", y$nafo.division, fixed = TRUE)
      y$nafo.subdivision            <- gsub(" ", "", y$nafo.subdivision, fixed = TRUE)
      y$nafo.division               <- toupper(y$nafo.division)
      y$nafo.subdivision            <- paste0(toupper(substr(y$nafo.subdivision, 1, nchar(y$nafo.subdivision)-1)),
                                              tolower(substr(y$nafo.subdivision, nchar(y$nafo.subdivision), nchar(y$nafo.subdivision))))
      y$nafo.division[y$nafo.division == ""]       <- NA
      y$nafo.subdivision[y$nafo.subdivision == ""] <- NA
      #y$gear.class   <- gear.str(y$gear.code, input = "stacac", output = "alpha")
      y$gear.class   <- ""
      y$year         <- as.numeric(substr(y$date.landed, 1, 4))
      y$year.landed  <- y$year
      y$month.landed <- as.numeric(substr(y$date.landed, 5, 6))
      y$day.landed   <- as.numeric(substr(y$date.landed, 7, 8))
   }
   
   return(y)
}


