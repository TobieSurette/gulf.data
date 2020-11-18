#' Read \strong{gulf} data sets.
#' 
#' @description Functions to read Gulf of Saint-Lawrence data sets.
#' 
#' @param x Survey year or file name.
#' @param file File name(s). 
#' @param year Survey year(s).
#' @param species Species code or name.
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.
#' 
#' @examples  
#' # Read snow crab survey set data files:
#' x <- read.scsset()                 # Read all available data.
#' x <- read.scsset(year = 2019)      # Read single year.
#' x <- read.scsset(year = 2010:2015) # Read range of years.
#' 
#' # Read specific tow data:
#' x <- read.scsset(2020, valid = 1)  # Load only valid tows.
#' x <- read.scsset(2020, tow.id = "GP354F")
#' x <- read.scsset(2020, date = "2020-07-13")
#' x <- read.scsset(2020, zone = "F")
#' 
#' # Using snow crab set data to specify corresponding biological data:
#' b <- read.scsbio(read.scsset(2020, valid = 1, zone = "F"))
#' 
#' @seealso \code{\link[gulf.data]{scsset}}
#' @seealso \code{\link[gulf.data]{scsbio}}

#' @describeIn read Read southern Gulf of Saint Lawrence snow crab survey set data.
#' @export read.scsset
read.scsset <- function(x, file, survey, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.scsset(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.scsset(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA

         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))

      v <- NULL
      # Read fixed-width file:
      if (ext == "txt"){
         v <- read.fortran(file = file, format = c("I4", "I2", "I2", "A2", "A8", "I2", "I1", "I8", "I8", "I8",
                                                   "I8", "I8", "I8", "A8", "A8", "A8", "A8", "I5", "F4.1", "I4",
                                                   "F5.1", "A7", "I1", "I1", "A300"))

         names(v) <- c("year", "month", "day", "zone", "tow.id", "tow.number", "valid", "longitude", "latitude",
                       "longitude.start.logbook", "longitude.end.logbook", "latitude.start.logbook", "latitude.end.logbook",
                       "start.time", "end.time", "start.time.logbook", "end.time.logbook",
                       "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method",
                       "groundfish.sample", "water.sample", "comment")

         # Remove blank spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[,j] <- gulf.utils::deblank(v[,j])
      }

      # Read comma-delimited file:
      if (ext == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

      # Compress date variables:
      if (all(c("year", "month", "day") %in% names(v))){
         v$date <- as.character(gulf.utils::date(v))
         v <- cbind(v[c("date")], v[setdiff(names(v), c("date", "year", "month", "day"))])
      }
   }

   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }

   # Convert to 'scsset' object:
   v <- scsset(v)

   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}

#' @describeIn read Read southern Gulf of Saint Lawrence Northumberland Strait survey set data.
#' @export read.nssset
read.nssset <- function(x, file, survey, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.nssset(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.nssset(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA
         
         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))
      
      v <- NULL
      # Read fixed-width file:
      if (ext %in% c("new", "txt")){
         v <- read.fortran(file = file, format = c('I1','A1','A3','I3','I3','I4','I2','I2','I3','I1','I2','I2','I0','I2','I3','I1',
                                                   'F3.1','I1','F6.2','F6.2','F6.2','F6.2','I3','I3','F3.2','I1',
                                                   'I1','I1','I1','F3.1','F3.1','F3.1','I3','I3','I3','I1','I2','I2','I4','A3','I4','A1',
                                                   'I4','A1','I6','A1','A12','I3','A4','A255'))
         
         names(v) <- c('card.type', 'vessel.code', 'cruise.number', 'stratum', 'set.number', 'year', 'month', 'day', 'unit.area', 
                       'experiment', 'start.hour', 'start.minute', 'start.second', 'duration', 'gear', 'auxiliary', 'speed', 'speed.method', 
                       'latitude.start', 'longitude.start', 'latitude.end', 'longitude.end', 'depth.start', 'depth.end', 'distance', 
                       'distance.method', 'wind.direction', 'wind.force', 'tide', 'surface.temperature', 'bottom.temperature', 'bottom.salinity', 
                       'light', 'btslide', 'hydrostation', 'bottom.type', 'species.fish.number', 'species.invertebrate.number', 'catch.total.weight', 
                       'blank1', 'warp.port', 'blank2', 'warp.starboard', 'blank3', 'cfvn', 'blank4', 'expedition.number', 'block.number', 
                       'station', 'comment')
         
         # Remove blank spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[,j] <- gulf.utils::deblank(v[,j])
         
         # Remove blank columns:
         v <- v[, -grep("blank", names(v))]
         
         # Define cruise:
         v$cruise <- toupper(paste0(v$vessel.code, v$cruise.number))
         
         # Remove irrelevant fields:
         remove <- c("species.fish.number", "species.invertebrate.number", "catch.total.weight", "cfvn", "expedition.number", 
                     "light", "btslide", "hydrostation", "bottom.type", "bottom.salinity", "distance.method", "speed.method", 
                     "unit.area", "vessel.code", "cruise.number")
         v <- v[, !(names(v) %in% remove)]
         
         # Tow validity:
         v$valid <- as.numeric(v$experiment != 3)
         
         # Subtitute commas by semi-colons in comments:
         v$comment <- gsub(",", ";", v$comment)
      }
      
      # Read comma-delimited file:
      if (ext == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      
      # Compress date variables:
      if (all(c("year", "month", "day") %in% names(v))){
         v$date <- as.character(gulf.utils::date(v))
         v <- cbind(v[c("date")], v[setdiff(names(v), c("date", "year", "month", "day"))])
      }
      
      # Compress time variables:
      if (all(c("start.hour", "start.minute", "start.second") %in% names(v))){
         v$start.second[is.na(v$start.second)] <- 0
         v$start.time <- gulf.utils::time(paste0(v$start.hour, ":", v$start.minute, ":", v$start.second))
         v <- cbind(v[c("date", "start.time")], v[setdiff(names(v), c("date", "start.time", "start.hour", "start.minute", "start.second"))])
      }   
   }
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }
   
   # Convert to 'nssset' object:
   v <- nssset(v)
   
   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}

#' @describeIn read Read southern Gulf of Saint Lawrence snow crab survey biological data.
#' @export read.scsbio
read.scsbio <- function(x, file, survey, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.scsbio(x, ...)
   if (length(file) == 0) return(NULL)

   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.scsbio(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA
         
         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))

      # Read fixed-width file:
      if (ext == "txt"){
         v <- read.fortran(file = file, 
                           format = c("A1", "A2", "A2", "A4", "A1", "A2", "A1", "A3", "A1", "A1", "A3", "A4", "A1", "A1", "A6", 
                                      "A5", "A1", "A5", "A1", "A1", "A1", "A1", "A2", "A1", "A1", "A1", "A8", "A1", "A10", "A1",
                                      "A2", "A1", "A8", "A1", "A8", "A1", "A3", "A1", "A1", "A3", "A1", "A4", "A7", "A21", "A6",  
                                      "A1", "A25", "A8"))

         names(v) <- c("blank1",  "day", "month", "year", "blank2",  "zone",  "subzone", "blank3", "data.type", 
                       "blank4", "tow.number", "crab.number", "blank5", "sex", "carapace.width", "abdomen.width", 
                       "blank6", "chela.height", "maturity", "blank7",  "shell.condition", "shell.condition.mossy", 
                       "gonad.colour", "blank8", "egg.colour", "eggs.remaining", "tag.number", "blank9", "missing.legs", 
                       "blank10", "position.type", "blank11", "latitude.start", "blank12", "longitude.start", "blank13", 
                       "depth", "blank14", "soak.days", "durometer", "blank15", "trap.code",  "blank16", "samplers",   
                       "weight", "blank17", "comments", "tow.id")  
         
         # Remove blank columns:
         v <- v[, -grep("blank", names(v))]
         
         # Remove blank leading and trailing spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[, j] <- deblank(v[, j])
          
         # Convert to date:
         v$date <- as.character(gulf.utils::date(v[c("day", "month", "year")]))
         
         # Numeric conversions:
         nvars <- c("tow.number", "crab.number", "carapace.width", "abdomen.width", 
                    "chela.height", "shell.condition", "gonad.colour", "egg.colour", "latitude.start", "longitude.start",
                    "soak.days", "depth", "weight")
         f <- function(x) return(as.numeric(gsub("[*]", "", x)))
         for (j in 1:length(nvars)) v[, nvars[j]] <- f(v[, nvars[j]])
      }
      
      # Read comma-delimited file:
      if (ext == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      
      # Compress date variables:
      if (all(c("year", "month", "day") %in% names(v))){
         v$date <- as.character(gulf.utils::date(v))
         v <- cbind(v[c("date")], v[setdiff(names(v), c("date", "year", "month", "day"))])
      }
   }

   # Delete empty data rows:
   index <- (v$carapace.width > 0) | !is.na(v$abdomen.width) | !is.na(v$chela.height) | !is.na(v$shell.condition) | 
            !is.na(v$gonad.colour) | !is.na(v$egg.colour) | !is.na(v$eggs.remaining) 
   v <- v[which(index), ]
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }

   # Subset if 'scsset' object was given:
   if (!missing(x)) if ("scsset" %in% class(x)) v <- v[!is.na(gulf.utils::match(v[key.scsset()], x[key.scsset()])), ]
   
   # Convert to 'scsset' object:
   v <- scsbio(v)
      
   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}

#' @describeIn read Read southern Gulf of Saint Lawrence snow crab survey by-catch data.
#' @export read.scscat
read.scscat <- function(x, file, species, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.scscat(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Load data:
   v <- NULL
   
   # No file read:
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.scscat(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA
         
         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))
      
      v <- NULL
      
      # Read comma-delimited file:
      if (ext == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      
      # Compress date variables:
      if (all(c("year", "month", "day") %in% names(v))){
         v$date <- as.character(gulf.utils::date(v))
         v <- cbind(v[c("date")], v[setdiff(names(v), c("date", "year", "month", "day"))])
      }
   }
   
   # Subset by species:
   if (!missing(species)){
      if (is.character(species)) species <- unlist(lapply(species(species, drop = FALSE), function(x) return(x[1]))) # Pick first match.
      v <- v[v$species %in% species, ]
   }
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- 1:nrow(v)
      for (i in 1:length(args)) index <- intersect(index, which(v[,names(args)[i]] %in% args[[i]]))
      v <- v[index, ]
   }
   
   # Subset if 'scsset' object was given:
   if (!missing(x)) if ("scsset" %in% class(x)) v <- v[!is.na(match(v[key.scsset()], x[key.scsset()])), ]
   
   # Convert to 'scscat' object:
   v <- scscat(v)
   
   return(v)
}

#' @describeIn read Read southern Gulf of Saint Lawrence Northumberland Strait survey catch data.
#' @export read.nsscat
read.nsscat <- function(x, file, survey, species, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.nsscat(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.nsscat(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA
         
         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))
      
      v <- NULL
      # Read fixed-width file:
      if (ext %in% c("new", "txt")){
         v <- read.fortran(file = file, format = c('I1', 'A1', 'A3', 'I3', 'I3', 'I4', 'I2', 'I2', 'I3', 'I1', 'I2', 'I2', 'I0', 'I2', 'F4.1', 'F3.1', 
                                                   'F3.1', 'I3', 'I1', 'I4', 'I5', 'F4.0', 'F4.0', 'I4', 'I4', 'I3', 'I3', 'I3', 'I3', 'I3', 'I1', 'A1', 
                                                   'I6', 'A1', 'A12', 'A2', 'F4.0', 'I3', 'A4', 'A255'))
         
         names(v) <- c('card.type', 'vessel.code', 'cruise.number', 'stratum', 'set.number', 'year', 'month', 'day', 'unit.area', 'experiment', 
                       'start.hour', 'start.minute', 'start.second', 'duration', 'depth', 'bottom.temperature', 'bottom.salinity', 'light', 'bottom.type', 
                       'species', 'number.caught', 'weight.caught', 'number.basket', 'number.length', 'number.sex', 'number.maturity', 'number.weight', 
                       'number.otolith', 'number.parasite', 'number.stomach', 'weight.calculated', 'blank3', 'cfvn', 'blank4', 'expedition.number', 'extra', 
                       'weight.sampled', 'block.number', 'station.number', 'comment')
         
         # Remove blank spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[,j] <- gulf.utils::deblank(v[,j])
         
         # Remove blank columns:
         v <- v[, -grep("blank", names(v))]
         
         # Define cruise:
         v$cruise <- toupper(paste0(v$vessel.code, v$cruise.number))
         
         # Remove irrelevant fields:
         remove <- c("expedition.number", "card.type", "depth", "bottom.temperature", "start.hour", "start.minute", "start.second", "light", "btslide", "hydrostation",
                     "bottom.type", "bottom.salinity", "unit.area", "vessel.code", "duration", "cruise.number", "stratum", "cfvn", "block.number", "station.number", 
                     "number.length", "number.sex", "number.maturity", "number.weight", "number.otolith", "number.parasite", "number.stomach", "weight.calculated")
         v <- v[, !(names(v) %in% remove)]
         
         # Tow validity:
         v$valid <- as.numeric(v$experiment != 3)
         
         # Subtitute commas by semi-colons in comments:
         v$comment <- gsub(",", ";", v$comment)
         v$comment <- gulf.utils::deblank(v$comment)
      }
      
      # Read comma-delimited file:
      if (ext == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      
      # Compress date variables:
      if (all(c("year", "month", "day") %in% names(v))){
         v$date <- as.character(gulf.utils::date(v))
         v <- cbind(v[c("date")], v[setdiff(names(v), c("date", "year", "month", "day"))])
      }
   }
   
   # Subset by species:
   if (!missing(species)){
      if (is.character(species)) species <- unlist(lapply(species(species, drop = FALSE), function(x) return(x[1]))) # Pick first match.
      v <- v[v$species %in% species, ]
   }
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }
   
   # Convert to 'nsscat' object:
   v <- nsscat(v)
   
   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}
