#' Read Northumberland Strait Survey Data
#' 
#' @aliases read.nss
#' 
#' @description Functions to Northumberland Strait survey  data sets.
#' 
#' @param x Survey year or file name.
#' @param file File name(s). 
#' @param year Survey year(s).
#' @param species Species code or name.
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.
#' 
#' @examples  
#' # Read Northumberland Strait survey set data files:
#' x <- read.nssset()                 # Read all available data.
#' x <- read.nssset(year = 2019)      # Read single year.
#' x <- read.nssset(year = 2010:2015) # Read range of years.
#' 
#' # Read Northumberland Strait survey biological data files:
#' x <- read.nssbio(2020)  
#' x <- read.scsset(2020, species = "lobster")
#' 
#' @seealso \code{\link[gulf.data]{nss}}
#' @seealso \code{\link[gulf.data]{read.scs}}

#' @describeIn read.nss Read southern Gulf of Saint Lawrence Northumberland Strait survey set data.
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

#' @describeIn read.nss Read southern Gulf of Saint Lawrence Northumberland Strait survey catch data.
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

#' @describeIn read.nss Read southern Gulf of Saint Lawrence Northumberland Strait survey biological data.
#' @export read.nssbio
read.nssbio <- function(x, file, survey, species, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.nssbio(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.nssbio(file = file[i], ...)
         
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
         v <- read.fortran(file = file, format = c('I1', 'A1', 'A3', 'I3', 'I3', 'I4', 'I2', 'I2', 'I3', 'I1', 'I1', 'I4', 'I1', 'I4', 'I3', 
                                                   'I1', 'I1', 'F5.0', 'I1', 'F5.1', 'I2', 'I2', 'F4.0', 'I1', 'I2', 'I1', 'A3', 'I2', 'I2', 
                                                   'A1', 'I3', 'A6', 'A1', 'A12', 'A255', 'I1', 'I1', 'F5.2', 'F5.2', 'I3', 'A4', 'A10', 'I3', 
                                                   'I1', 'I10', 'A4', 'I4'))
         
         names(v) <- c('card.type', 'vessel.code', 'cruise.number', 'stratum', 'set.number', 'year', 'month', 'day', 
                       'unit.area', 'experiment', 'bottom.type', 'species', 'record.number', 'fish.number', 'length', 
                       'sex', 'maturity', 'weight', 'stomach.type', 'stomach.weight', 'stomach.full', 'stomach.part', 
                       'weight.gonad', 'age.material', 'annuli', 'edge.type', 'age.check', 'age', 'year.class', 'ager', 
                       'parasite', 'field.definition', 'blank3', 'expedition.number', 'comment', 'shell.condition', 
                       'egg.condition', 'chela', 'abdomen', 'block.number', 'station.number', 'missing.legs', 'disc.width',
                       'bobtail', 'specimen', 'photo.id', 'gonad.vial.number')
         
         # Remove blank spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[,j] <- gulf.utils::deblank(v[,j])
         
         # Remove blank columns:
         v <- v[, -grep("blank", names(v))]
         
         # Define cruise:
         v$cruise <- toupper(paste0(v$vessel.code, v$cruise.number))
         
         # Create data field:
         v$date <- as.character(gulf.utils::date(year = v$year, month = v$month, day = v$day))
         
         # Remove empty columns:
         v <- squeeze(v)
         
         # Remove irrelevant fields:
         remove <- c("year", "month", "day", "unit.area", "stratum", "card.type", "vessel.code", "record.number", 
                     "experiment", "station.number", "cruise.number", "bottom.type", "block.number", "missing.legs")
         v <- v[, !(names(v) %in% remove)]
         
         # Weight fixes:
         if (!("weight" %in% names(v))) v$weight <- NA
         v$weight[v$weight == 0] <- NA
         
         # Subtitute commas by semi-colons in comments:
         if (!("comment" %in% names(v))) v$comment <- ""
         v$comment <- gsub(",", ";", v$comment)
         v$comment <- gulf.utils::deblank(v$comment)
         
         # Specimen number fix:
         if (!("specimen" %in% names(v)) & ("fish.number" %in% names(v))) v$specimen <- v$fish.number 
         if (!("specimen" %in% names(v))){
            vars <- c("date", "cruise", "set.number",  "species")
            ix <- match(v[vars], unique(v[vars]))
            for (i in 1:max(ix)) v$specimen[ix == i] <- 1:sum(ix == i)
         }
         
         # Sort data:
         vars <- c("date", "cruise", "set.number",  "species", "specimen")
         if (!any(is.na(v[vars]))){
            v <- sort(v, by = vars)
            rownames(v) <- NULL
         }
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
   
   # Convert to 'nssbio' object:
   v <- nssbio(v)
   
   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}

#' @describeIn read.nss Read southern Gulf of Saint Lawrence Northumberland Strait survey length data.
#' @export read.nsslen
read.nsslen <- function(x, file, survey, species, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.nsslen(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         # Append data:
         tmp <- read.nsslen(file = file[i], ...)
         
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
         v <- read.fortran(file = file, format = c('I1', 'A1', 'A3', 'I3', 'I3', 'I4', 'I2', 'I2', 'I3', 'I1', 'I4', 'I4', 
                                                   'A1', 'F9.9', 'I1', 'I1', 'I1', 'I1', 'I1', 'I3', 'I3', 'I3', 'I3', 'I3', 'I3',
                                                   'I3', 'I3', 'I3', 'I3', 'I3', 'I3', 'I3', 'I3', 'I3', 'A3', 'I6', 'A1', 'A12', 
                                                   'I3', 'A4'))
         
         names(v) <- c('card.type', 'vessel.code', 'cruise.number', 'stratum', 'set.number', 'year', 'month', 'day', 'unit.area',
                       'experiment', 'species', 'number.length', 'ratio.decimal', 'ratio', 'sex', 'length.interval', 'length.unit',
                       'record.number', 'group', 'start.length', 'freq0', 'freq1', 'freq2', 'freq3', 'freq4', 'freq5', 'freq6', 
                       'freq7', 'freq8', 'freq9', 'freq10', 'freq11', 'freq12', 'freq13', 'blank3', 'cfvn', 'blank4', 
                       'expedition.number', 'block.number', 'station.number')
         
         # Remove blank spaces:
         for (j in 1:ncol(v)) if (is.character(v[, j])) v[,j] <- gulf.utils::deblank(v[,j])
         
         # Remove blank columns:
         v <- v[, -grep("blank", names(v))]
         
         # Define cruise:
         v$cruise <- toupper(paste0(v$vessel.code, v$cruise.number))
         v$size.class <- v$group
         
         v$length.unit <- c("cm", "mm")[v$length.unit]
         v$length.interval <- 1 / v$length.interval
         
         # Create data field:
         v$date <- as.character(gulf.utils::date(year = v$year, month = v$month, day = v$day))
         
         # Remove irrelevant fields:
         remove <- c("year", "month", "day", "expedition.number", "card.type", "depth", "bottom.temperature", "start.hour", "start.minute", "start.second", "light", "btslide", "hydrostation",
                     "record.number", "group", "bottom.type", "number.length", "bottom.salinity", "unit.area", "vessel.code", "duration", "cruise.number", "stratum", "cfvn", "block.number", "station.number", 
                     "ratio.decimal", "number.length", "number.sex", "number.maturity", "number.weight", "number.otolith", "number.parasite", "number.stomach", "weight.calculated")
         v <- v[, !(names(v) %in% remove)]
         
         # Sampling ratio fix:
         v$ratio[v$ratio == 0] <- 1
         
         # Subtitute commas by semi-colons in comments:
         if (!("comment" %in% names(v))) v$comment <- ""
         v$comment <- gsub(",", ";", v$comment)
         v$comment <- gulf.utils::deblank(v$comment)
         
         # Length-frequency categories:
         fvars <- names(v)[grep("^freq", names(v))]
         
         # Define frequencies:
         f <- round(v[, fvars])
         f[is.na(f)] <- 0 
         fvars <- fvars[apply(f, 2, sum) > 0]
         f <- f[, fvars]
         f <- as.vector(as.matrix(f)) #Linearize
         
         # Define length matrix:
         l <- gulf.utils::repvec(v$start.length, ncol = length(fvars) ) + repvec(0:(length(fvars) -1), nrow = nrow(v)) * repvec(v$length.interval, ncol = length(fvars) )
         
         # Create data frame of biological card variables:
         fields <- names(v)[-grep("^freq", names(v))]
         tmp <- data.frame(set.number = rep(as.vector(repvec(v[, "set.number"], ncol = length(fvars))), f))
         for (i in 1:length(fields)) tmp[, fields[i]] <- rep(as.vector(repvec(v[, fields[i]], ncol = length(fvars))), f)
         tmp$length <- rep(as.vector(l), f)
         
         # Re-order variables:
         vars <- c("date", "cruise", "set.number", "experiment", "species", "size.class", "sex", "ratio", "length") 
         v <- tmp[c(vars, setdiff(names(tmp), c(vars, "length.interval", "start.length")))]
         
         # Sort data:
         if (!any(is.na(v[vars]))){
            v <- sort(v, by = vars)
            rownames(v) <- NULL
         }
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
   
   # Convert to 'nsslen' object:
   v <- nsslen(v)
   
   # Subset by survey type:
   if (!missing(survey)) v <- v[survey(v) %in% survey, ]
   
   return(v)
}
