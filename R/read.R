#' Read \strong{gulf} data sets.
#' 
#' @description Functions to read Gulf of Saint-Lawrence data sets.
#' 
#' @param x Survey year or file name.
#' @param file File name(s). 
#' @param year Survey year(s).
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
#' 
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
read.scscat <- function(x, file, ...){
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
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }
   
   # Subset if 'scsset' object was given:
   if (!missing(x)) if ("scsset" %in% class(x)) v <- v[!is.na(match(v[key.scsset()], x[key.scsset()])), ]
   
   # Convert to 'scscat' object:
   v <- scscat(v)
   
   return(v)
}

#' @describeIn read Read Star Oddi probe data.
#' @export read.star.oddi
read.star.oddi <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.star.oddi(x, ...)
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.star.oddi(file[i])
          information <- header(temp)
          for (j in 1:length(information)){
             temp[, names(information)[j]] <- information[[names(information)[j]]]
          }

          if (!is.null(x)){
             # Create NA-valued columns if new variables appear:
             index <- setdiff(names(x), names(temp))
             if (length(index) > 0) temp[index] <- NA
             index <- setdiff(names(temp), names(x))
             if (length(index) > 0) x[index] <- NA

             # Order new file properly:
             temp <- temp[names(x)]
          }

          x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp

      return(x)
   }

   # Empty file:
   if (length(file) == 0) return(NULL)

   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n", 
                   blank.lines.skip = FALSE, fileEncoding = "Windows-1252")[[1]]
   y <- deblank(y)
   y <- gsub("\t", " ", y)
   index <- grep("^#", y)
   k <- max(index)
   y <- y[index]
   y <- gsub("^#[0-9]* ", "",y)
   
   # Parse header:
   str <- strsplit(y, ":")
   header <- deblank(unlist(lapply(str, function(x) x[2])))
   names(header) <- deblank(unlist(lapply(str, function(x) x[1])))
   
   # Extract file name:
   file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]

   # Define variable names:
   fields <- header[grep("Channel ", names(header))]
   fields <- unlist(lapply(strsplit(fields, " "), function(x) x[1]))

   # Read E-Sonar data:
   x <- read.table(file = file, header = FALSE, skip = k, dec = ",", sep = "\t",
                   colClasses = c("numeric", "character", rep("numeric", length(fields))))
   fields <- c("record", "date", fields)
   names(x) <- fields

   # Parse date fields:
   date <- data.frame(year = as.numeric(substr(x$date, 7, 8)),
                      month = as.numeric(substr(x$date, 4, 5)),
                      day = as.numeric(substr(x$date, 1, 2)),
                      stringsAsFactors = FALSE)

   x$time <- unlist(lapply(strsplit(x$date, "[ ,]"), function(x) x[2]))
   time <- data.frame(hour   = as.numeric(substr(x$time, 1, 2)),
                      minute = as.numeric(substr(x$time, 4, 5)),
                      second = as.numeric(substr(x$time, 7, 8)),
                      stringsAsFactors = FALSE)

   # Auto-correct date and time:
   index <- (date$year < 100)
   date$year[index] <- 2000 + date$year[index]
   index <- is.na(date$year)
   date$year[index] <- unique(date$year[!index])[1]
   index <- is.na(date$month)
   date$month[index] <- unique(date$month[!index])[1]
   index <- is.na(date$day)
   date$day[index] <- unique(date$day[!index])[1]

   # Create result variable:
   v <- cbind(x["record"], date, time, x[setdiff(names(x), c("date", "record"))])

   # Modify time by specified offset:
   if (offset != 0){
      t <- as.matrix(as.POSIXlt(time(v) + offset * 60))
      v$year   <- t[, "year"] + 1900
      v$month  <- t[, "mon"] + 1
      v$day    <- t[, "mday"]
      v$hour   <- t[, "hour"]
      v$minute <- t[, "min"]
      v$second <- t[, "sec"]
   }

   # Add tow ID to header:
   if (length(grep("GP[0-9][0-9][0-9]", file)) > 0){
      temp <- unlist(lapply(strsplit(file, "GP"), function(x) x[length(x)]))
      temp <- lapply(strsplit(temp, "/", fixed = TRUE), function(x) x[1])
      tow.id <- paste0("GP", unlist(temp))
      tow.id <- unlist(lapply(strsplit(tow.id, "[.]"), function(x) x[1]))
   }

   # Create 'star.oddi' object:
   v <- star.oddi(v, header = header, tow.id = tow.id, file.name = file.name, ...)
  
   # Define measurement units:
   index <- grep("[(]", names(v))
   vars <- names(v)[index]
   units <- strsplit(gsub("[)]", "", vars), "[(]")
   vars <- tolower(unlist(lapply(units, function(x) x[1])))
   units <- unlist(lapply(units, function(x) x[2]))
   units <- gsub("°", "degrees", units)
   str <- names(v)
   str[index] <- vars
   names(v) <- str
   names(units) <- vars
   gulf.metadata::units(v) <- units
   
   return(v)
}

#' @describeIn read Read \strong{eSonar} trawl acoustic monitoring data.
#' @export read.esonar
read.esonar <- function(x, file, offset = -3*60, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.esonar(x, ...)
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         temp <- read.esonar(file[i])
         information <- info(temp)[1:8]
         for (j in 1:length(information)){
            temp[, names(information)[j]] <- information[[names(information)[j]]]
         }

         if (!is.null(x)){
            # Create NA-valued columns if new variables appear:
            index <- setdiff(names(x), names(temp))
            if (length(index) > 0) temp[index] <- NA
            index <- setdiff(names(temp), names(x))
            if (length(index) > 0) x[index] <- NA

            # Order new file properly:
            temp <- temp[names(x)]
         }

         x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp

      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 20, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)

   # Define header information:
   header <- NULL
   header[gsub(" ", "", strsplit(y[1, ], ",")[[1]])] <- strsplit(y[2, ], ",")[[1]]
   header[gsub(" ", "", strsplit(y[3, ], ",")[[1]])] <- strsplit(y[4, ], ",")[[1]]
   header <- header[header != ""]
   file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
   tow.id <- toupper(lapply(strsplit(file.name, "[.]"), function(x) x[1])[[1]])

   # Define data field names:
   fields <- gsub(" ", "_", strsplit(y[5,], ",")[[1]]) # Split header fields and their values.

   # Read E-Sonar data:
   x <- read.table(file = file, header = FALSE, skip = 6, sep = ",", colClasses = "character")
   names(x) <- fields

   # Remove lines with no date fields:
   temp <- table(substr(x[, 1], 1, 3))
   x <- x[substr(x[, 1], 1, 3) == names(temp[temp == max(temp)]), ]

   # Parse date fields:
   date <- data.frame(year = as.numeric(paste("", substr(x$GPS_Date, 8, 11), sep = "")),
                      month = match(tolower(substr(x$GPS_Date, 4, 6)), substr(tolower(month.name), 1, 3)),
                      day = as.numeric(substr(x$GPS_Date, 1, 2)),
                      stringsAsFactors = FALSE)

   # Pad time with zeroes:
   index <- (nchar(x$GPS_Time) == 5)
   x$GPS_Time[index] <- paste("0", x$GPS_Time[index], sep = "")

   time <- data.frame(hour   = as.numeric(substr(x$GPS_Time, 1, 2)),
                      minute = as.numeric(substr(x$GPS_Time, 3, 4)),
                      second = as.numeric(substr(x$GPS_Time, 5, 6)),
                      stringsAsFactors = FALSE)

   # Auto-correct date and time:
   index <- (date$year < 100)
   date$year[index] <- 2000 + date$year[index]
   index <- is.na(date$year)
   date$year[index] <- unique(date$year[!index])[1]
   index <- is.na(date$month)
   date$month[index] <- unique(date$month[!index])[1]
   index <- is.na(date$day)
   date$day[index] <- unique(date$day[!index])[1]

   # Create result variable:
   v <- cbind(date, time)

   # Parse latitude and longitude:
   lon <- -(as.numeric(substr(x$Longitude, 1, 3)) + as.numeric(substr(x$Longitude, 4, 12)) / 60)
   lat <- as.numeric(substr(x$Latitude, 1, 2)) + as.numeric(substr(x$Latitude, 4, 12)) / 60
   v <- cbind(v, data.frame(longitude = lon, latitude = lat))

   # Parse speed variable:
   v$speed <- as.numeric(x$Speed)
   v$heading <- as.numeric(x$Heading)
   v$validity <- x$Validity
   v$transducer <- x$Transducer_Name
   v$sensor <- x$Sensor_Name
   v$value <- as.numeric(x$Sensor_Value)
   v$error.code <- x$Error_Code
   v$hydrophone <- x$Hydrophone
   v$signal.strength <- as.numeric(x$Signal_Strength)

   # Parse sensor values into separate columns:
   str <- unique(v$sensor)
   str <- sort(str[str != ""])
   for (i in 1:length(str)){
      v[tolower(str[i])] <- NA
      v[[tolower(str[i])]][v$sensor == str[i]] <- v$value[v$sensor == str[i]]
   }

   # Set NULL values to zero, and zeroes to NA:
   vars <- c("depth", "doormaster", "headline")
   v[setdiff(vars, names(v))] <- NA
   temp <- v[vars]
   temp[temp == 0] <- NA
   v[vars] <- temp

   # Remove repeating values:
   if (!repeats){
      for (i in 1:length(vars)){
         if (!all(is.na(v[, vars[i]]))){
            index <- which(diff(v[, vars[i]]) == 0)+1
            v[index, vars[i]] <- NA
         }
      }
   }

   # Modify time by specified offset:
   if (offset != 0){
      t <- as.character(time(v) + offset * 60)
      v$year   <- as.numeric(substr(t, 1, 4))
      v$month  <- as.numeric(substr(t, 6, 7))
      v$day    <- as.numeric(substr(t, 9, 10))
      v$hour   <- as.numeric(substr(t, 12, 13))
      v$minute <- as.numeric(substr(t, 15, 16))
      v$second <- as.numeric(substr(t, 18, 19))
   }

   # Remove records with missing time stamp:
   v <- v[!(is.na(v$hour) | is.na(v$minute) | is.na(v$second)), ]
   
   # Create 'esonar' object:
   v <- esonar(v, header = header, tow.id = tow.id, file.name = file.name)

   return(v)
}

#' @describeIn read \strong{Notus} trawl acoustic monitoring data.
#' @export read.notus
read.notus <- function(file){
  x <- readLines(file) # Read each line separately:
 
  # Parse variable field names:
  i <- min(grep("^Code", x))
  vars <- strsplit(x[i], ",")[[1]]
 
  # Extract date from header and reformat:
  date <- x[grep("Tow Date:", x)]
  date <- strsplit(strsplit(date, ": ")[[1]][2], " ")[[1]][1]
  str <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:length(str)) if (length(grep(str[i], date)) > 0) date <- gsub(str[i], ifelse(i<10, paste0("0", i), i), date)
  date <- strsplit(date, "-")[[1]]
  date <- paste0(date[3:1], collapse = "-")
 
  # Remove non-data lines:
  x <- x[grep("^[A-Z][0-9][0-9][A-Z]", x)]
 
  # Split using commas:
  y <- strsplit(x, ",")
 
  # Convert to data frame:
  for (i in 1:length(vars)){
    if (i == 1) data <- data.frame(unlist(lapply(y, function(x) x[i]))) else data <- cbind(data, data.frame(unlist(lapply(y, function(x) x[i]))))
  }
  names(data) <- vars
  data$Date <- date
  data <- cbind(data["Date"], data["Time"], data["Code"], data[setdiff(names(data), c("Date", "Time", "Code"))])
 
  # Convert factors to character strings:
  for (i in 1:ncol(data)) if (is.factor(data[, i])) data[, i] <- as.character(data[, i])
 
  # Convert numeric fields:
  for (i in 2:ncol(data)) data[, i] <- gsub("-", "", data[, i])
  for (i in 2:ncol(data)) if (!all(is.na(data[, i]))) if (all(gsub("[.0-9/-]", "", data[, i]) == ""))  data[, i] <- as.numeric(data[, i])
 
  return(data)
}

#' @describeIn read Read data from the Altantic Lobster Stock-recruitment Index. Larvae collectors are used to mainly provide recruitment indices for lobster and rock crab. 
#' @export read.alsi 
read.alsi <- function(year, site, species, type, ...){
   # Parse input arguments:
   if (!missing(type)){
      type <- pmatch(tolower(type), c("site", "collector", "biological"))
      type <- type[!is.na(type)]
   }else{
      type <- c("site", "collector", "biological")
   }

   # Set file loading flags:
   tab <- c(site = FALSE, col = FALSE, biological = FALSE)
   if (length(type) > 0) tab[type] <- TRUE else return(NULL)

   # Upload files:
   v <- list()
   if (tab["site"])       v$site       <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/site.csv", stringsAsFactors = FALSE)
   if (tab["collector"])  v$collector  <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/collector.csv", stringsAsFactors = FALSE)
   if (tab["biological"]) v$biological <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/biological.csv", stringsAsFactors = FALSE)

   # Data subsetting:
   if (!missing(year))    v <- lapply(v, function(x) x[x$year %in% year, ])
   if (!missing(site))    v <- lapply(v, function(x) x[x$site %in% site, ])
   if (!missing(species)) v <- lapply(v, function(x) x[x$species %in% species, ])

   return(v)
}
