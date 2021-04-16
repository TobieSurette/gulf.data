#' Read Probe Data.
#'
#' @description Functions to read probe data, such as depth/temperature or acoustic trawl monitoring data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.

#' @describeIn read.probe Read Minilog probe data.
#' @export read.minilog
read.minilog <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.minilog(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple netmind files and concatenate them:
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         temp <- read.minilog(file[i])
         information <- header(temp)
         for (j in 1:length(information)) temp[, names(information)[j]] <- information[[names(information)[j]]]
         
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
   y <- read.table(file = file, colClasses = "character", comment.char = "", sep = "\n",
                   blank.lines.skip = FALSE, fileEncoding = "Windows-1252")[[1]]
   y <- gulf.utils::deblank(y)
   k <- min(grep("^[0-9]", y)) - 1
   fields <- y[k] 
   x <- y[(k+1):length(y)]   
   y <- y[1:(k-1)]   
   
   # Parse header:
   str <- strsplit(y, ":")
   header <- gulf.utils::deblank(unlist(lapply(str, function(x) paste0(x[2:length(x)], collapse = ":"))))
   names(header) <- deblank(unlist(lapply(str, function(x) x[1])))
   
   # Extract file name:
   file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
   
   # Define variable names:
   fields <- strsplit(gsub("[()]", " ", fields), ",")[[1]]
   fields <- gsub(" +", " ", fields)
   units <- lapply(strsplit(fields, " "), function(x) x[2])
   units <- gsub("°", "degrees", units)
   fields <- tolower(unlist(lapply(strsplit(fields, " "), function(x) x[1])))
   names(units) <- fields
   
   # Read E-Sonar data:
   tmp <- data.frame(unlist(lapply(strsplit(x, ","), function(x) x[1])), stringsAsFactors = FALSE)
   for (i in 2:length(fields)) tmp <- cbind(tmp, data.frame(unlist(lapply(strsplit(x, ","), function(x) x[i])), stringsAsFactors = FALSE))  
   names(tmp) <- fields
   x <- tmp
   
   # Numeric conversion:
   for (i in 3:length(fields)) x[,i] <- as.numeric(x[,i])
   
   # Modify time by specified offset:
   if (offset != 0){
      tmp <- gulf.utils::time(x) + offset * 60
      x$date <- unlist(lapply(strsplit(as.character(tmp), " "), function(x) x[1]))
      x$time <- unlist(lapply(strsplit(as.character(tmp), " "), function(x) x[2]))
   }
   
   # Create 'minilog' object:
   x <- minilog(x, header = header, file.name = file.name, ...)
   
   # Attach units:
   gulf.metadata::units(x) <- units
   
   return(x)
}

#' @describeIn read.probe Read Star Oddi probe data.
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

#' @describeIn read.probe Read \strong{scanmar} trawl acoustic monitoring data.
#' @export read.scanmar
read.scanmar <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.scanmar(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         temp <- read.scanmar(file[i])
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
   
   # Create 'scanmar' object:
   v <- scanmar(v, header = header, tow.id = tow.id, file.name = file.name)
   
   return(v)
}

#' @describeIn read.probe Read \strong{eSonar} trawl acoustic monitoring data.
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

#' @describeIn read.probe \strong{Notus} trawl acoustic monitoring data.
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

