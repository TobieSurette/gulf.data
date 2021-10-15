#' Read eSonar Data.
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

#' @export read.esonar
read.esonar <- function(x, ...) UseMethod("read.esonar")

#' @describeIn read.esonar Read \strong{eSonar} trawl acoustic monitoring data.
#' @rawNamespace S3method(read.esonar,default)
read.esonar.default <- function(x, file, offset = -3*60, repeats = FALSE, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)){
      if (missing(x)) file <- locate.esonar(...) else file <- locate.esonar(x, ...)  
   }
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- vector(mode = "list", length = length(file))
      k <- 0
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         x[i] <- list(expand(read.esonar(file[i])))
         k <- k + nrow(x[[i]])
      }

      # Standardize data frame formats:
      vars <- unique(unlist(lapply(x, names)))
      for (i in 1:length(x)){
         ix <- setdiff(vars, names(x[[i]]))
         if (length(ix) > 0){
            x[[i]][ix] <- ""
            x[[i]] <- x[[i]][vars]
         }
      }

      # Efficiently catenate data frames:
      while (length(x) >= 2){
         ix <- seq(2, length(x), by = 2)
         for (i in ix) x[i] <- list(rbind(x[[i-1]], x[[i]]))
         if (i < length(x)) ix <- c(ix, length(x))
         x <- x[ix]
      }
      x <- x[[1]]
      
      gulf.metadata::header(x) <- NULL
      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 20, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)

   # Define header information:
   header <- header.esonar(file)

   # Define data field names:
   k <- max(grep("CPU", y[, 1]))
   if (length(k) == 0) k <- 5
   fields <- gsub(" ", "_", strsplit(y[k,], ",")[[1]]) # Split header fields and their values.

   # Read file:
   x <- read.table(file = file, header = FALSE, skip = k, sep = ",", colClasses = "character")
   names(x) <- fields

   # Remove lines with no date fields:
   temp <- table(substr(x[, 1], 1, 3))
   x <- x[substr(x[, 1], 1, 3) == names(temp[temp == max(temp)]), ]

   # Parse date fields:
   date <- data.frame(year = as.numeric(paste0("", substr(x$GPS_Date, 8, 11))),
                      month = match(tolower(substr(x$GPS_Date, 4, 6)), substr(tolower(month.name), 1, 3)),
                      day = as.numeric(substr(x$GPS_Date, 1, 2)),
                      stringsAsFactors = FALSE)

   # Pad time with zeroes:
   ix <- (nchar(x$GPS_Time) == 5)
   x$GPS_Time[ix] <- paste0("0", x$GPS_Time[ix])
   time <- data.frame(hour   = as.numeric(substr(x$GPS_Time, 1, 2)),
                      minute = as.numeric(substr(x$GPS_Time, 3, 4)),
                      second = as.numeric(substr(x$GPS_Time, 5, 6)),
                      stringsAsFactors = FALSE)

   # Auto-correct date and time:
   ix <- (date$year < 100)
   date$year[ix] <- 2000 + date$year[ix]
   ix <- is.na(date$year)
   date$year[ix] <- unique(date$year[!ix])[1]
   index <- is.na(date$month)
   date$month[ix] <- unique(date$month[!ix])[1]
   ix <- is.na(date$day)
   date$day[ix] <- unique(date$day[!ix])[1]

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
   if (length(str) > 0){
      for (i in 1:length(str)){
         v[tolower(str[i])] <- NA
         v[[tolower(str[i])]][v$sensor == str[i]] <- v$value[v$sensor == str[i]]
      }
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
            ix <- which(diff(v[, vars[i]]) == 0)+1
            v[ix, vars[i]] <- NA
         }
      }
   }

   # Modify time by specified offset:
   if (offset != 0){
      t <- time(v) +  offset * 60
      v$date <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[1]))
      v$time <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[2]))
   }

   # Remove records with missing time stamp:
   v <- v[!(is.na(v$hour) | is.na(v$minute) | is.na(v$second)), ]

   # Create 'esonar' object:
   v <- esonar(v)
   header(v) <- header

   return(v)
}
