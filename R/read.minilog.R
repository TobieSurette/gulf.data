#' Read Minilog Data
#'
#' @param x Minilog data file

read.minilog <- function(x, project, offset = 0, file, ...){
   # Define list of files to be read:
   if (missing(file)) file <- minilog.file.str(x, survey = survey, ...)

   # Read multiple minilog files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.minilog(file[i], ...)
          for (j in 1:length(header(temp))){
             temp[, names(header(temp))[j]] <- header(temp)[[names(header(temp))[j]]]
          }

          # Add columns if necessary:
          if ((length(setdiff(names(temp), names(x))) > 0) & (!is.null(x))){
             x[setdiff(names(temp), names(x))] <- NA
             x <- x[names(temp)]
          }

          # Append rows:
          x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp
      if ("Study.ID" %in% names(x)) x$Study.ID <- unlist(lapply(strsplit(x$Study.ID, " "), function(x) x[[1]]))

      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n")
   k <- sum(substr(y[,1], 1, 1) == "*") - 1 # Number of header lines.
   if (k <= 0){
      k <- sum(substr(y[,1], 1, 1) %in% c(letters, LETTERS))-1
      y[1:(k+1),1] <- paste("*", y[1:(k+1),1])
   }
   y <- substr(y[1:k, 1], 3, 100) # Eliminate leading characters.
   y <- gsub("\t", "", y)
   y <- strsplit(y, "([:][ ])|([=])") # Split header fields and their values.
   header <- list()
   fields <- NULL
   for (i in 1:k){
      fields[i] <- gsub(" ", ".", y[[i]][1])
      header[[i]] <- y[[i]][2]
   }
   str <- strsplit(file, "/")[[1]]
   header <- c(header, str[length(str)])
   names(header) <- c(fields, "file.name")

   # Read field variable names:
   fields <- read.table(file = file, header = FALSE, skip = k, nrows = 1, sep = ",", colClasses = "character")
   fields <- t(fields)[, 1]
   fields <- unlist(strsplit(fields, "\t"))
   fields <- gsub("* ", "", fields, fixed = TRUE)

   # Determine date column and format:
   date.col <- grep("Date", fields)
   date.format <- substr(fields[date.col],
                         regexpr("(", fields[date.col], fixed = TRUE)[1] + 1,
                         regexpr(")", fields[date.col], fixed = TRUE)[1] - 1)

   # Determine time column:
   time.col <- grep("Time", fields)

   # Read minilog data:
   x <- read.table(file = file, header = TRUE, skip = k, sep = ",", colClasses = "character")
   if (ncol(x) == 1) x <- read.table(file = file, header = TRUE, skip = k, sep = "\t", colClasses = "character")

   # Separate date and time fields:
   if (length(c(grep("Date", fields[1]), grep("Time", fields[1]))) == 2){
      fields <- c(strsplit(fields[1], " ", fixed = TRUE), fields[2:length(fields)])
      fields <- unlist(fields)
      names(fields) <- NULL

      temp <- unlist(strsplit(x[,1], " |\t"))
      temp <- data.frame(date = temp[seq(1, length(temp), by = 2)],
                         time = temp[seq(2, length(temp), by = 2)], stringsAsFactors = FALSE)
      x <- cbind(temp, x[, 2:dim(x)[2]])
      time.col <- 2
   }

   # Parse date fields:
   date <- NULL
   sep <- unique(strsplit(gsub("[0-9]", "", x[1, date.col]), "")[[1]])
   if (nchar(strsplit(x[1, date.col], sep)[[1]][3]) == 4) date.format <- "dd-mm-yyyy"

   if (length(date.format) > 0){
      if (date.format == "dd-mm-yyyy") ii <- c(3, 2, 1)
      if (date.format == "mm-dd-yyyy") ii <- c(3, 1, 2)
      if (date.format == "yyyy-mm-dd") ii <- c(1, 2, 3)
      date <- data.frame(year = as.numeric(unlist(lapply(strsplit(x[, date.col], sep), function(x) x[ii[1]]))),
                         month = as.numeric(unlist(lapply(strsplit(x[, date.col], sep), function(x) x[ii[2]]))),
                         day = as.numeric(unlist(lapply(strsplit(x[, date.col], sep), function(x) x[ii[3]]))),
                         stringsAsFactors = FALSE)
   }
   if (is.null(date)) stop(paste("Unrecognized data format:", date.format))

   # Parse time field:
   sep <- unique(strsplit(gsub("[0-9]", "", x[1, time.col]), "")[[1]])
   time <- data.frame(hour = as.numeric(unlist(lapply(strsplit(x[, time.col], sep), function(x) x[1]))),
                      minute = as.numeric(unlist(lapply(strsplit(x[, time.col], sep), function(x) x[2]))),
                      second = as.numeric(unlist(lapply(strsplit(x[, time.col], sep), function(x) x[3]))),
                      stringsAsFactors = FALSE)

   # Create column index for measured data:
   index <- setdiff(1:length(fields), c(time.col, date.col))

   # Convert data to numeric format:
   for (i in 1:length(index)) x[, index[i]] <- as.numeric(as.character(x[, index[i]]))

   # Create data frame containing minilog data:
   x <- cbind(date, time, x[, index])

   # Convert data field names to proper format:
   fields <- fields[index]
   fields <- unlist(lapply(strsplit(as.character(fields), "(", fixed = TRUE), function(x) x[[1]][1]))
   fields <- gsub(" ", "", fields)

   # Assign column names:
   names(x) <- c(names(x)[1:6], tolower(fields))
   str <- names(header)
   str[str == "Study.ID"] <- "tow.id"
   names(header) <- str

   # Define survey string:
   header$survey <- survey
   if ("tow.id" %in% names(header)){
      temp <- grep("[G][P]*[0-9][0-9][0-9]*", header$tow.id)
      if (length(temp) > 0) header$survey <- "sc"
   }else{
      if ("Study.Description" %in% names(header)){
         if (length(grep("^GP", header$Study.Description)) > 0) header$tow.id <- header$Study.Description
      }
   }

   # Substitute 'meters' and 'celsius' by 'depth' and 'temperature':
   str <- names(x)
   str[str == "meters"] <- "depth"
   str[str == "celsius"] <- "temperature"
   names(x) <- str

   # Modify time by specified offset:
   if (offset != 0){
      t <- as.matrix(as.POSIXlt(time(x) + offset * 60))
      x$year   <- t[, "year"] + 1900
      x$month  <- t[, "mon"] + 1
      x$day    <- t[, "mday"]
      x$hour   <- t[, "hour"]
      x$minute <- t[, "min"]
      x$second <- t[, "sec"]
   }

   # Include 'tow.id' as a field:
   x$tow.id <- header$tow.id

   # Convert to minilog object:
   x <- minilog(x, header)

   return(x)
}
