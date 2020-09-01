#' Minilog Data
#'
#' @description Functions to create, read, write, manipulate and analyze Minilog depth/temperature 
#'              probe data.
#' 
#' @param x Minilog data file
#' @param year Numeric value specifying the survey or project year(s).
#' @param survey,project Character string specifying the survey or project name. 
#'    The survey may refer to the September multi-species survey (\code{survey = "rv"},
#'    \code{survey = "sep"} or \code{survey = "September"}), the  Northumberland Strait 
#'    survey (\code{= "ns"}), the mobile Sentinel survey  \code{= "sentinel"}),
#'    or the snow crab survey (\code{= "sc"} or \code{= "snow crab"}). 
#' @param set.number,tow.id Numeric value or strings specifying the survey set number or tow ID.
#' @param set.card Data frame containing the survey tows which specify the Minilog data to be loaded.
#' @param path Logical value specifying whether to include the path in the Minilog files.
#' 
#' @examples 
#' # Minilog files for the 2000 September RV survey:
#' minilog.file(survey = "rv", year = 2000)
#'
#' # Use a set card extract for the first ten sets of 2012:
#' x <- read.gulf(year = 2012, survey = "sc")
#' minilog.file(x[1:10, ])
#' 
#' # Use a tow ID to extract file names for the snow crab survey 2006-2012:
#' minilog.file("GP001", year = 2006:2012)
#' 
#' x <- read.minilog(tow.id = "GP001F", year = 2010)
#' x <- read.minilog(survey = "rv", year = 2010)
#' 
#' # Plot Minilog data:
#' file <- system.file("extdata", "Minilog example.txt", package = "gulf.data")
#' x <- read.minilog(file)
#' plot(x)
#' 
#' @section Functions:
#' \describe{
#'    \item{\code{minilog}}{Generic \code{minilog} method.}
#'    \item{\code{minilog.default}}{Default minilog method}
#'    \item{\code{minilog.data.frame}}{Convert data frame to minilog object.}
#'    \item{\code{path.minilog}}{Find a minilog data path.}
#'    \item{\code{file.minilog}}{Find a minilog data file.}
#'    \item{\code{read.minilog}}{Read Minilog data. }
#'    \item{\code{write.minilog}}{Write a \code{minilog} object to a file.}
#'    \item{\code{plot.minilog}}{Plot minilog data object.}
#'    \item{\code{summary.minilog}}{Minilog data object summary.}
#'    \item{\code{split.minilog}}{Split minilog data object into disjoint subsets.}
#'    \item{\code{truncate.minilog}}{Truncate minilog data object.}
#'    \item{\code{info.minilog}}{Minilog data object information.}
#' }  
#' 

#' @export
minilog <- function(x, ...) UseMethod("minilog")

#' @export
minilog.default <- function(x, ...) if ("minilog" %in% class(x)) return(x) else stop("Not a 'minilog' object.")

#' @export
minilog.data.frame <- function(x, header, ...){
   # Remove non-minilog variables:
   names(x) <- tolower(names(x))
   vars <- c("date", "time", "temperature", "depth")
   x <- x[, intersect(vars, names(x))]

   # Check that date and time fields is defined:
   if (!all(c("date", "time") %in% names(x))) stop("'date' and 'time' fields must be defined.")
   
   # Format 'header':
   if (!missing(header)){
      if (!is.list(header)) stop("'header' must be a list.")
      attributes(x) <- c(attributes(x), list(header = header))
   }
   
   # Index key:
   key(x) <- c("date", "time")
   
   # Minilog class identifier:
   class(x) <- union("minilog", class(x))

   # Attach metadata attributes:
   if ("temperature" %in% names(x)) units(x, "temperature") <- "degCelsius"
   if ("depth" %in% names(x)) units(x, "depth") <- "meter"
   
   return(x)
}

#' @export path.minilog
path.minilog <- function(year, project, survey, ...){
   # Check input arguments:
   if (missing(project) & missing(survey)) stop("Specify 'project' or 'survey'.")
   if (!missing(project) & !missing(survey)) stop("Specify 'project' or 'survey', but not both.")
   
   # Parse 'survey' argument:
   if (!missing(survey)) project <- survey

   # Parse project string:
   project <- gsub("[. _-]", "", tolower(project), fixed = TRUE)
   project <- match.arg(survey, c("azmp", "collector", "alsi", "rvs", "september", "nss", "northumberlandstrait", "scs", "snowcrab"))
   project <- gsub("collector", "alsi", project)
   project <- gsub("september", "rvs", project)
   project <- gsub("snowcrabs", "scs", project)
   project <- gsub("northumberlandstrait", "nss", project)
   if (length(project) == 0) stop("'project' not defined.")

   
   # Turn off warnings:
   warning <- options("warn")[[1]]  
   options(warn = -1)
   
   # Atlantic Lobster Settlement Index project:
   if (project == "alsi"){
      
   }
   
   # September and NS surveys:
   if (project %in% c("rvs", "nss")){
      if (missing(year)){
         command <- paste('dir/ad/b "', str, '"\\????', sep = "")
         res <- shell(command, intern = TRUE)
         str <- paste(str, res, "/", sep = "")
      }else{
         str <- paste(str, year, "/",  sep = "")
      }
   }
   
   # Snow crab survey:
   if (project == "scs"){
      # Append year to path:
      if (missing(year)) str <- paste0(str, list.files(path = str, pattern = "^Fishing Year [0-9]+$")) else str <- paste(str, "Fishing Year ", year, "/", sep = "")   
      str <- paste0(str, "/Trawl Data/South Western Gulf/Minilog/ASCII")
      str <- str[file.exists(str)]
   
      # Remove irrelevant errors:
      index <- which(str == "File Not Found")
      index <- grep("File Not Found", str)
      if (length(index) > 0) str <- str[-index]
   }

   # Restore warnings:
   options(warn = warning)
   
   # Remove pure path entries:
   index <- which(substr(str, nchar(str), nchar(str)) != "/")
   if (length(index) > 0) str[index] <- paste0(str[index], "/")
     
   return(str)
}

#' @export file.minilog
file.minilog <- function(x, year, set.number, tow.id, set.card, path = TRUE, survey = "sc", project, ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         index <- file.exists(x)
         if (any(index)) return(x[index])
         if ((length(grep(".", x, fixed = TRUE)) > 0) | (length(grep("/", x, fixed = TRUE)) > 0)) return(x)
         tow.id <- x 
      }
      if ("gulf.set" %in% class(x)) set.card <- x
   }
  
   # If 'file' is a set card, define proper parameters:
   if (!missing(set.card)){
      tow.id <- set.card$tow.id
      tow.id <- tow.id[!is.na(tow.id)]
      year   <- unique(set.card$year)   
   }  
   
   # Set 'survey' to snow crab survey if tow ID is specified:
   if (!missing(tow.id)) survey <- "sc"
   
   # Get minilog path directories:
   if (!missing(year)){
      path.str <- minilog.path.str(survey = survey, year = year, ...)
   }else{
      path.str <- minilog.path.str(survey = survey, ...)
   }
 
   # Parse 'survey' argument:
   survey <- tolower(survey)
   survey <- gsub(" ", "", survey, fixed = TRUE)
   survey <- gsub(".", "", survey, fixed = TRUE)
   survey <- match.arg(survey, c("rv", "september", "ns", "northumberlandstrait", "sc", "snowcrab"))

   # Define file string:
   if (survey %in% c("ns", "northumberlandstrait")){
      str <- shell(paste('dir/b "', path.str, '"\\*.txt', sep = ""), intern = TRUE)
   }
   if (survey %in% c("sc", "snowcrab", "rv", "september")){
      command <- paste('dir/b "', path.str, '"\\Asc-*', sep = "")
      str <- NULL
      for (i in 1:length(path.str)){
         str <- c(str, list.files(path = path.str[i], pattern = "*.csv", recursive = TRUE, full.names = path, ignore.case = TRUE))
         str <- c(str, list.files(path = path.str[i], pattern = "Asc-*", recursive = TRUE, full.names = path, ignore.case = TRUE))
      }   
   }
               
   # Remove invalid results:
   str <- str[setdiff(1:length(str), grep("File Not Found", str))]
   str <- str[setdiff(1:length(str), grep("deleted", tolower(str)))]
   
   # Extract subset using tow ID:
   if (!missing(tow.id)){
      if (year <= 2016){
         index <- rep(FALSE, length(str))
         for (i in 1:length(str)){
            tmp <- readLines(str[i], n = 10)
            tmp <- tmp[grep("GP[0-9][0-9][0-9]", tmp)]
            tmp <- strsplit(tmp, "GP")[[1]]
            tmp <- tmp[length(tmp)]
            tmp <- paste0("GP", tmp)
            tmp <- toupper(gsub(" +$", "", tmp))
            if (tmp %in% tow.id) index[i] <- TRUE
         }
         if (!any(index)) str <- NULL else str <- str[index] 
      }else{
         temp <- unlist(lapply(strsplit(str, "_", fixed = TRUE), function(x) x[length(x)]))
         temp <- toupper(gsub(".csv", "", tolower(temp)))
         index <- which(temp %in% tow.id)
         if (length(index) == 0) str <- NULL else str <- str[index]
      }
   }
   
   # Remove path:
   if (!path) str <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
   
   return(str)
}

#' @export read.minilog
read.minilog <- function(x, project, offset = 0, file, ...){
   # Define list of files to be read:
   if (missing(file)){
      if (is.character(x)){
         if (any(file.exists(x))) file <- x
      }else{
         file <- minilog.file(x, survey = survey, ...)
      }
   }
 
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
   header <- header.minilog(file)

   # Read field variable names:
   fields <- read.table(file = file, header = FALSE, nrows = 20, 
                        sep = "\n", colClasses = "character", fileEncoding = "Windows-1252")
   k <- grep("^Date", fields[, 1])
   fields <- fields[k, 1]
   fields <- unlist(strsplit(fields, ","))
   fields <- gsub(" ", "", fields, fixed = TRUE)
   
   # Determine date column and format:
   date.col <- grep("Date", fields)
   date.format <- substr(fields[date.col],
                         regexpr("(", fields[date.col], fixed = TRUE)[1] + 1,
                         regexpr(")", fields[date.col], fixed = TRUE)[1] - 1)

   # Determine time column:
   time.col <- grep("Time", fields)

   # Read minilog data:
   x <- read.table(file = file, header = TRUE, skip = k-1, sep = ",", colClasses = "character", fileEncoding = "Windows-1252")
   if (ncol(x) == 1) x <- read.table(file = file, header = TRUE, skip = k, sep = "\t", colClasses = "character", fileEncoding = "Windows-1252")

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
   fields <- tolower(gsub(" ", "", fields))

   # Assign column names:
   names(x) <- c(names(x)[1:6], fields)
   str <- colnames(header)
   str[str == "Study.ID"] <- "tow.id"
   names(header) <- str

   # Extract snow crab survey tow ID:
   if ("Study.Description" %in% names(header)){
      if (length(grep("^GP", header$Study.Description)) > 0) header$tow.id <- header$Study.Description
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

   x$date <- as.character(gulf.utils::date(x))
   x$time <- unlist(lapply(strsplit(as.character(time(x)), " "), function(x) x[2]))
   x <- x[c("date", "time", setdiff(names(x), c("date", "time", "year", "month", "day", "hour", "minute", "second")))]
   
   # Convert to minilog object:
   x <- minilog(x, header)

   return(x)
}

#' @export write.minilog
write.minilog <- function(x, file = NULL){
   # Write header information to file:
   info <- header(x)
   
   for (i in 1:length(info)){
      str <- paste("* ", gsub(".", " ", names(info[i]), fixed = TRUE), "=", info[[i]], sep = "")
      write.table(str, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
   }
   fields <- names(x)
   fields <- fields[7:length(fields)]
   fields <- c("Time(hh:mm:ss)", fields)
   fields <- c("Date(yyyy-mm-dd)", fields)
   
   # Add 'Celsius' column:
   col <- grep("celsius", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), " (ºC)", sep = "")
   }
   
   # Add 'Temp' column:
   col <- grep("temp", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), "(AtoD)", sep = "")
   }    
    
   # Add 'Meters' column:
   col <- grep("meters", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), " (m)", sep = "")
   }    
   
   # Add 'Depth' column:
   col <- grep("depth", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), "(AtoD)", sep = "")
   }    
   
   # Write header line for file:
   fields <- do.call(paste, c(as.list(fields), sep = ","))
   fields <- paste("*", fields) 
   write.table(fields, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
   
   # Create data frame to be written to file:
   temp <- data.frame(date = as.character(date(x)), 
                      time = substr(as.character(time(x)), 12, 20))
   temp <- cbind(temp, x[, 7:dim(x)[2]])
   
   # Write minilog data to file:
   write.table(temp, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
}

#' @export
plot.minilog <- function(x, col, lwd = 1, ...){
   # Define default colours:
   if (missing(col)){
      cols <- "black"
      if (all(c("temperature", "depth") %in% names(x))) cols <- c("blue", "chartreuse3")
   }
   
   # Plot temperature:
   ylim <- c(0, 1.15 * max(x$temperature))
   plot(time(x), x$temperature, xaxs = "i", yaxs = "i",
        ylim = ylim, lwd = lwd, col = cols[1], yaxt = "n", xlab = "", ylab = "", type = "l", ...)
   
   axis(2, col = cols[1], col.axis = cols[1])
   mtext("Time", 1, 2, col = cols[1])
   mtext("Temperature(C)", 2, 2, col = cols[1])
      
   # Plot depth:
   if ("depth" %in% names(x)){
      r <- range(x$depth)
      r <- c(r[1] - 0.25, r[2] + 0.4)
      par(usr = c(par("usr")[1:2], r))
      lines(time(x), x$depth, lwd = lwd, col = cols[2], ...)
      axis(4, col = cols[2], col.axis = cols[2])
      mtext("Depth", 4, 2, col = cols[2])
   }
}

#' @export 
summary.minilog <- function(x, truncate = TRUE, round = TRUE, ...){
   # SUMMARY.MINILOG - Returns summary statistics of a 'minilog' object.
   
   # Read tow data:
   y <- read.scset(year = unique(x$year), print = FALSE)
  
   # Trim data:
   if (truncate) x <- truncate(x, ...)
   
   # Grouping variables:
   vars <- c("year", "month", "day", "tow.id")
     
   # Calculate maximal depth:
   res <- aggregate(data.frame(depth = x$depth), by = x[vars], mean, na.rm = TRUE)
   res$bottom.temperature <- NA
   
   # Index of data observations during trawling:
   x$time <- time(x)
   ii <- match(x[c("year", "tow.id")], y[c("year", "tow.id")])
   index <- which((x$time >= start.time(y)[ii]) & (x$time <= end.time(y)[ii]))
   x$depth.trawling <- NA
   x$depth.trawling[index] <- x$depth[index] 
   x$temperature.trawling <- NA
   x$temperature.trawling[index] <- x$temperature[index]
  
   # Calculate mean depth and temperature:
   ii <- match(res[c("year", "tow.id")], y[c("year", "tow.id")])
   res$depth <- aggregate(data.frame(depth = x$depth.trawling), by = x[vars], mean, na.rm = TRUE)[, "depth"]
   res$depth.logbook   <- y$depth[ii]
   res$bottom.temperature <- aggregate(data.frame(bottom.temperature = x$temperature.trawling), by = x[vars], mean, na.rm = TRUE)[, "bottom.temperature"]
   res$start.time <- start.time(y[ii,], ...)
   res$end.time   <- end.time(y[ii,], ...)
 
   # Function to extract sampling resolution for depth and temperature:                    
   interval <- function(x){
      f <- table(diff(sort(unique(x))))
      d <- as.numeric(names(f))
      return(d[which.max(f)])
   }
   res <- cbind(res, aggregate(data.frame(depth.delta = x$depth), by = x[vars], interval)["depth.delta"])
   res <- cbind(res, aggregate(data.frame(temperature.delta = x$temperature), by = x[vars], interval)["temperature.delta"])
         
   # Determine approximate median bottom time:
   res$start.time.minilog <- aggregate(data.frame(start.time = x$time), by = x[vars], min)[, "start.time"]
   res$end.time.minilog <- aggregate(data.frame(end.time = x$time), by = x[vars], max)[, "end.time"]

   # Reformat time fields:
   fun <- function(x) return(unlist(lapply(strsplit(as.character(x), " "), function(x) x[2])))
   res$start.time <- fun(res$start.time)
   res$end.time   <- fun(res$end.time)
   res$start.time.minilog <- fun(res$start.time.minilog)
   res$end.time.minilog   <- fun(res$end.time.minilog)
      
   # Round-off data:
   if (round){
      res$depth <- round(res$depth, 1)
      res$bottom.temperature <- round(res$bottom.temperature, 2)
   }
   
   return(res)
}

#' @export split.minilog
split.minilog <- function(x, by = "file.name", set.card, start, end, buffer = 0){
   # SPLIT.MINILOG - Split a 'minilog' object into its component tows.
   
   # Parse 'by' argument:
   by <- match.arg(tolower(by), c("file.name", "time", names(x)))
   
   # Initialize result variable:
   res <- NULL
   
   # Split using file name:
   if (by %in% names(x)){
      if (!("file.name" %in% names(x))) return(x)
      
      # Separate 'x' into component parts:
      res <- by(x, x[, by, drop = FALSE], function(x) return(x))
   
      # Remove 'by' attributes:
      attributes(res) <- NULL
   }
   
   # Split using start and end times:
   if (by == "time"){
      V <- list()

      # Extract start and end times:
      if (!missing(set.card)){
         start <- time(set.card)
         end <- time(set.card) + set.card$duration*60
      }
      
      # Check start and end times:
      if (missing(start) | missing(end)) 
         stop("Unable to proceed without tow start and end times.")
      if (length(start) != length(end)) 
         stop("'start' and 'end' time vectors are not the same length.")
         
      # Partition scanmar object 'x' by adjacent elements of 'time':
      t <- time(x)
      for (i in 1:length(start)){
         index <- (t >= (start[i] - buffer*60)) & (t <= (end[i] + buffer*60))
         V[[i]] <- x[index, ]
      }
      
      # Remove empty tows:
      index <- unlist(lapply(V, function(x) dim(x)[1] == 0))
      
      res <- V[!index]
   }
   
   return(res)
}

#' @export truncate.minilog
truncate.minilog <- function(x, buffer = 0, ...){
   # TRUNCATE.MINILOG - Truncate a 'minilog' object.

   # Check input data:
   year <- unique(x$year)
   if (length(year) != 1) stop("'minilog' object 'x' must contain a single year of data.")
   
   # Load set card:
   tows <- read.scset(year = unique(x$year), print = FALSE)
   tows <- tows[tows$tow.id %in% unique(x$tow.id) , ]
   x <- x[x$tow.id %in% tows$tow.id, ]
   
   # Initialize result vector:
   index <- rep(FALSE, nrow(x))

   # Convert from minutes to seconds:
   if (length(buffer) == 1) buffer = c(buffer, buffer)
   if (!is.numeric(buffer) | !(length(buffer) == 2)) stop("'buffer' must be a numeric scalar or two-element vector.")  
   buffer <- buffer * 60  # Convert to seconds.
   
   # Remove data outside the define start and end times:
   for (i in 1:nrow(tows)){
      ii <- which(x$tow.id == tows$tow.id[i])
      t <- time(x[ii, ])
      ii <- ii[(t >= (start.time(tows[i,], ...) - buffer[1])) & (t <= (end.time(tows[i,], ...) + buffer[2]))]
      index[ii] <- TRUE
   }

   # Return subset of the data:
   x <- x[index, ]

   return(x)
}

#' @export info.minilog
info.minilog <- function(x, set.card = NULL){
   # INFO.MINILOG - Extract information for a 'minilog' object.

   if (!is.null(header(x))){
      v <- list()
      v$file.name <- header(x)$file.name
      v$tow.id <- header(x)$Study.ID
      v$touchdown$estimated <- touchdown(x)
      v$start.time <- header(x)$Start.Time
      v$finish.time <- header(x)$Finish.Time
      v$duration    <- as.numeric(max(time(x)) - min(time(x)))
      v$rows <- dim(x)[1]
      v$columns <- dim(x)[2]
      sensors <- setdiff(names(x), c("year", "month", "day", "hour", "minute", "second"))
      sensors <- sensors[sensors != ""]
      v$sensors <- paste("(", paste(sensors, collapse = ", "), ")", sep = "")
         
      t <- table(diff(time(x)))
      t <- t[t == max(t)]
      v$sampling.frequency <- as.numeric(names(t))
      
      if (!is.null(set.card)){
         index <- match(x, set.card)
         v$tow.number <- set.card[index, "tow.number"]
         v$year <- set.card[index, "year"]
         v$month <- set.card[index, "month"]
         v$day <- set.card[index, "day"]
         v$swept.area <- set.card[index, "swept.area"]
      }
   }else{
      return(NULL)
   }
   
   
   return(v)
}
