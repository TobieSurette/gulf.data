#' \strong{Star-Oddi} Data
#'
#' @name star.oddi
#'
#' @description Functions to read, manipulate and analyze \strong{star.oddi} trawl acoustic monitoring  probe data.
#'
#' @param x An \code{star.oddi} object, data file, survey year or keyword search term.
#' @param year Numeric value specifying the survey or project year(s).
#' @param full.names Logical value specifying whether to include file paths when searching for \strong{star.oddi} data files.
#' @param header \strong{star.oddi} file header information to be assigned as metadata.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param remove Character string specifying keyword(s) which, if found in the data file or path, are removed from the search results.
#'
#' @examples
#' # star.oddi files for the 2020 snow crab survey:
#' locate.star.oddi(year = 2020)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.star.oddi("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.star.oddi("GP001", year = 2020)
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#' plot(x)      # Graphical summary.
#' summary(x)   # Data summary.
#'
#' @section Functions:
#' \describe{
#'    \item{\code{star.oddi}}{Generic \code{star.oddi} method.}
#'    \item{\code{star.oddi.default}}{Create an \code{star.oddi} object.}
#'    \item{\code{locate.star.oddi}}{Find \code{star.oddi} data file(s).}
#'    \item{\code{read.star.oddi}}{Read \code{star.oddi} data file(s).}
#'    \item{\code{plot.star.oddi}}{Plot \code{star.oddi} data.}
#'    \item{\code{map.star.oddi}}{Map \code{star.oddi} vessel and data track.}
#'    \item{\code{describe.star.oddi}}{\code{star.oddi} data description.}
#'    \item{\code{summary.star.oddi}}{Summary statistics for an \code{star.oddi} object.}
#'    \item{\code{match.star.oddi}}{Match \code{star.oddi} data records.}
#' }
#'

#' @export
star.oddi <- function(x, ...) UseMethod("star.oddi")

#' @rdname star.oddi
#' @export
star.oddi.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Add 'esonar' class tag:
   class(x) <- unique(c("star.oddi", class(x)))

   return(x)
}

#' @rdname star.oddi
#' @export locate.star.oddi
locate.star.oddi <- function(x, year, tow.id, full.names = TRUE, location, remove = "test", ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(tow.id)) tow.id <- x$tow.id
      if (is.data.frame(x)) if (("year" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }

   # Load set of file names:
   files <- locate(pattern = "*.DAT", keywords = "star oddi", ...)

   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
      files <- unique(files[index])
   }

   # Target tow ID:
   if (!missing(tow.id)){
      tow.id <- as.character(tow.id)
      index <- NULL
      for (i in 1:length(tow.id)) index <- c(index, grep(tolower(tow.id[i]), tolower(files)))
      files <- unique(files[index])
   }

   # Remove path:
   if (!full.names) files <- unlist(lapply(strsplit(files, "/", fixed = TRUE), function(x) x[length(x)]))

   # Remove files:
   if (!missing(remove)) if (length(remove) == 1) if (remove == FALSE) remove <- NULL
   if (!missing(remove)) remove <- remove[remove != "" & !is.na(remove)]
   if ((length(files) > 0) & (length(remove) > 0)) {
      index <- NULL
      for (i in 1:length(remove)) index <- c(index, grep(tolower(remove[i]), tolower(files)))
      if (length(index) > 0) files <- files[-index]
   }

   # Only keep unique file names:
   files <- unique(files)

   return(files)
}

#' @rdname star.oddi
#' @export read.star.oddi
read.star.oddi <- function(x, offset = 0, repeats = FALSE, ...){
   # Define list of files to be read:
   file <- locate.star.oddi(x, ...)

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

   if (length(file) == 0) return(NULL)

   if.numeric.str <- function(x) return(gsub("[ 0-9.]", "", x) == "")

   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n", blank.lines.skip = FALSE)[[1]]
   y <- gsub("#", "", y[substr(y, 1, 1) == "#"], fixed = TRUE)
   #y <- lapply(strsplit(y, "\t"), function(x) if (if.numeric.str(x[1])) x else x[2:length(x)])
   y <- lapply(strsplit(y, "\t"), function(x) x[2:length(x)])
   str <- unlist(lapply(y, function(x) return(gsub(":", "", x[1]))))
   #str <- gsub(" ", ".", unlist(lapply(y, function(x) gsub("(:)", "", x[[1]]))))
   #str <- gsub("..", ".", str, fixed = TRUE)
   #str <- gsub("-", ".", str, fixed = TRUE)
   values <- unlist(lapply(y, function(x) paste(x[2:length(x)], collapse = ", ")))
   header <- list()
   for (i in 1:length(str)){
      header[[i]] <- values[i]
   }
   names(header) <- str
   header$file.name <- file

   # Extract channel names:
   index <- grep("Channel [1-9]", str)
   channels <- unlist(header[index])
   if (is.null(channels)){
      index <- grep("Axis", str)
      channels <- unlist(header[index])
   }
   if (length(grep(".ACC", file)) > 0){
      channels <- gsub("Acc. vector (m/s^2), ", "", channels, fixed = TRUE)
      channels <- gsub("Axes X,Y,Z (m/s^2), ", "", channels, fixed = TRUE)
      channels <- lapply(strsplit(channels, ","), function(x) return(x[[1]]))
      channels <- unlist(channels)
   }else{
      channels <- strsplit(channels, "[,()]")
      channels <- unlist(lapply(channels, function(x) x[which.max(nchar(x))]))
      channels <- gsub(" ", "", channels, fixed = TRUE)
      channels <- gsub("-", ".", channels, fixed = TRUE)
      channels <- tolower(channels)
      names(channels) <- NULL
   }

   # Read E-Sonar data:
   k <- length(y)
   x <- read.table(file = file, header = FALSE, skip = k, dec = ",", sep = "\t",
                   colClasses = c("numeric", "character", rep("numeric", length(channels))))
   fields <- c("record", "date", channels)
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
      header$tow.id <- tow.id

      # Include 'tow.id' as a field:
      v$tow.id <- header$tow.id
   }

   # Create 'esonar' object:
   v <- star.oddi(v, header)

   return(v)
}

#' @rdname star.oddi
#' @export
plot.star.oddi <- function(x, tow.id, year, buffer = 2, ...){   
   # PLOT.STAR.ODDI - Graphically display a Star Oddi object.

   # Define 'tow.id' argument:
   if (!missing(year) & !missing(tow.id) & missing(x)) x <- read.star.oddi(year = year, tow.id = tow.id)
   if (missing(tow.id)) tow.id <- sort(unique(x$tow.id))
   
   # Define data year:
   if (missing(year)) year <- sort(unique(x$year))
   
   # Parse 'buffer' argument:
   if (length(buffer) == 1) buffer  <- c(buffer, buffer)
   
   # Define default colours:
   cols <- c("blue", "chartreuse3")
   
   for (i in 1:length(tow.id)){
      windows()
      par(mar = c(5, 4, 4, 4))
      y <- read.scset(year = year)
      
      tt <- c(start.time(y[y$tow.id == tow.id[i], ]), end.time(y[y$tow.id == tow.id[i], ]))
      if (is.na(tt[1])) tt[1] <- min(time(x))
      if (is.na(tt[2])) tt[2] <- max(time(x))
      
      # Subset by tow ID:
      xx <- x[x$tow.id == tow.id[i], ]
      
      # Define relative time by minutes:
      xx$time <- time2min(time(xx), tt[1])
      
      # Start and end times relative to start time:
      tt <- time2min(tt, tt[1])
      
      # Plot depth:
      flag <- "depth" %in% names(x)
      if (!flag) xx$depth <- xx$pressure
      plot(xx$time, xx$depth, xlim = c(0-buffer[1], tt[2]+buffer[2]), ylim = c(0, 1.15 * max(xx$depth)), 
           lwd = 2, col = cols[1], yaxt = "n", ylab = "", xlab = "Time (min)", type = "l")
      axis(2, col = cols[1], col.axis = cols[1])
      if (flag) mtext("Depth(m)", 2, 2, col = cols[1]) else  mtext("Pressure", 2, 2, col = cols[1])
      
      # Plot temperature:
      if ("temperature" %in% names(x)){
         ii <- (xx$time >= (0-buffer[1])) & (xx$time <= (tt[2]+buffer[2]))
         r <- range(x$temperature[ii])
         r <- c(r[1] - 0.25, r[2] + 0.4)
         par(usr = c(par("usr")[1:2], r))
         lines(xx$time, xx$temperature, lwd = 2, col = cols[2])
         axis(4, col = cols[2], col.axis = cols[2])
         mtext("Temperature(C)", 4, 2, col = cols[2])
      }
      
      lines(c(tt[1], tt[1]), par("usr")[3:4], lwd = 2, lty = "dashed", col = "red")
      lines(c(tt[2], tt[2]), par("usr")[3:4], lwd = 2, lty = "dashed", col = "red")
      
      legend("bottomleft", legend = c("Depth", "Temperature", "Start and end time"), 
             lwd = 2, bg = "white", col = c(cols, "red"), lty = c("solid", "solid", "dashed")) 
             
      text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
           par("usr")[3] + 0.95 * diff(par("usr")[3:4]), 
           paste0("Tow ID: '", tow.id[i], "'"))
   }
}

#' @rdname star.oddi
#' @export
describe.star.oddi <- function(x, ...){}

#' @rdname star.oddi
#' @export
summary.star.oddi <- function(x, year, truncate = TRUE, round = TRUE, ...){}
 
