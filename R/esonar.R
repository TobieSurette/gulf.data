#' \strong{eSonar} Data
#'
#' @name esonar
#' 
#' @description Functions to read, manipulate and analyze \strong{eSonar} trawl acoustic monitoring  probe data.
#'
#' @param x An \code{esonar} object, data file, survey year or keyword search term.
#' @param year Numeric value specifying the survey or project year(s).
#' @param full.names Logical value specifying whether to include file paths when searching for \strong{eSonar} data files.
#' @param header \strong{eSonar} file header information to be assigned as metadata.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep repeated data records.
#' @param remove Character string specifying keyword(s) which, if found in the data file or path, are removed from the search results.
#'
#' @examples
#' # eSonar files for the 2020 snow crab survey:
#' locate.esonar(year = 2020)
#'
#' # Locate files with a specific tow ID from snow crab survey 2018-2020:
#' locate.esonar("GP001", year = 2018:2020)
#'
#' # Working example:
#' x <- read.esonar("GP001", year = 2020)
#' describe(x)  # Description of file contents.
#' header(x)    # File header information.
#' plot(x)      # Graphical summary.
#' summary(x)   # Data summary.
#'
#' @section Functions:
#' \describe{
#'    \item{\code{esonar}}{Generic \code{esonar} method.}
#'    \item{\code{esonar.default}}{Create an \code{esonar} object.}
#'    \item{\code{locate.esonar}}{Find \code{esonar} data file(s).}
#'    \item{\code{read.esonar}}{Read \code{esonar} data file(s).}
#'    \item{\code{plot.esonar}}{Plot \code{esonar} data.}
#'    \item{\code{map.esonar}}{Map \code{esonar} vessel and data track.}
#'    \item{\code{describe.esonar}}{\code{esonar} data description.}
#'    \item{\code{summary.esonar}}{Summary statistics for an \code{esonar} object.}
#'    \item{\code{match.esonar}}{Match \code{esonar} data records.}
#'    \item{\code{truncate.esonar}}{Remove \code{esonar} which lie outside a specified time interval.}
#' } 
#' 

#' @export
esonar <- function(x, ...) UseMethod("esonar")

#' @rdname esonar
#' @export
esonar.default <- function(x, header, ...){
   # Add 'esonar' class tag:
   if (!("esonar" %in% class(x))) class(x) <- c("esonar", class(x))

   # Add header:
   if (!missing(header)) header(x) <- header

   # Assign key:
   key(x) <- c("year", "month", "day", "hour", "minute", "second")

   # Assign additional arguments as attributes:
   args <- list(...)
   if (length(args) > 0) for (i in 1:length(args)) attr(x, names(args)[i]) <- args[[i]]

   return(x)
}

#' @rdname esonar
#' @export locate.esonar
locate.esonar <- function(x, year, tow.id, full.names = TRUE, remove = "test", ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(tow.id)) tow.id <- x$tow.id
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }

   # Load set of file names:
   files <- locate(pattern = "*.csv", keywords = "esonar", ...)

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

#' @rdname esonar
#' @export read.esonar
read.esonar <- function(x, offset = 0, repeats = FALSE, ...){
   # Define list of files to be read:
   file <- locate.esonar(x, ...)

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
   tow.id <- toupper(lapply(strsplit(header["file.name"], "[.]"), function(x) x[1])[[1]])

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

   # Create 'esonar' object:
   v <- esonar(v, header, tow.id = tow.id, file.name = file.name)

   # Include 'tow.id' as a field:
   v$tow.id <- header[["tow.id"]]

   # Remove records with missing time stamp:
   index <- is.na(v$hour) | is.na(v$minute) | is.na(v$second)
   v <- v[!index, ]

   return(v)
}

#' @rdname esonar
#' @export
plot.esonar <- function(x, ...){
   # Define time series in minutes:
   time <- as.numeric((time(x) - min(time(x))) / 60)

   layout(matrix(1:4, ncol = 2, nrow = 2))

   # Plot primary sensor profile:
   index <- !is.na(time) & !is.na(x$headline) & (x$headline > 0)
   plot(time[index], x$headline[index],
        type = "l", xlab = "Time(min)", ylab = "Headline(m)",
        col = "blue", ylim = c(0, 50))
   points(time[index], x$headline[index], pch = 21, bg = "blue")

   # Plot secondary sensor profile:
   index <- !is.na(time) & !is.na(x$depth)
   plot(time[index], x$depth[index], type = "l", xlab = "Time(min)", ylab = "Depth(m)", col = "blue")
   points(time[index], x$depth[index], pch = 21, bg = "blue")

   # Plot door spread profile:
   index <- !is.na(time) & !is.na(x$doormaster) & (x$doormaster > 0)
   plot(time[index], x$doormaster[index], type = "l",
        xlab = "Time(min)", ylab = "Door spread(m)",
        ylim = c(0, 30), col = "blue")
   points(time[index], x$doormaster[index], pch = 21, bg = "blue")

   map(x)
}

#' @rdname esonar
#' @export
map.esonar <- function(x, set.card = NULL, variable = NULL, ...){
   # MAP.SONAR - Display an 'esonar' object on a map.

   # Create map axes:
   rx <- range(x$longitude)
   ry <- range(x$latitude)
   dx <- diff(rx)
   dy <- diff(ry)
   gulf.map(xlim = c(rx[1] - dx*0.1, rx[2] + dx*0.1),
            ylim = c(ry[1] - dy*0.1, ry[2] + dy*0.1),
            aspect.adjust = TRUE, ...)

   # Draw points:
   if (is.null(variable)){
      points(x$longitude, x$latitude, pch = 21, bg = "blue", cex = 0.8)
   }else{
      index <- !is.na(x[, variable])
      points(x$longitude[index], x$latitude[index], pch = 21, bg = "blue", cex = 2* 0.8* x[index, variable] / max(x[index, variable]))
   }

   # Plot set card start-end points:
   if (!is.null(set.card)){
      index <- match(x, set.card)
      points(set.card$longitude.start[index], set.card$latitude.start[index], pch = 21, bg = "red")
      points(set.card$longitude.end[index], set.card$latitude.end[index], pch = 21, bg = "red")
   }
}

#' @rdname esonar
#' @export
describe.esonar <- function(x, ...){
   if (is.null(header(x))) return(NULL)

   v <- list()
   v$ship.number <- header(x)$ShipNumber
   v$trip.number <- as.numeric(header(x)$TripNumber)
   v$tow.number  <- as.numeric(header(x)$TowNumber)
   v$comments    <- header(x)$Comments
   v$duration    <- as.numeric(max(time(x)) - min(time(x)))

   # Parse file name and path:
   str <- strsplit(header(x)$file, "/", fixed = TRUE)[[1]]
   v$file.name <- str[length(str)]
   if (length(str) == 1) v$path <- "" else v$path <- paste(str[1:(length(str)-1)], collapse = "/")

   str <- strsplit(tolower(v$file), ".", fixed = TRUE)[[1]]
   str <- toupper(str[1])
   v$tow.id <- str
   v$rows <- dim(x)[1]
   v$columns <- dim(x)[2]
   sensors <- unique(x$sensor)
   sensors <- sensors[sensors != ""]
   v$sensors <- paste("(", paste(sensors, collapse = ", "), ")", sep = "")
   t <- table(diff(time(x)))
   t <- t[t == max(t)]
   v$sampling.frequency <- as.numeric(names(t))

   return(v)
}

#' @rdname esonar
#' @export
summary.esonar <- function(x, year, truncate = TRUE, round = TRUE, ...){


   # Load E-Sonar data:
   if (missing(x) & !missing(year)) x <- read.esonar(year = year)

   # Read tow data:
   y <- read.scset(year = unique(x$year), print = FALSE)

   # Trim data:
   if (truncate) x <- truncate(x, ...)

   # Define aggregating variables:
   vars <- c("year", "month", "day", "tow.id", "tow.number")
   res <- aggregate(list(speed = x$speed), x[vars], mean, na.rm = TRUE)
   res <- cbind(res, aggregate(list(heading = x$heading), x[vars], mean, na.rm = TRUE)["heading"])
   res <- cbind(res, aggregate(list(longitude.start = x$longitude), x[vars], min, na.rm = TRUE)["longitude.start"])
   res <- cbind(res, aggregate(list(latitude.start = x$latitude), x[vars], min, na.rm = TRUE)["latitude.start"])
   res <- cbind(res, aggregate(list(longitude.end = x$longitude), x[vars], max, na.rm = TRUE)["longitude.end"])
   res <- cbind(res, aggregate(list(latitude.end = x$latitude), x[vars], max, na.rm = TRUE)["latitude.end"])

   # Attach tow validity
   index <- match.data.frame(res[c("year", "tow.id")], y[c("year", "tow.id")])
   res$valid <- y$valid[index]
   res$tow.number.logbook <- y$tow.number[index]

   # Calculate tow distance:
   res$distance       <- distance(res$longitude.start, res$latitude.start, res$longitude.end,        res$latitude.end, pairwise = FALSE)
   res$distance.start <- distance(res$longitude.start, res$latitude.start, y$longitude.start.logbook[index], y$latitude.start.logbook[index], pairwise = FALSE)
   res$distance.end   <- distance(res$longitude.end,   res$latitude.end,   y$longitude.end.logbook[index],   y$latitude.end.logbook[index], pairwise = FALSE)

   # Net measurements:
   res <- cbind(res, aggregate(list(depth = x$depth), x[vars], mean, na.rm = TRUE)["depth"])
   res <- cbind(res, aggregate(list(wingspread = x$doormaster), x[vars], mean, na.rm = TRUE)["wingspread"])
   res <- cbind(res, aggregate(list(headline = x$headline), x[vars], mean, na.rm = TRUE)["headline"])

   # Number of non-NA observations:
   fun <- function(x) return(sum(!is.na(x)))
   res <- cbind(res, aggregate(list(n.depth = x$depth), x[vars], fun)["n.depth"])
   res <- cbind(res, aggregate(list(n.wingspread = x$doormaster), x[vars], fun)["n.wingspread"])
   res <- cbind(res, aggregate(list(n.headline = x$headline), x[vars], fun)["n.headline"])
   res <- cbind(res, aggregate(list(n = x$depth), x[vars], length)["n"])

   # Calculate sampling rate:
   x$time <- time2sec(time(x))

   # Calculate length of data interval:
   res$duration <- aggregate(list(x = x$time), x[vars], function(x) return(diff(range(x))))$x + 1

   # Calculate data sampling rate:
   fun <- function(x){
      d <- table(diff(x))
      return(as.numeric(names(d[d == max(d)]))[1])
   }
   res <- cbind(res, aggregate(list(sampling.rate = x$time), x[vars], fun)["sampling.rate"])

   # Round-off results:
   rvars <- c("heading", "depth", "wingspread", "headline")
   if (round){
      res[rvars] <- round(res[rvars], 1)
      res[c("speed")] <- round(res[c("speed")], 2)
      res[c("distance", "distance.start", "distance.end")] <- round(res[c("distance", "distance.start", "distance.end")], 3)
   }

   # Sort results:
   vars <- c("year", "month", "day", "tow.number", "tow.id")
   res <- sort(res, by = vars)
   rownames(res) <- NULL

   return(res)
}

#' @rdname esonar
#' @export
match.esonar <- function(x, set.card, method = "file.name"){
   # MATCH.ESONAR - Return the set card indices which match a 'esonar' object.

   # Parse 'method' argument:
   method <- tolower(method)
   method <- gsub(".", "", method, fixed = TRUE)
   method <- gsub(" ", "", method, fixed = TRUE)
   method <- match.arg(tolower(method), c("latlong", "filename", "time"))

   # Single esonar file:
   if (!is.null(header(x))){
      if (!missing(set.card)){
         # Match 'x' to set card using coordinates:
         if (method == "latlong"){
            index <- which(set.card$year == info(x)$year)
            set.card <- set.card[index, ]
            d <- distance(mean(x$longitude, na.rm = TRUE), mean(x$latitude, na.rm = TRUE), longitude(set.card), latitude(set.card)) * 1000
            d <- as.numeric(d)
            if (d[which.min(d)] > 500) index <- NA else index <- which.min(d)
         }

         # Match 'x' to set card using file name:
         if (method == "filename"){
            set.card$file.name <- gsub(" ", "", set.card$file.name)
            temp <- info(x)
            temp <- data.frame(year       = temp$year,
                               tow.number = temp$tow.number,
                               file.name  = temp$file.name,
                               stringsAsFactors = FALSE)

            index <- match(temp, set.card, by = c("year", "tow.number", "file.name"))
         }

         # Match 'x' to set card using file name:
         if (method == "time"){
            ts <- touchdown(set.card) + (touchdown(set.card) - liftoff(set.card)) / 2
            tm <- mean(time(x)) - 3*60*60 # Substract 3 hours.
            d <- abs(as.numeric(tm - ts))
            if (d[which.min(d)] > 6) index <- NA else index <- which.min(d)
         }
      }
   }

   # Multiple esonar files:
   if (is.null(header(x))){
      # Match 'x' to set card using coordinates:
      if (method == "latlong"){
         res <- aggregate(x[c("longitude", "latitude")], by = x[c("year", "tow.number", "file.name")], mean)
         d <- distance(res$longitude, res$latitude, longitude(set.card), latitude(set.card)) * 1000
         res$index <- apply(d, 1, which.min)
         res$distance <- apply(d, 1, function(x) x[which.min(x)])
         res$index[res$distance > 500] <- NA
         index <- merge(x, res, by = c("year", "tow.number", "file.name"), names = "index", all.x = TRUE, sort = FALSE)$index
      }

      # Match 'x' to set card using file name:
      if (method == "filename"){
         set.card$file.name <- gsub(" ", "", set.card$file.name)
         set.card$file.name <- unlist(lapply(strsplit(set.card$file.name, ".", fixed = TRUE), function(x) x[[1]]))
         x$file.name <- unlist(lapply(strsplit(x$file.name, ".", fixed = TRUE), function(x) x[[1]]))
         index <- match(x$file.name, set.card$file.name)
      }

      # Match 'x' to set card using file name:
      if (method == "time"){
         ts <- touchdown(set.card) + (touchdown(set.card) - liftoff(set.card)) / 2
         tm <- time(x) - 3*60*60 # Substract 3 hours.
         res <- aggregate(tm, by = x[c("year", "tow.number", "file.name")], mean)
         d <- abs(repvec(res[, 4], ncol = length(ts)) - repvec(ts, nrow = dim(res)[1]))
         res$index <- apply(d, 1, which.min)
         res$distance <- apply(d, 1, function(x) x[which.min(x)])
         res$index[res$distance > 600] <- NA
         index <- merge(x, res, by = c("year", "tow.number", "file.name"), names = "index", all.x = TRUE, sort = FALSE)$index
      }
   }

   return(index)
}

#' @rdname esonar
#' @export
truncate.esonar <- function(x, buffer = 0, ...){
   # TRUNCATE.ESONAR - Truncate a 'esonar' object.

   # Check input data:
   year <- unique(x$year)
   if (length(year) != 1) stop("'esonar' object 'x' must contain a single year of data.")

   # Load set card:
   tows <- read.scset(year = unique(x$year), print = FALSE)
   tows <- tows[tows$tow.id %in% unique(x$tow.id) , ]
   x <- x[x$tow.id %in% tows$tow.id, ]

   # Initialize result vector:
   index <- rep(FALSE, nrow(x))

   # Convert from minutes to seconds:
   if (!is.numeric(buffer) | !(length(buffer) %in% c(1,2)))
      stop("'buffer' must be a numeric scalar or two-element vector.")
   if (length(buffer) == 1) buffer = c(buffer, buffer)
   buffer <- buffer * 60  # Convert to seconds.
   buffer <- abs(buffer)

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
