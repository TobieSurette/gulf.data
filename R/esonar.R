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
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
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
#'    \item{\code{key.esonar}}{\code{esonar} object index key.}
#'    \item{\code{wingspread}}{Generic \code{wingspread} method.}
#'    \item{\code{wingspread.esonar}}{Extract wing spread observations from \code{esonar} object.}
#'    \item{\code{locate.esonar}}{Find \code{esonar} data file(s).}
#'    \item{\code{read.esonar}}{Read \code{esonar} data file(s).}
#'    \item{\code{plot.esonar}}{Plot \code{esonar} data.}
#'    \item{\code{describe.esonar}}{\code{esonar} data description.}
#'    \item{\code{summary.esonar}}{Summary statistics for an \code{esonar} object.}
#' }
#'

#' @export
esonar <- function(x, ...) UseMethod("esonar")

#' @rdname esonar
#' @export
esonar.default <- function(x, ...){
   # Define as probe data object:
   x <- probe(x, ...)
   
   # Define study project:
   gulf.metadata::project(x) <- project("snow crab survey")
   
   # Define measurement units:
   gulf.metadata::units(x, intersect(c("headline", "wingspread", "doorspread", "doormaster", "depth"), names(x))) <- "meters"
   gulf.metadata::units(x, intersect(c("speed"), names(x))) <- "knots"
   gulf.metadata::units(x, intersect(c("longitude", "latitude", "heading"), names(x))) <- "degrees"

   # Add 'esonar' class tag:
   class(x) <- unique(c("esonar", class(x)))
   
   return(x)
}

#' @rdname esonar
#' @export key.esonar
#' @export
key.esonar <- function(x, ...) if (missing(x)) return(c("date", "time")) else return(gulf.metadata::key(x))

#' @rdname esonar
#' @export wingspread
wingspread <- function(x, ...) UseMethod("wingspread")

#' @rdname esonar
#' @export
wingspread.esonar <- function(x, ...){
   v <- x$wingspread
   if (is.null(v)) v <- x$doorspread
   if (is.null(v)) v <- x$doormaster
   return(v)
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
      if (is.data.frame(x)) if (("year" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }

   # Load set of file names:
   files <- locate(pattern = "*.csv", keywords = "esonar", ...)

   # Search Shared drive:
   if (length(files) == 0){
      if (file.exists(options()$gulf.path$snow.crab)){
         files <- locate(pattern = "*.csv",
                         path = paste0(options()$gulf.path$snow.crab, 
                                       "/Offshore Crab Common/Fishing Year ", year, "/Trawl Data/South Western Gulf/ESonar/Summary"))      
      }
   }

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

#' @rdname esonar
#' @export
plot.esonar <- function(x, ...){
   # Define time series in minutes:
   time <- as.numeric((time(x) - min(time(x))) / 60)

   # Define set of variables to display:
   vars <- c("speed", "heading", "depth", "headline") 
   vars <- vars[vars %in% names(x)]
   vars <- vars[!unlist(lapply(x[, vars], function(x) return(all(is.na(x)))))]
   x$wingspread <- wingspread(x)
   vars <- c(vars, "wingspread")
   
   # Prepare layout:
   m <- kronecker(matrix(1:length(vars)), matrix(1, ncol = 5, nrow = 5))
   m <- rbind(0, cbind(0, m, 0), 0, 0)
   layout(m)
   par(mar = c(0,0,0,0))

   # Plot secondary sensor profile:
   xlim = range(time)
   for (i in  1:length(vars)){
      y <- x[, vars[i]]

      if (!(vars[i] %in% "heading")) ylim = c(0, 1.15 * max(y, na.rm = TRUE)) else ylim <- range(y, na.rm = TRUE)
      plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
      if (!any(is.na(y))){
         lines(time, y, col = "blue", lwd = 2)
      }else{
         points(time, y, pch = 21, bg = "blue")
      }
      
      # Y axis labels:
      ylab <- vars[i]
      ylab <- paste0(toupper(substr(ylab, 1, 1)), substr(ylab, 2, nchar(ylab)))
      if (vars[i] == "wingspread") u <- "meters" else u <- gulf.metadata::units(x)[vars[i]]
      u <- gsub("meters", "m", u)
      u <- gsub("degrees", "°", u)
      u <- gsub("knots", "kts", u)
      
      if (length(u) > 0) ylab <- paste0(ylab, "(", u, ")")
      mtext(ylab, 2, 2.5, cex = 0.8)
      
      # X axis:
      if (i == length(vars)){
         mtext("Time(min)", 1, 2.5, cex = 0.8)
         axis(1)
      } 
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
   y <- read.scsset(year = unique(x$year), print = FALSE)

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
