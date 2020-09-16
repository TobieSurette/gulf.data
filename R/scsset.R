#' Snow Crab Set/Tow Data
#' 
#' @name scsset
#' 
#' @description The \code{scsset} class is a container for Snow Crab Survey Set/Tow data, i.e. 
#'              information about individual tows performed on the snow crab annual survey. 
#'              
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric 
#'          vector specifying the survey years.
#'          
#' @param year Survey year(s) to be loaded.   
#'  
#' @examples 
#' # Read data:
#' x <- read.scsset()                 # Read all avaliable data.
#' x <- read.scsset(year = 2019)      # Read single year.
#' x <- read.scsset(year = 2010:2015) # Read range of years. 
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{key.scsset}}{Define or extract \code{scsset} object index key.}
#'   \item{\code{scsset}}{Generic \code{scsset} method.}
#'   \item{\code{scsset.default}}{Create an \code{scsset} object.}
#'   \item{\code{locate.scsset}}{Locate snow crab set/tow data files.}
#'   \item{\code{read.scsset}}{Read snow crab survey set/tow data.}
#'   \item{\code{start.time.scsset}}{Tow start time.}
#'   \item{\code{end.time.scsset}}{Tow end time.}
#'   \item{\code{summary.scsset}}{Return a summary of an \code{scsset} object.}
#' }
#' 
#' @seealso \code{\link{scsbio}}, \code{\link{tow.id}}

#' @rdname scsset
#' @export
key.scsset <- function(x, ...) if (missing(x)) return(c("year", "tow.id") ) else return(attr(x, "key"))

#' @export scsset
scsset <- function(x, ...) UseMethod("scsset")

#' @rdname scsset
#' @export
scsset.default <- function(x, ...){
   if ("scsset" %in% class(x)) return(x)
 
   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::key(x) <- key.scsset()
   gulf.metadata::units(x, c("longitude", "latitude", "longitude.start.logbook", "longitude.end.logbook", "latitude.start.logbook", "latitude.end.logbook")) <- "degrees"
   gulf.metadata::units(x, "swept.area") <- "square.meters"
   gulf.metadata::units(x, c("depth", "warp")) <- "fathoms"
   gulf.metadata::units(x, "bottom.temperature") <- "degreesC"
   gulf.metadata::fmt(x, c("start.time", "end.time", "start.time.logbook", "end.time.logbook")) <- "hh:mm:ss"
   
   # Define class:
   class(x) <- unique(c("scsset", class(x))) 
   
   return(x)
}

#' @rdname scsset
#' @export locate.scsset
locate.scsset <- function(x, year, source = "gulf.data", ...){
   if (!missing(x)) if (is.numeric(x)) year <- x

   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      path <- paste0(path, "/Trawl Data/South Western Gulf/Tow Data")
      file <- locate(path = path, pattern = "Tows [0-9]{4,4}.txt")
   }
      
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", pattern = c("scs", "set", "csv"), ...)

   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @rdname scsset
#' @export read.scsset
read.scsset <- function(x, ...){
   # Find data file:
   file <- locate.scsset(x, ...)
   
   # No files found:
   if (length(file) == 0) return(NULL)
   
   # Load data:
   v <- NULL
   for (i in 1:length(file)){
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file[i], "[.]"), function(x) x[length(x)])))
      
      tmp <- NULL
      # Read fixed-width file:
      if (ext == "txt"){
         tmp <- read.fortran(file = file, format = c("I4", "I2", "I2", "A2", "A8", "I2", "I1", "I8", "I8", "I8",
                                                     "I8", "I8", "I8", "A8", "A8", "A8", "A8", "I5", "F4.1", "I4",
                                                     "F5.1", "A7", "I1", "I1", "A300"))
         
         names(tmp) <- c("year", "month", "day", "zone", "tow.id", "tow.number", "valid", "longitude", "latitude", 
                         "longitude.start.logbook", "longitude.end.logbook", "latitude.start.logbook", "latitude.end.logbook",            
                         "start.time", "end.time", "start.time.logbook", "end.time.logbook",   
                         "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method", 
                         "groundfish.sample", "water.sample", "comment")
      }
      
      # Remove blank spaces:
      for (j in 1:ncol(tmp)) if (is.character(tmp[, j])) tmp[,j] <- gulf.utils::deblank(tmp[,j]) 
      
      # Read comma-delimited file:
      if (ext == "csv") tmp <- read.csv(file[i], header = TRUE, stringsAsFactors = FALSE)

      # Append data:
      v <- rbind(v, tmp) 
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

   return(v)
}

#' @rdname scsset
#' @export start.time.scsset
#' @rawNamespace S3method(start.time,scsset)
start.time.scsset <- function(x, ...){
   v <- x$start.time
   v[is.na(v)] <- ""
   index <- which(deblank(v) == "")
   v[index] <- x$start.time.logbook
   v[is.na(v)] <- ""
   v <- as.POSIXct(paste(as.character(gulf.utils::date(x)), v))
   return(v)   
}

#' @rdname scsset
#' @rawNamespace S3method(end.time,scsset)
end.time.scsset <- function(x, ...){
   v <- x$end.time
   v[is.na(v)] <- ""
   index <- which(deblank(v) == "")
   v[index] <- x$end.time.logbook
   v[is.na(v)] <- ""
   v <- as.POSIXct(paste(as.character(gulf.utils::date(x)), v))
   return(v)   
}

#' @rdname scsset
#' @export
summary.scsset <- function(x, truncate = TRUE, ...){
   if (missing(tow.id)) tow.id <- unique(x$tow.id)

   # Initialize result variable:
   res <- NULL
   
   for (i in 1:nrow(x)){
      # Read probe files:
      esonar   <- read.esonar(year = x$year[i], tow.id = x$tow.id[i])
      minilog  <- read.minilog(year = x$year[i], tow.id = x$tow.id[i])
      headline <- read.star.oddi(year = x$year[i], tow.id = x$tow.id[i], type = "depth")
      tilt     <- read.star.oddi(year = x$year[i], tow.id = x$tow.id[i], type = "tilt")
      
      # Record count function:
      n <- function(x) if (is.data.frame(x)) return(nrow(x)) else return(length(x))
      
      # Calculate minimum distance between eSonar coordinates and survey logbook coordinates:
      esonar.distance <- NA
      if (n(esonar) > 0) esonar.distance <- min(distance(longitude(x[i,]), latitude(x[i,]), esonar$longitude, esonar$latitude))

      # Counts of data records:
      tmp <- data.frame(date = as.character(gulf.utils::date(x[i,])),
                        tow.number = x$tow.number[i],
                        valid = x$valid[i],
                        esonar = n(esonar), 
                        minilog = n(minilog), 
                        headline = n(headline), 
                        tilt = n(tilt),
                        esonar.distance = esonar.distance)
      
      # Apped results:
      res <- rbind(res, tmp)
   }
   
   return(res)
}


