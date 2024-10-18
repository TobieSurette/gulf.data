#' Read Seabird Probe Data.
#'
#' @description Functions to read Seabird data, such as depth/temperature or acoustic trawl monitoring data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.

#' @export read.seabird
read.seabird <- function(x, ...) UseMethod("read.seabird")

#' @describeIn read.seabird Read a Seabird data file.
#' @rawNamespace S3method(read.seabird,default)
read.seabird.default <- function(x, file, offset = 0, repeats = FALSE, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)){
      if (missing(x)) file <- locate.star.oddi(...) else file <- locate.star.oddi(x, ...)  
   }
   if (length(file) == 0) return(NULL)

   # Read multiple Star Oddi files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- vector(mode = "list", length = length(file))
      k <- 0
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         x[i] <- list(expand(read.star.oddi(file[i])))
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

   extension <- tolower(unlist(lapply(strsplit(file, "[.]"), function(x) x[length(x)])))
   if (extension == "csv") return(read.csv(file))
   
   # Read file header information:
   header <- readLines(file, n = 15)
   header <- header[grep("[%]", header)]
   header <- gsub("% +", "", header)
   header <- gsub(",,", "", header)
   header <- strsplit(header, " = ")
   names <- tolower(unlist(lapply(header, function(x) x[1])))
   names <- gsub(" ", ".", gsub(": ", "", names))
   header <- unlist(lapply(header, function(x) x[2]))
   names(header) <- names
   header <- header[!is.na(header)]
   
   # Parse data fields:
   x <- readLines(file)
   x <- x[(max(grep("[%]", x)) + 1):length(x)]
   fields <- tolower(unlist(strsplit(x[1], ",")))
   fields <- gsub('"', "", fields)
   x <- x[-1]
   x <- gsub('"', "", x)
   
   # Parse data:
   x <- strsplit(x, ",")
   date <- unlist(lapply(x, function(x) x[1]))
   time <- unlist(lapply(x, function(x) x[2]))
   temperature <- as.numeric(unlist(lapply(x, function(x) x[3])))
   
   # Build result:
   v <- data.frame(date = date, time = time, temperature = temperature)
   
   # Store file name:
   file <- unlist(lapply(strsplit(file, "/"), function(x) x[length(x)]))
   if (length(file) > 0) header <- c(file.name = file, header)
   
   # Convert to 'seabird' object:
   v <- seabird(v)
   header(v) <- header

   return(v)
}
