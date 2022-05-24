#' @title Read VEMCO Minilog Data
#'
#' @description VEMCO Minilog probes are used to measure water temperature and depth. 
#'              These functions reads the ASCII formatted Minilog data or header information.
#'              
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param echo Logical value specifying whether to report files being read to the R console.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.
#' 
#' @examples 
#' # Read snow crab survey Minilog data from 1997:
#' x <- read.minilog(1997)
#' x <- read.minilog(tow.id = 300)
#' 
#' # Read minilog headers:
#' files <- locate.minilog(year = 1999, project = "scs")
#' read.minilog.header(files)

#' @export read.minilog
read.minilog <- function(x, ...) UseMethod("read.minilog")

#' @describeIn read.minilog Read a Minilog data file.
#' @rawNamespace S3method(read.minilog,default)
read.minilog.default <- function(x, file, offset = 0, echo = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)){
      if (missing(x)) file <- locate.minilog(...) else file <- locate.minilog(x, ...)  
   }
   if (length(file) == 0) return(NULL)

   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- vector(mode = "list", length = length(file))
      k <- 0
      for (i in 1:length(file)){
         if (echo) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         x[i] <- list(expand(read.minilog(file[i])))
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
   y <- read.table(file = file, nrow = 30, colClasses = "character", sep = "\n")
   y <- y[, 1]

   # Fix odd characters:
   y <- gsub('\xeb', " ", y)  
   y <- gsub('\xf8C', " ", y)
   y <- gsub('\xb0C', " ", y)
   y <- gsub('\xee', "i", y)  
   y <- gsub('\xfb', "u", y)  
   y <- gsub('\xce', "I", y) 
   y <- gsub('\xc9', "E", y) 
   y <- gsub('\xf4', "a", y) 
   y <- gsub('\xe0', "a", y) 
   y <- gsub('\xe9', "e", y)
   y <- gsub('\xe8', "e", y)  
   y <- gsub('\"+', " ", y)
   
   # Define location of field names :
   k <- grep("^[*]*[ ]*date", tolower(y)) 
   if (length(k) == 0) k <- (length(y) + 1)
   if (length(k) > 1)  k <- k[1]
   
   # Read minilog data:
   for (i in c(",", " ", "\t")){
      if (i == ","){
         x <- read.table(file = file, header = FALSE, skip = k, sep = i, colClasses = "character")
         if (ncol(x) > 1) sep <- i
      }
      if ((ncol(x) == 1) & (i != ",")){
         x <- read.table(file = file, header = FALSE, skip = k, sep = i, colClasses = "character")
         if (ncol(x) > 1) sep <- i
      }
   }

   # Rename fields:
   y[k] <- tolower(gsub("[*] ", "", y[k]))
   y[k] <- gsub("celsius", "temp", y[k])  
   y[k] <- gsub("meters", "depth", y[k])   
   y[k] <- gsub("temp[(] +[)]", "temp", y[k])
   fields <- tolower(unlist(strsplit(y[k], sep)[[1]]))
   fields[grep("date", fields)]  <- "date"
   fields[grep("time", fields)]  <- "time"
   fields[grep("atod", fields)]  <- "depth"
   fields[grep("depth", fields)] <- "depth"
   fields[grep("temp", fields)]  <- "temperature"
   
   # Name variable fields:
   names(x) <- fields
   
   # Get date format:
   date.format <- tolower(unlist(strsplit(y[k], sep)[[1]]))[which(fields == "date")]
   date.format <- tolower(gsub("[)]", "", unlist(strsplit(date.format, "[(]"))[2]))
   if (is.na(date.format)) date.format <- "yyyy-mm-dd"
   
   # Parse header information:
   header <- header.minilog(file)
   
   # Format numeric variables:
   if ("temperature" %in% names(x)) x$temperature <- as.numeric(x$temperature)
   if ("depth" %in% names(x))       x$depth       <- as.numeric(x$depth)
   
   # Fix date formats:
   if (date.format == "yy-mm-dd"){
      ix <- as.numeric(substr(x$date, 1, 2)) < 20
      x$date[!ix] <- paste0("19", x$date[!ix])
      x$date[ix]  <- paste0("20", x$date[ix])
   }
   if (date.format == "dd-mm-yyyy") x$date <- paste0(substr(x$date, 7, 10), "-", substr(x$date, 4, 5), "-", substr(x$date, 1, 2))
   if (date.format == "mm-dd-yyyy") x$date <- paste0(substr(x$date, 7, 10), "-", substr(x$date, 1, 2), "-", substr(x$date, 4, 5))
   
   # Set header:
   header(x) <- header

   # Modify time by specified offset:
   if (offset != 0){
      t <- time(x) +  offset * 60
      x$date <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[1]))
      x$time <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[2]))
   }

   # Convert to minilog object:
   x <- minilog(x)
   header(x) <- header
   
   return(x)
}
