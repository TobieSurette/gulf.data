#' Read Scanmar Acoustic Trawl Data.
#'
#' @description Functions to read Scanmar acoustic trawl measurement data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.
#' 
#' @examples 
#' # Read snow crab survey Scanmar data from 1990 to 1994:
#' x <- read.scanmar(1990:1994)
#' 

#' @export read.scanmar
read.scanmar <- function(x, file, offset = 0, repeats = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x
   if (missing(file)) file <- locate.scanmar(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple scanmar files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
         cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         temp <- read.scanmar(file[i])
         temp <- expand(temp)  # Attach attribute information as appended columns.
         
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
   
   # Load CSV file:
   if (length(grep("[.]csv$", file)) > 0){
      v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      return(v)
   }
   
   # Read and parse header info:
   warnings <- getOption("warn")
   options(warn = -1)
   y <- read.table(file = file, nrow = 20, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)[[1]]

   format <- "simple"
   if (length(grep("filename", tolower(y[1]))) > 0) format <- "complex"
      
   if (format == "simple"){
      x <- read.table(file = file, colClasses = "character", sep = "\n", blank.lines.skip = FALSE, fileEncoding = "UTF-8")[[1]]

      # Parse data fields:
      v <- data.frame(year  = as.numeric(substr(y, 5, 8)),
                      month = as.numeric(substr(y, 3, 4)),
                      day   = as.numeric(substr(y, 1, 2)),
                      tow.number = as.numeric(substr(y, 9, 12)),
                      latitude   = as.numeric(substr(y, 13, 21)),
                      longitude  = as.numeric(substr(y, 23, 31)),
                      wingspread =  as.numeric(substr(y, 32, 36)))
      
      # Store attributes:
      file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
      tow.id <- gsub("([.][A-Zz-z]+)", "", file.name)
      attr(v, "file.name") <- file.name
      attr(v, "tow.id")    <- tow.id
      options(warn = warnings)
      return(v)
   }
   
   if (format == "complex"){
      # Read file and clean up weird characters:
      y <- read.table(file = file, quote = "",  colClasses = "character", sep = "\n", blank.lines.skip = FALSE)[[1]]
      options(warn = warnings)

      y <- gsub('\xeb', " ", y)  
      y <- gsub('\"+', " ", y)
      
      # Define header information:
      ix <- grep("comment", tolower(y))
      header <- c(comment = deblank(strsplit(y[ix], ":")[[1]][2]))
      file.name <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
      tow.id <- toupper(lapply(strsplit(file.name, "[.]"), function(x) x[1])[[1]])
      header <- c(header, c(file.name = file.name, tow.id = tow.id))
     
      # Parse data field names:
      fields <- y[grep("YYYY", y)]
      fields <- deblank(gsub(" +", " ", fields))
      fields <- unlist(strsplit(fields, " ")[[1]])
      if (length(fields) != 21){
         fields <- c("YYYY", "MM", "DD", "HH", "MM", "SS", "Latitude", "Longitude", "Mets", "Knots",
                     "DI1", "?", "CL1", "q", "?", "TO1", "?", "CS1", "?", "TM1", "?")
      }
      ix <- grep("[?]", fields)
      fields[ix] <- paste0("x", 1:length(ix))
      
      # Remove non-data lines:
      y <- y[(grep("-------", y) + 1):length(y)]
      y <- y[-which(nchar(y)  < 100)]
      y <- deblank(y)
      y <- strsplit(y, " +")
      
      # Parse date fields:
      n <- max(unlist(lapply(y, length)))
      for (i in 1:n){
         if (i == 1){
            v <- data.frame(unlist(lapply(y, function(x) x[i])), stringsAsFactors = FALSE)
         }else{
            v <- cbind(v, data.frame(unlist(lapply(y, function(x) x[i])), stringsAsFactors = FALSE))
         }
      }
      
      # Remove empty fields:
      fun <- function(x) all(x %in% c("?", "."))
      v <- v[-which(unlist(lapply(v, fun)))]
      
      # Rename data fields:
      fields <- c(c("date","time"), c("latitude", "longitude"), fields[-(1:8)])
      names(v) <- fields

      # Set header attributes:
      header(v) <- header
   }
   
   # Modify time by specified offset:
   if (offset != 0){
      t <- time(v) +  offset * 60
      v$date <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[1]))
      v$time <- unlist(lapply(strsplit(as.character(t), " "), function(x) x[2]))
   }
   
   # Create 'scanmar' object:
   v <- scanmar(v)
   
   return(v)
}
