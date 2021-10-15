#' @title Fetch Data Probe File Header Information
#' 
#' @description Extract file header information from probe data files.
#' 
#' @param x Data file name(s).
#' @param probe Data probe name. See \link[gulf.data]{probe} for available options.
#' @param verbose Logical value specifying whether to report files as they are being read.
#' 
#' @examples 
#' header.minilog()
#' header.minilog(1998)

#' @export header.probe
header.probe <- function(x, probe, ...){
   probe <- probe(probe)
   if (probe == "minilog")   v <- header.minilog(x, ...)
   if (probe == "star.oddi") v <- header.star.oddi(x, ...)
   if (probe == "netmind")   v <- header.netmind(x, ...)
   if (probe == "esonar")    v <- header.esonar(x, ...)
   
   return(v)
}

#' @describeIn header.probe Extract Minilog file header information.
#' @export header.minilog
header.minilog <- function(x, verbose = FALSE, ...){
   # Define file(s) to be read:
   file <- NULL
   if (!missing(x)) if (is.character(x)) file = x
   if (length(file) == 0){
      if (missing(x)) file <- locate.minilog(...) else file <- locate.minilog(x, ...)  
   }
   if (length(file) == 0) return(NULL)

   # Read multiple Minilog files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.minilog(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Read and parse header info:
   warnings <- getOption("warn")
   options(warn = -1)
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n")
   options(warn = warnings)
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
   k <- grep("date", tolower(y)) 
   if (length(k) == 0) k <- (length(y) + 1)
   if (length(k) > 1)  k <- k[1]

   # Parse header information:
   header <- gulf.utils::deblank(unlist(lapply(strsplit(y[1:(k-1)], "[a-z ][:=]"), function(x) x[2])))
   str <- tolower(gsub("^[*] ", "", unlist(lapply(strsplit(y[1:(k-1)], "[:=]"), function(x) x[1]))))
   str <- gsub("[\\%*][ ]", "", str)
   names(header) <- gsub(" ", ".", gulf.utils::deblank(str))
   header <- header[!is.na(header)] 

   return(header)
}

#' @describeIn header.probe Extract Star Oddi file header information.
#' @export header.star.oddi
header.star.oddi <- function(x, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x)){
      file <- NULL
      if (is.character(x)) file <- x 
      if (is.numeric(x))   file <- locate.star.oddi(x, ...) 
   }else{
      file <- locate.star.oddi(...) 
   } 
   if (length(file) == 0) return(NULL)
   
   # Read multiple Star Oddi files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.star.oddi(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Empty file:
   if (length(file) == 0) return(NULL)

   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n", blank.lines.skip = FALSE, fileEncoding = "Windows-1252")[[1]]
   y <- gulf.utils::deblank(y)
   y <- gsub("\t", " ", y)
   ix <- grep("^#", y)
   y <- y[ix]
   y <- gsub("^#[0-9]* ", "", y)

   # Parse header:
   str <- strsplit(y, ":")
   header <- deblank(unlist(lapply(str, function(x) x[2])))
   names(header) <- unlist(lapply(str, function(x) x[1]))
   names(header) <- gsub("#", "", names(header))
   names(header) <- gulf.utils::deblank(names(header))
   
   return(header)
}

#' @describeIn header.probe Extract Netmind file header information.
#' @export header.netmind
header.netmind <- function(x, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x)){
      file <- NULL
      if (is.character(x)) file <- x 
      if (is.numeric(x))   file <- locate.netmind(x, ...) 
   }else{
      file <- locate.netmind(...) 
   } 
   if (length(file) == 0) return(NULL)

   # Read multiple Netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.netmind(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Read file and clean up weird characters:
   warnings <- getOption("warn")
   options(warn = -1)
   y <- read.table(file = file, quote = "",  colClasses = "character", sep = "\n", blank.lines.skip = FALSE)[[1]]
   options(warn = warnings)
   
   # Replace problem characters:
   y <- gsub('\xee', "i", y)  
   y <- gsub('\xfb', "u", y)  
   y <- gsub('\xce', "I", y) 
   y <- gsub('\xc9', "E", y) 
   y <- gsub('\xf4', "a", y) 
   y <- gsub('\xe0', "a", y) 
   y <- gsub('\xe9', "e", y)
   y <- gsub('\xe8', "e", y)  
   y <- gsub('\xeb', " ", y)  
   y <- gsub('\"+', " ", y)
   
   # Fix blanks and missing data lines:
   y <- tolower(gulf.utils::deblank(y))
   y <- y[y != ""]

   comment <- gsub("comment[s]*[: ]*", "", y[grep("comment", y)])
   tow <- strsplit(y[grep("tow", y)], "tow")[[1]]
   tow <- tow[length(tow)]
  
   # Define header info:
   header <- c(comment = comment, tow = tow)
   
   return(header)
}

#' @describeIn header.probe Extract eSonar file header information.
#' @export header.esonar
header.esonar <- function(x, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x)){
      file <- NULL
      if (is.character(x)) file <- x 
      if (is.numeric(x))   file <- locate.esonar(x, ...) 
   }else{
      file <- locate.esonar(...) 
   } 
   if (length(file) == 0) return(NULL)

   # Read multiple eSonar files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.esonar(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }

   # Read and parse header info:
   y <- read.table(file = file, nrow = 10, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)

   # Define header information:
   header <- NULL
   vars <- gsub(" ", "", strsplit(y[1, ], ",")[[1]])
   values <- strsplit(y[2, ], ",")[[1]]
   if (length(values) > length(vars)) values <- values[1:length(vars)]
   header[vars] <- values
   comment <- strsplit(y[4, ], ",")[[1]]
   if (length(comment) == 0) comment <- ""
   vars <- gsub(" ", "", strsplit(y[3, ], ",")[[1]])
   if (length(comment) > 1){
      vars <- vars[vars != ""]      
      comment <- paste(comment[comment != ""], collapse = ", ")
   }
   header[vars] <- comment
   header <- header[names(header) != ""]
   
   return(header)
}

#' @describeIn header.probe Extract Notus file header information.
#' @export header.notus
header.notus <- function(x, verbose = FALSE, ...){
   # Define file(s) to be read:
   if (!missing(x)){
      file <- NULL
      if (is.character(x)) file <- x 
      if (is.numeric(x))   file <- locate.notus(x, ...) 
   }else{
      file <- locate.notus(...) 
   } 
   if (length(file) == 0) return(NULL)
   
   # Read multiple eSonar files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      for (i in 1:length(file)){
         if (verbose) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
         header <- header.notus(file[i])
         header <- as.data.frame(t(header), stringsAsFactors = FALSE)
         header["file.name"] <- unlist(lapply(strsplit(file[i], "/"), function(x) x[length(x)])[[1]])
         if (i == 1){
            x <- header
         }else{
            if (!all(names(header) %in% names(x))) x[setdiff(names(header), names(x))] <- ""
            if (!all(names(x) %in% names(header))) header[setdiff(names(x), names(header))] <- ""
            header <- header[names(x)]
            x <- rbind(x, header)
         } 
      }
      
      rownames(x) <- NULL
      
      return(x)
   }
   
   # Parse header for a single file:
   x <- readLines(file, n = 25) 
   
   # Parse variable field names:
   i <- min(grep("^Code", x))
   x <- x[1:(i-1)]
   x <- x[x != ""]
   x <- x[-grep("NOTUS TEXTFILE", x)]

   # Extract header information:
   header <- unlist(lapply(strsplit(x, ": "), function(x) x[2]))
   names(header) <- unlist(lapply(strsplit(x, ": "), function(x) x[1]))
   
   return(header)
}
