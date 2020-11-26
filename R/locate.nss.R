#' Locate Northumberland Strait Survey Data
#' 
#' @description Functions to locate various data file(s) from different studies and surveys.
#' 
#' @param x Data object.
#' @param year Study or survey year.
#' @param source Character string specifying the data source for data files. Options are \sQuote{ascii} or a gulf 
#'               package name (e.g. \sQuote{gulf.data}).
#' @param cruise Character string(s) specifying a survey cruise identifier(s) (e.g. "P953").        
#' 
#' @examples 
#' locate.nssset()
#' locate.nsscat()
#' locate.nssbio()
#' 
#' # Northumberland Strait length-frequency files:
#' locate.nsslen()
#' locate.nsslen(2020)
#' 
#' @seealso \code{\link{nss}}
#' @seealso \code{\link{read.nss}}
#' 
#' @describeIn locate.nss Locate Northumberland Strait survey set data files.
#' @export locate.nssset
locate.nssset <- function(x, year, source = "gulf.data", cruise, ...){
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   file <- NULL
   
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", file = c("nss", "set", "csv"), ...)
   
   # Use DFO drive as data source:
   if (length(file) == 0) source <- "ascii"
   if (source == "ascii") file <- dir(path = options()$gulf.path$nss, pattern = "^rv.[0-9]+s.new", full.names = TRUE)
   
   # Subset by year:
   if (!missing(year)){
      if (source == "ascii"){ 
         id <- survey("nss", year = year)$id 
         index <- NULL
         for (i in 1:length(id)) index <- c(index, grep(toupper(id[i]), toupper(file)))
         file <- file[index]
      }
      if (source == "gulf.data"){ 
         if (length(file) > 0){
            index <- rep(FALSE, length(file))
            for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
            file <- file[index]         
         }
      }
   }
   
   # Find cruise:
   if (!missing(cruise)){
      if (source == "ascii"){ 
         index <- NULL
         for (i in 1:length(cruise)) index <- c(index, grep(toupper(cruise[i]), toupper(file)))
         file <- file[index] 
      }
   }
      
   # Empty search:
   if (length(file) == 0)  return(NULL) else return(file)
}

#' @describeIn locate.nss Locate Northumberland Strait survey catch data files.
#' @export locate.nsscat
locate.nsscat <- function(x, year, source = "gulf.data", cruise, ...){
   file <- NULL
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)) file <- x
   }
   
   # Data source is 'gulf.data' package:
   if ((length(file) == 0) & (source == "gulf.data")) file <- locate(package = "gulf.data", file = c("nss", "cat", "csv"), ...)
   
   # Use DFO drive as data source:
   if (length(file) == 0) source <- "ascii"
   if (source == "ascii"){
      if ("path" %in% names(list(...))) path <- list(...)$path else path <- options()$gulf.path$nss
      file <- dir(path = path, pattern = "^rv.[0-9]+c.new", full.names = TRUE)
   } 
   
   # Subset by year:
   if (!missing(year)){
      if (source == "ascii"){ 
         id <- survey("nss", year = year)$id 
         index <- NULL
         for (i in 1:length(id)) index <- c(index, grep(toupper(id[i]), toupper(file)))
         file <- file[index]
      }
      if (source == "gulf.data"){ 
         if (length(file) > 0){
            index <- rep(FALSE, length(file))
            for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
            file <- file[index]         
         }
      }
   }
   
   # Find cruise:
   if (!missing(cruise)){
      if (source == "ascii"){ 
         index <- NULL
         for (i in 1:length(cruise)) index <- c(index, grep(toupper(cruise[i]), toupper(file)))
         file <- file[index] 
      }
   }
      
   # Empty search:
   if (length(file) == 0)  return(NULL) else return(file)
}

#' @describeIn locate.nss Locate Northumberland Strait survey length data files.
#' @export locate.nsslen
locate.nsslen <- function(x, year, source = "gulf.data", cruise, ...){
   file <- NULL
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)) file <- x
   }
   
   # Data source is 'gulf.data' package:
   if ((length(file) == 0) & (source == "gulf.data")) file <- locate(package = "gulf.data", file = c("nss", "len", "csv"), ...)
   
   # Use DFO drive as data source:
   if (length(file) == 0) source <- "ascii"
   if (source == "ascii"){
      if ("path" %in% names(list(...))) path <- list(...)$path else path <- options()$gulf.path$nss
      file <- dir(path = path, pattern = "^rv.[0-9]+l.new", full.names = TRUE)
   } 
   
   # Subset by year:
   if (!missing(year)){
      if (source == "ascii"){ 
         id <- survey("nss", year = year)$id 
         index <- NULL
         for (i in 1:length(id)) index <- c(index, grep(toupper(id[i]), toupper(file)))
         file <- file[index]
      }
      if (source == "gulf.data"){ 
         if (length(file) > 0){
            index <- rep(FALSE, length(file))
            for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
            file <- file[index]         
         }
      }
   }
   
   # Find cruise:
   if (!missing(cruise)){
      if (source == "ascii"){ 
         index <- NULL
         for (i in 1:length(cruise)) index <- c(index, grep(toupper(cruise[i]), toupper(file)))
         file <- file[index] 
      }
   }
   
   # Empty search:
   if (length(file) == 0)  return(NULL) else return(file)
}

#' @describeIn locate.nss Locate Northumberland Strait survey biological data files.
#' @export locate.nssbio
locate.nssbio <- function(x, year, source = "gulf.data", cruise, ...){
   file <- NULL
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)) file <- x
   }
   
   # Data source is 'gulf.data' package:
   if ((length(file) == 0) & (source == "gulf.data")) file <- locate(package = "gulf.data", file = c("nss", "bio", "csv"), ...)
   
   # Use DFO drive as data source:
   if (length(file) == 0) source <- "ascii"
   if (source == "ascii"){
      if ("path" %in% names(list(...))) path <- list(...)$path else path <- options()$gulf.path$nss
      print(path)
      if (!file.exists(path)) path <- getwd()
      file <- dir(path = path, recursive = TRUE, pattern = "^rv.[0-9]+b.new", full.names = TRUE)
   } 
   
   # Subset by year:
   if (!missing(year)){
      if (source == "ascii"){ 
         id <- survey("nss", year = year)$id 
         index <- NULL
         for (i in 1:length(id)) index <- c(index, grep(toupper(id[i]), toupper(file)))
         file <- file[index]
      }
      if (source == "gulf.data"){ 
         if (length(file) > 0){
            index <- rep(FALSE, length(file))
            for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
            file <- file[index]         
         }
      }
   }
   
   # Find cruise:
   if (!missing(cruise)){
      if (source == "ascii"){ 
         index <- NULL
         for (i in 1:length(cruise)) index <- c(index, grep(toupper(cruise[i]), toupper(file)))
         file <- file[index] 
      }
   }
   
   # Empty search:
   if (length(file) == 0)  return(NULL) else return(file)
}
