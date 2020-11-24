#' Locate Gulf Data Files
#' 
#' @description Functions to locate various data file(s) from different studies and surveys.
#' 
#' @param x Data object.
#' @param year Study or survey year.
#' @param source Character string specifying the data source for data files. Options are \sQuote{ascii} or a gulf 
#'               package name (e.g. \sQuote{gulf.data}).
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' @param tow.id Character string(s) specifying a tow identifier(s) (e.g. \sQuote{GP354F}).
#' @param probe Character string specifying probe type or location. For example \sQuote{headline} specifies the Star Oddi 
#'              probe attached to the trawl headline on snow crab surveys.
#' 
#' @examples 
#' locate.scsset()          # Find all snow crab survey set data files.
#' locate.scsset(2020)      # Find specific year.
#' locate.scsset(2010:2020) # Find set of years.
#'  
#' locate.esonar(2020) # Find snow crab survey eSonar data from 2020.
#' locate.esonar(2020, tow.id = "GP354F")
#' 
#' locate.star.oddi(2020, probe = "headline", source = 'ascii') 
#' locate.star.oddi(2020, probe = "footrope", source = 'ascii', tow.id = "GP354F")

#' @describeIn locate Locate snow crab survey set data files.
#' @export locate.scsset
locate.scsset <- function(x, year, source = "gulf.data", ...){
   if (!missing(x)) if (is.numeric(x)) year <- x

   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      path <- paste0(path, "/Trawl Data/South Western Gulf/Tow Data")
      file <- locate(path = path, pattern = c("Tows [0-9]{4,4}.txt"))
   }

   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", file = c("scs", "set", "csv"), ...)

   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }

   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @describeIn locate Locate Northumberland Strait survey set data files.
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

#' @describeIn locate Locate snow crab survey biological data files.
#' @export locate.scsbio
locate.scsbio <- function(x, year, source = "gulf.data", remove = "bad", ...){
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if (missing(year)) year <- gulf.utils::year(x)
   }
   
   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      if (missing(year)){
         path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      }else{
         path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year)
      }
      path <- paste0(path, "/Trawl Data/South Western Gulf/Raw Data")
      file <- locate(path = path, file = "*.txt")
   }
   
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", file = c("scs", "bio", "csv"), ...)

   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Remove irrelevant files:
   if (length(remove) > 0){
      index <- rep(TRUE, length(file))
      for (i in 1:length(remove)) index[grep(tolower(remove[i]), tolower(file))] <- FALSE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @describeIn locate Locate snow crab survey biological data files.
#' @export locate.scscat
locate.scscat <- function(x, year, source = "gulf.data", remove = "bad", ...){
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if ("year" %in% names(x)) year <- sort(unique(x$year))
      if ("date" %in% names(x)) year <- as.numeric(substr(x$date, 1, 4))
   }
   
   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      if (missing(year)){
         path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      }else{
         path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year)
      }
      path <- paste0(path, "/Trawl Data/South Western Gulf/By-catch")
      file <- locate(path = path, file = "scs*c.csv")
   }
   
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", file = c("scs", "cat", "csv"), ...)
   
   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Remove irrelevant files:
   if (length(remove) > 0){
      index <- rep(TRUE, length(file))
      for (i in 1:length(remove)) index[grep(tolower(remove[i]), tolower(file))] <- FALSE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @describeIn locate Locate Northumberland Strait survey catch data files.
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

#' @describeIn locate Locate snow crab survey biological data files.
#' @export locate.scslen
locate.scslen <- function(x, year, source = "gulf.data", remove = "bad", ...){
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if ("year" %in% names(x)) year <- sort(unique(x$year))
      if ("date" %in% names(x)) year <- as.numeric(substr(x$date, 1, 4))
   }
   
   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      if (missing(year)){
         path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      }else{
         path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year)
      }
      path <- paste0(path, "/Trawl Data/South Western Gulf/By-catch")
      file <- locate(path = path, file = "scs*l.csv")
   }
   
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", file = c("scs", "cat", "csv"), ...)
   
   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Remove irrelevant files:
   if (length(remove) > 0){
      index <- rep(TRUE, length(file))
      for (i in 1:length(remove)) index[grep(tolower(remove[i]), tolower(file))] <- FALSE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @describeIn locate Locate Northumberland Strait survey length data files.
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

#' @describeIn locate Locate Northumberland Strait survey biological data files.
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
      file <- dir(path = path, pattern = "^rv.[0-9]+b.new", full.names = TRUE)
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

#' @export locate.star.oddi
locate.star.oddi <- function(x, ...) UseMethod("locate.star.oddi")

#' @rawNamespace S3method(locate.star.oddi,default)
locate.star.oddi.default <- function(x, year, tow.id, full.names = TRUE, probe, remove = c("test", "lost", "NA"), ...){
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
   
   # Parse 'probe' argument:
   if (missing(probe)) probe <- c("headline", "footrope")
   probe <- tolower(probe)
   if (!all(probe %in% c("headline", "tilt", "footrope"))) 
      stop("'probe' must be either 'headline', 'tilt', 'footrope'.")
   if ("tilt" %in% probe) probe <- unique(c("footrope", probe))
   if ("footrope" %in% probe) probe <- unique(c("tilt", probe))
   
   # Load set of file names:
   files <- NULL
   for (i in 1:length(probe)){
      files <- unique(c(files, locate(pattern = "*.DAT", keywords = c("star oddi", probe[i]), ...)))
   }
   
   # Search Shared drive:
   if (length(files) == 0){
      path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year, "/Trawl Data/South Western Gulf/Star Oddi")
      if (!missing(location)) path <- paste0(path, "/", location)
      if (file.exists(options()$gulf.path$snow.crab)) files <- locate(pattern = "*.DAT", path = path)
      
      # Remove redundant files:
      fn <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
      files <- files[setdiff(1:length(files), grep("^[0-9]-", fn))]
   }

   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
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

#' @rawNamespace S3method(locate.star.oddi,scsset)
locate.star.oddi.scsset <- function(x, ...) return(locate.star.oddi(year = as.numeric(substr(gulf.utils::date(x), 1, 4)), tow.id = x$tow.id, ...))

#' @export locate.esonar
locate.esonar <- function(x, ...) UseMethod("locate.esonar")

#' @rawNamespace S3method(locate.esonar,default)
locate.esonar.default <- function(x, year, tow.id, remove = "test", ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
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

#' @rawNamespace S3method(locate.esonar,scsset)
locate.esonar.scsset <- function(x, ...) return(locate.esonar(year = unique(year(x)), tow.id = unique(x$tow.id, ...)))
