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
#' 
#' @examples 
#' locate.scsset()          # Find all snow crab survey set data files.
#' locate.scsset(2020)      # Find specific year.
#' locate.scsset(2010:2020) # Find set of years.
#' 
#' @seealso \code{\link{scs}}
#' @seealso \code{\link{read.scs}}

#' @describeIn locate.scs Locate snow crab survey set data files.
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

#' @describeIn locate.scs Locate snow crab survey biological data files.
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

#' @describeIn locate.scs Locate snow crab survey biological data files.
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

#' @describeIn locate.scs Locate snow crab survey biological data files.
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

