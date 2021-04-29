#' @title Locate Probe Data
#' 
#' @description Locate data files for various types of probes.
#' 
#' @param x Study year, data file name or tow identification tag.
#' @param probe Probe name. See \code{\link[gulf.data]{probe}} for available options.
#' @param project Project name. See \code{\link[gulf.data]{project}} for available options.
#' @param location Probe location. For trawl probes, options are \code{headline} and \code{footrope}.
#' @param remove Character string specifying terms to remove from search results.
#' @param year Study year.
#' @param tow.id Tow identification tag for trawl studies and surveys.
#' 
#' @examples 
#' locate.probe(2020, "esonar", project = "scs")                             # 2020 Snow crab survey eSonar data files.
#' locate.probe(2018, "minilog", project = "scs")                            # 2018 snow crab survey Minilog data files.
#' locate.probe(2018, "star.oddi", project = "scs", location = "headline")   # 2018 snow crab survey Star Oddi data files.
#' locate.probe(probe = "scanmar", project = "scs")                          # All snow crab survey Scanmar files.
#'
#' locate.minilog()                     # Find all available Minilog data files.
#' locate.minilog(1997)                 # Find Minilog data files for the 1997 snow crab survey.
#' locate.minilog(1997:1999)            # Find Minilog data files for the 1997-1997 snow crab survey.
#'
#' locate.star.oddi(2020, probe = "headline") 
#' locate.star.oddi(2020, probe = "tilt", tow.id = "GP354F")
#' 
#' #' # Global searches:
#' locate.scanmar()     # Find all available Scanmar data files.
#' locate.scanmar(1990) # Find Scanmar data files for the 1990 snow crab survey.
#' locate.scanmar(1990:1994) # Find Scanmar data files for the 1990-1994 snow crab survey.
#' 
#' # Specific searches:
#' locate.scanmar(1990, tow.id = 100)
#' locate.scanmar(1990, tow.id = "100")
#' locate.scanmar(1990, tow.id = "S90100")
#' locate.scanmar(tow.id = "S90100")
#' 
#' locate.netmind()     # Find all available Netmind data files.
#' locate.netmind(1999) # Find Netmind data files for the 1990 snow crab survey.
#' locate.netmind(1999:2004) # Find Netmind data files for the 1990-1994 snow crab survey.
#' 
#' # Specific searches:
#' locate.netmind(1999, tow.id = 100)
#' locate.netmind(1999, tow.id = "100")
#' locate.netmind(1999, tow.id = "S90100")
#' locate.netmind(tow.id = "355")
#'  
#' locate.esonar(2020) # Find snow crab survey eSonar data from 2020.
#' locate.esonar(2020, tow.id = "GP354F")
#' 
#' @seealso code{\link[gulf.metadata]{project}}, code{\link[gulf.data]{probe}}

#' @export
locate.probe <- function(x, probe, project = "scs", location = "headline", remove, year, tow.id, ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
   }
   
   #' Parse arguments:
   probe   <- probe(probe)
   project <- project(project)
   location <- match.arg(tolower(location), c("headline", "footrope", "tilt"))
   
   # Minilog 
   if (probe == "minilog"){
      if (project == "scs"){
         files <- locate(package = "gulf.trawl.data", keywords = c("minilog"), ...)
         files <- files[union(grep("asc", tolower(files)), grep(".csv$", tolower(files)))]    
      }
   }
   
   # Star Oddi:
   if (probe == "star.oddi"){
      remove  = c("test", "lost", "NA")
       
      # Parse 'location' argument:
      location <- tolower(gsub("[. ]", "", location))
      location <- location[location %in% c("headline", "footrope", "tilt")]
   
      # Locate candidate files:
      files <- NULL
      for (i in 1:length(location)){
         files <- c(files, locate(package = "gulf.trawl.data", file = "*.DAT", keywords = c(project, "star", "oddi", location[i]), ...))
      }
   }
   
   # Scanmar
   if (probe == "scanmar"){
      if (project == "scs"){
         # Load set of file names:
         files <- locate(package = "gulf.trawl.data", keywords = c("scanmar"), ...)
         ix <- unique(c(grep("[.]txt", tolower(files)), grep("[.]scd", tolower(files)), grep("[.]csv", tolower(files))))
         files <- files[ix]
   
         # Valid file names:
         valid <- c("pos", "s[0-9][0-9]", "sta")
         ix <- union(grep("pos", tolower(files)), grep("s[0-9][0-9]", tolower(files)))
         ix <- union(ix, grep("sta", tolower(files)))
         ix <- union(ix, grep(".csv$", tolower(files)))
         files <- files[ix]
      }
   }
   
   # Netmind
   if (probe == "netmind"){
      if (project == "scs"){
         # Load set of file names:
         files <- locate(package = "gulf.trawl.data", keywords = c("netmind"), ...)
         ix <- unique(c(grep("[.]txt", tolower(files)), grep("[.]scd", tolower(files)), grep("[.]csv", tolower(files))))
         files <- files[ix]
   
         # Valid file names:
         valid <- c("pos", "s[0-9][0-9]", "sta")
         ix <- union(grep("pos", tolower(files)), grep("s[0-9][0-9]", tolower(files)))
         ix <- union(ix, grep("sta", tolower(files)))
         ix <- union(ix, grep(".csv$", tolower(files)))
         ix <- union(ix, grep(".txt$", tolower(files)))
         files <- files[ix]
      }
   }
   
   # eSonar:
   if (probe == "esonar"){
      if (project == "scs"){  
         # Load set of file names:
         files <- locate(package = "gulf.trawl.data", file = "*.csv", keywords = c("scs", "esonar"), ...)
      }
   }
   
   # Notus:
   if (probe == "notus"){
      if (project == "scs"){  
         # Load set of file names:
         files <- locate(package = "gulf.trawl.data", file = "*.txt", keywords = c("nss", "notus"), ...)
      }
   }
   
   # Remove files:
   if (!missing(remove)){
      if (length(remove) == 1) if (remove == FALSE) remove <- NULL
      remove <- remove[remove != "" & !is.na(remove)]
      if ((length(files) > 0) & (length(remove) > 0)){
         ix <- NULL
         for (i in 1:length(remove)) ix <- c(ix, grep(tolower(remove[i]), tolower(files)))
         if (length(ix) > 0) files <- files[-ix]
      }
   }
   
   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(unique(year))
      ix <- NULL
      for (i in 1:length(year)) ix <- c(ix, grep(year[i], files))
      files <- unique(files[ix])
   }
   
   # Target tow ID:
   if (!missing(tow.id)){
      if (!is.null(tow.id)){
         if ((project == "scs")){
            if (probe == "minilog"){
               h <- header.minilog(files)
               y <- rep("", nrow(h)) # Result variable.
      
               # Parse study ID:
               ix <- grep("study.id", tolower(names(h)))
               if (length(ix) > 0){
                  iv <- (y == "") & (h[, ix] != "") & !is.na(h[, ix])
                  y[iv] <- h[iv, ix]
               }
      
               # Parse study description:
               ix <- grep("study.description", tolower(names(h)))
               if (length(ix) > 0){
                  iv <- (y == "") & (h[, ix] != "") & !is.na(h[, ix])
                  y[iv] <- h[iv, ix]
               }
      
               # Formatting adjustments:
               y <- gsub("[(].*[)]", "", y)
               y <- gulf.utils::deblank(toupper(y))
      
               # Spot corrections:
               y[which(y == "ZONE - F  385-S")] <- "GP385S"
               h <- y
               files <- files[toupper(h) %in% toupper(tow.id)]
            
               tow.id <- NULL
            }else{
               tow.id <- unique(tow.id)
               ix <- NULL
               for (i in 1:length(tow.id)) ix <- c(ix, grep(tow.id[i], files))
               files <- unique(files[ix])               
            }
         }
      }
   }
   
   # Only keep unique file names:
   files <- unique(files)
   
   return(files)
}

#' @export locate.minilog
#' @export locate.star.oddi
#' @export locate.scanmar
#' @export locate.netmind
#' @export locate.esonar
locate.minilog   <- function(x, ...) UseMethod("locate.minilog")
locate.star.oddi <- function(x, ...) UseMethod("locate.star.oddi")
locate.scanmar   <- function(x, ...) UseMethod("locate.scanmar")
locate.netmind   <- function(x, ...) UseMethod("locate.netmind")
locate.esonar    <- function(x, ...) UseMethod("locate.esonar")
locate.notus     <- function(x, ...) UseMethod("locate.notus")

#' @describeIn locate.probe Default method for locating Minilog probe data files.
#' @rawNamespace S3method(locate.minilog,default)
locate.minilog.default <- function(x, ...) return(locate.probe(x, probe = "minilog", remove = c("reject", "test", "invalid", "DS_Store"), ...))

#' @describeIn locate.probe Locate Minilog associated with snow crab survey tow data.
#' @rawNamespace S3method(locate.minilog,scsset)
locate.minilog.scsset <- function(x, ...) return(locate.minilog(year = gulf.utils::year(x), tow.id = gulf.data::tow.id(x), ...))

#' @describeIn locate.probe Default method for locating Star Oddi probe data files.
#' @rawNamespace S3method(locate.star.oddi,default)
locate.star.oddi.default <- function(x, ...) return(locate.probe(x, probe = "star.oddi", ...))

#' @describeIn locate.probe \code{scsset} method for locating Star Oddi data files.
#' @rawNamespace S3method(locate.star.oddi,scsset)
locate.star.oddi.scsset <- function(x, ...) return(locate.star.oddi(year = gulf.utils::year(x), tow.id = x$tow.id, ...))

#' @describeIn locate.probe Default method for locating Scanmar acoustic data files.
#' @rawNamespace S3method(locate.scanmar,default)
locate.scanmar.default <- function(x, ...) return(locate.probe(x, probe = "scanmar", remove = c("reject", "test", "invalid"), ...))

#' @describeIn locate.probe Default method for locating Netmind acoustic data files.
#' @rawNamespace S3method(locate.netmind,default)
locate.netmind.default <- function(x, ...)  return(locate.probe(x, probe = "netmind", remove = c("reject", "test", "invalid"), ...))

#' @describeIn locate.probe Default method for locating eSonar data files.
#' @rawNamespace S3method(locate.esonar,default)
locate.esonar.default <- function(x, ...) return(locate.probe(x, probe = "esonar", remove = c("test", "use raw"), ...))

#' @describeIn locate.probe \code{scsset} method for locating eSonar data files.
#' @rawNamespace S3method(locate.esonar,scsset)
locate.esonar.scsset <- function(x, ...) return(locate.esonar(year = unique(year(x)), tow.id = unique(x$tow.id, ...)))
