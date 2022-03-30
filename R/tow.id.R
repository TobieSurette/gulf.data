#' @title Extract Tow Identifier
#' 
#' @description Functions to extract or determine trawl tow identifiers.
#' 
#' @param x Data object.
#' @param method Character string specifying the method to be used when determining the tow identification number for a 
#'               \code{probe} object. Available methods are \sQuote{time} and \sQuote{latlong}. Available options are 
#'               \sQuote{observed}, \sQuote{file name}, \sQuote{header}, and \sQuote{time}. 
#' @param max.distance Numeric valiue specifying the maximum distance tolerance (in kilometers) when determining the tow 
#'                     identification number for a \code{probe} object from lat-lon coordinates.         
#' 
#' #' @examples 
#' files <- locate.minilog(2018)
#' tow.id(read.minilog(files[200]))          
#' 

#' @export tow.id
tow.id <- function(x, ...) UseMethod("tow.id")

#' @describeIn tow.id Default \sQuote{tow.id} method.
#' @rawNamespace S3method(tow.id,default)
tow.id.default <- function(x, ...){
   v <- attr(x, "tow.id")
   if (is.null(v)) if ("tow.id" %in% names(x)) v <- x$tow.id
   return(v)
} 

#' @describeIn tow.id \sQuote{tow.id} \sQuote{probe} method.
#' @rawNamespace S3method(tow.id,probe)
tow.id.probe <- function(x, method, ...){
   if (missing(method)) return(tow.id.default(x,))
   
   # Parse 'method' argument:
   if (!missing(method)) method <- match.arg(tolower(method), c("time", "latlong"))
   
   # Determine project study year:                                          
   year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))
   if (is.null(year)) stop("Unable to determine study year.")
   if (length(year) > 1) stop("Multiple years in probe dataset.")
   
   # Load project sampling data:
   if (gulf.metadata::project(x) == "scs") y <- read.scsset(year)
   
   # Match using time stamps:
   if (method == "time"){
      t <- abs(difftime(mean(time(x)), start.time(y), units = "mins"))
      v <- y$tow.id[which.min(t)] 
   }
   
   # Match using mean coordinate values:
   if (method == "latlong"){
      lat <- lat(x)
      lon <- lon(x)
      if (!is.null(lat) & !is.null(lon)){
         d <- distance(mean(lon, na.rm = TRUE), mean(lat, na.rm = TRUE), lon(y), lat(y))[1,]
         v <- y$tow.id[which.min(d)] 
      }
   }
   
   return(v)
}

#' @describeIn tow.id \sQuote{tow.id} sQuote{star.oddi} method.
#' @rawNamespace S3method(tow.id,star.oddi)
tow.id.star.oddi <- function(x, method, ...){
   
   if ("tow.id" %in% names(x)) return(x$tow.id)
   
   header <- header(x)
   if (length(header) > 0){
      if ("tow.id" %in% names(header)) return(header[[which(names(header) == "tow.id")]])
      if ("file.name" %in% names(header)){
         if (length(grep("GP", header["file.name"])) > 0){
            file <- strsplit(header["file.name"], "/")[[1]]
            file <- file[grep("GP", file)] 
            file <- gsub(".DAT", "", file)
            return(file)
         }
      }
   }
   
   # Use time method:
   s <- read.scsset(year = unique(year(time(x))))
   return(s$tow.id[which.min(abs(mean(time(x)) - time(s)))])
   
   return(NULL)
}

#' @describeIn tow.id \sQuote{tow.id} \sQuote{minilog} method.
#' @rawNamespace S3method(tow.id,minilog)
tow.id.minilog <- function(x, method = "observed"){
   # method = "time"
   
   # Parse 'method' argument:
   if (missing(method)) method <- "observed"
   method <- match.arg(gsub(" +", ".", tolower(method)), c("observed", "file.name", "header", "time")) 
   
   # Extract tow ID from header information:
   if (method %in% c("observed", "header")){
      if (!is.null(gulf.metadata::header(x))) v <- as.data.frame(t(gulf.metadata::header(x))) else v <- x
      y <- rep("", nrow(v)) # Result variable.
      
      # Parse study ID:
      ix <- grep("study.id", tolower(names(v)))
      if (length(ix) > 0){
         iv <- (y == "") & (v[, ix] != "") & !is.na(v[, ix])
         y[iv] <- v[iv, ix]
      }
      
      # Parse study description:
      ix <- grep("study.description", tolower(names(v)))
      if (length(ix) > 0){
         iv <- (y == "") & (v[, ix] != "") & !is.na(v[, ix])
         y[iv] <- v[iv, ix]
      }
      
      # Formatting adjustments:
      y <- gsub("[(].*[)]", "", y)
      y <- gulf.utils::deblank(toupper(y))
      
      # Spot corrections:
      y[which(y == "ZONE - F  385-S")] <- "GP385S"
      v <- y
   }
   
   # Determine tow ID from file name:
   if (method == "file.name"){
      ix <- grep("file.name", tolower(names(x)))
      if (length(ix) > 0){
         iy <- grep("GP[0-9]+", toupper(x[, ix]))
         v <- rep("", nrow(x))
         v[iy] <- toupper(x[iy, ix])
         v[iy] <- unlist(lapply(strsplit(v, "GP"), function(x) x[2]))
         v[iy] <- unlist(lapply(strsplit(v, "[.]"), function(x) x[1]))
         v[iy] <- paste0("GP", v[iy])
      }
   }
   
   if (method == "time"){
      # Load scs tow data:
      y <- read.scsset(year = unique(year(x)))
      x <- gulf.utils::expand(x)
      
      # Define grouping variables:
      if ("file.name" %in% names(x)) vars <- c("date", "file.name") else vars <- c("date", names(x)[grep("study", names(x))])
      
      # Find tow ID from survey tows using time match:
      ux <- unique(x[vars])
      v <- rep(NA, nrow(ux))
      for (i in 1:nrow(ux)){
         xx <- x[which(x[, vars[1]] == ux[i, vars[1]] & x[, vars[2]] == ux[i, vars[2]]), ]
         d <- abs(difftime(median(time(xx[xx$depth > 20, ])), time(y), units = "mins"))
         if (min(d) < 60) v[i] <- y$tow.id[which.min(d)]
      }
      
      ix <- match(x[vars], ux)
      v <- v[ix]
   }
   
   return(v)
}

#' @describeIn tow.id \sQuote{tow.id} \sQuote{scscat} method.
#' @rawNamespace S3method(tow.id,scscat)
tow.id.scscat <- function(x, ...){
   if (is.null(x$tow.id)) x$tow.id <- ""
   x$tow.id[is.na(x$tow.id)] <- ""
   ix <- x$tow.id == ""
   if (any(ix)){
      s <- read.scsset(year = as.numeric(unique(substr(unique(x$date[ix]), 1, 4))))
      x$tow.id[ix] <- s$tow.id[gulf.utils::match(x[ix, c("date", "tow.number")], s[c("date", "tow.number")])]
   }
   
   return(x$tow.id)
}

#' @describeIn tow.id \sQuote{tow.id} \sQuote{scsbio} method.
#' @rawNamespace S3method(tow.id,scsbio)
tow.id.scsbio <- function(x, ...){
   if (is.null(x$tow.id)) x$tow.id <- ""
   x$tow.id[is.na(x$tow.id)] <- ""
   ix <- x$tow.id == ""
   if (any(ix)){
      s <- read.scsset(year = as.numeric(unique(substr(unique(x$date[ix]), 1, 4))))
      x$tow.id[ix] <- s$tow.id[gulf.utils::match(x[ix, c("date", "tow.number")], s[c("date", "tow.number")])]
   }
   
   return(x$tow.id)
}

#' @describeIn tow.id \sQuote{tow.id} \sQuote{scslen} method.
#' @rawNamespace S3method(tow.id,scslen)
tow.id.scslen <- function(x, ...){
   if (is.null(x$tow.id)) x$tow.id <- ""
   x$tow.id[is.na(x$tow.id)] <- ""
   ix <- x$tow.id == ""
   if (any(ix)){
      s <- read.scsset(year = as.numeric(unique(substr(unique(x$date[ix]), 1, 4))))
      x$tow.id[ix] <- s$tow.id[gulf.utils::match(x[ix, c("date", "tow.number")], s[c("date", "tow.number")])]
   }
   
   return(x$tow.id)
}
