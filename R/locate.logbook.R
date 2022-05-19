#' @title Locate Fishery Logbook Data
#' @rdname locate.logbook
#' 
#' @description Functions to locate fishery logbook data, such as snow crab fishery data and 
#'              Zonal Information File Format (ZIFF) data.
#' 
#' @param x Data object.
#' @param year Survey year.
#' @param species Species to be targeted (e.g. "snow crab").
#' @param path Character string specifying the location of ZIFF data.
#' @param ... Not used.
#' 
#' @examples 
#' # Locate snow crab fishery logbook data:
#' locate.logbook(species = "snow crab")                        # Find all snow crab fishery logbook data files.
#' locate.logbook(2013:2015, species = "snow crab")             # Find all snow crab fishery logbook data files from 2013-2015.
#'
#' # Locate groundfish and pelagic fishery logbook data:
#' locate.logbook()                                             # Find all available ziff files.
#' locate.ziff()                                                # Find all available ziff files.
#' locate.ziff(year = 2000:2008, region = "g")                  # Find Gulf region ziff files from 2000-2008.     
#' locate.ziff(year = 2002:2003, region = c("Gulf", "Quebec"))  # Find Gulf and Quebec regions ziff files from 2002-2003. 

#' @export locate.logbook
locate.logbook <- function(x, year, species, ...){ 
   # Parse 'year' argument:
   if (!missing(x)) if (is.numeric(x)) year <- x
   if (missing(year)) year <- NULL
   
   # Parse 'species' argument:
   if (!missing(species)){
      if (is.numeric(species)) species <- tolower(species(species)[1])
      species <- match.arg(tolower(species), "snow crab")
      if (species == "snow crab"){
         path <- options("gulf.path")[[1]]$snow.crab$logbook
         files <- dir(path = path, full.names = TRUE, pattern = "*.csv")
         if (length(year > 0)){
            ix <- NULL
            for (i in 1:length(year)) ix <- c(ix, grep(year[i], files))
            files <- files[ix]
         }
         return(files)
      } 
   }
   
   return(locate.ziff(year, ...))
}

#' @rdname locate.logbook
#' @export locate.ziff
locate.ziff <- function(x, year, path = options("gulf.path")[[1]]$groundfish$ziff, region, ...){ 
   # Parse 'year' argument:
   if (!missing(x)) if (is.numeric(x)) year <- x
   if (missing(year)) year <- NULL
   
   # Parse 'path' argument:
   path <- paste0(path, "R versions")
   
   # Parse 'region' argument:
   if (!missing(region)){
      region <- substr(tolower(as.character(region)),1,1)
      region <- region[region %in% c("g", "n", "q", "s")]
   }else{
      region <- NULL
   }
   
   # Locate files:
   files <- locate(path = path)
   files <- files[grep("[gnqs]_raw_[12][0-9][0-9][0-9].Rdata", files)]

   # Subset by region:
   if (length(region) > 0){
      ix <- NULL
      for (i in 1:length(region)) ix <- c(ix, grep(paste0(region[i], "_raw"), files))
      files <- files[ix]
   }

   # Subset by year:
   if (length(year) > 0){
      ix <- NULL
      for (i in 1:length(year)) ix <- c(ix, grep(paste0("raw_", year[i]), files))
      files <- files[ix]
   }
   
   return(files)
}




