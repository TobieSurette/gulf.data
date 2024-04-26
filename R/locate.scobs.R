#' Locate Snow Crab Observer Data
#' 
#' @description Functions to locate snow crab at-sea observer data file(s).
#' 
#' @param x Data object.
#' @param year Fishing year.
#' @param path Data file path.
#' @param type 'sea' or 'port'.
#' @param zone Fishing year.
#' @param source Data source.
#' 
#' @examples 
#' locate.scobs(2010)                            # Find all at-sea sample files for the 2010 snow crab fishery.
#' locate.scobs(2010, type = "port")             # Find all port sample files for the 2010 snow crab fishery.
#' locate.scobs(2010, type = "port", zone = "E") # Find all port sample files from the 2010 snow crab fishery in zone 12E.
#' 

#' @export locate.scobs
locate.scobs <- function(x, year, path = options()$gulf.path$snow.crab$survey, type = "sea", zone, source = "ascii", ...){
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Parse 'zone':
   if (!missing(zone)){
      zone <- match.arg(toupper(zone), c('12', '18', '19', 'E', '12E', '12F', 'F'))
      zone[zone == "12E"] <- "E"
      zone[zone == "12F"] <- "F"
      zone.str <- zone
   }
   
   if (source == "ascii"){
      path <- dir(options()$gulf.path$snow.crab$survey, pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      path <- paste0(path, "/Observer Data/")
      
      # Year subset:
      if (!missing(year) & (length(path) > 0)){
         ix <- rep(FALSE, length(path))
         for (i in 1:length(year)) ix[grep(year[i], path)] <- TRUE
         path <- path[ix]
      }   
      file <- locate(path = path, pattern = "*.txt")
      file <- file[grep("[.]txt$", tolower(file))] 
      file <- file[grep("raw data", tolower(file))]
      
      # Subset by sample type:
      if (length(file) > 0){
         ix <- grep(type, tolower(file))
         if (length(ix) == 0){
            if (type == "port") file <- file[grep("p[0-9]+[.]txt$", tolower(file))]
            if (type == "sea")  file <- file[grep("s[0-9]+[.]txt$", tolower(file))]
         }else{
            file <- file[ix]            
         }
      } 

      # Subset by zone:
      if (!missing(zone) & (length(file) > 0)){
         ix <- NULL
         
         # Zone 12:
         if ("12" %in% zone){
            ix <- unique(c(ix, grep("/zone 12/", tolower(file)), grep("/12/", tolower(file))))
         }

         
         # Zone E:
         if ("E" %in% zone){
            ix <- unique(c(ix, grep("/zone e/", tolower(file)), grep("/zone 12e/", tolower(file)),
                               grep("/e/", tolower(file)), grep("/12e/", tolower(file)),
                               grep("ee", tolower(file))))
         }

         # Zone F:
         if ("F" %in% zone){
            ix <- unique(c(ix, grep("/zone f/", tolower(file)), grep("/zone 12f/", tolower(file)),
                               grep("/f/", tolower(file)), grep("/12f/", tolower(file)),
                               grep("ef", tolower(file))))
         }
         
         # Zone 18:
         if ("18" %in% zone){
            ix <- unique(c(ix, grep("/zone 18/", tolower(file)), grep("/18/", tolower(file)), grep("cb18", tolower(file))))
         }
         
         # Zone 19:
         if ("19" %in% zone){
            ix <- unique(c(ix, grep("/zone 19/", tolower(file)), grep("/19/", tolower(file)), grep("cb19", tolower(file))))
         }
         
         file <- file[ix]
      } 
   }

   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}
