#' @title Read AZMP data
#' 
#' @description This function reads Atlantic Zonal Monitoring Program (AZMP) oceanographic data.
#' 
#' @param x Year or file name.
#' @param file File name(s)
#' @param station AZMP station identifier.
#' @param year Year.
#' @param month Month>
#' 
#' @return Returns a data.frame with date, time, latitude and longitude fields.
#' 
#' @seealso \code{\link{probe}}

#' @export read.azmp
read.azmp <- function(x, file, station, year, month, ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x))   year <- x
      if (is.character(x)) file <- x
   }
   
   # Read file(s):
   if (!missing(file)){
      if (length(file) == 1){
         # Read file:
         x <- readLines(file)
         
         # Parse header:
         header <- x[grep("^[*]+", x)]
         header <- gsub("^[*]+[ ]+", "", header)
         if (length(grep("END", unlist(header))) == 0){
            print(x)
            return(NULL)  # File is empty.
         } 
         header <- header[-grep("END", header)]
         header <- strsplit(header, ": +")
         names <- tolower(unlist(lapply(header, function(x) x[1])))
         names <- gsub("the actual *", "", names)
         names <- gsub(" is", "", names)
         names <- gsub("and *", "", names)
         names <- gsub(" ", ".", names)
         values <- unlist(lapply(header, function(x) x[2]))
         header <- values
         names(header) <- names
         header[is.na(header)] <- ""
         
         # Convert header coordinates to decimal degrees:
         header["latitude"] <- gsub("[ SNEW]", "", header["latitude"])   
         header["longitude"] <- gsub("[ SNEW]", "", header["longitude"])  
         header["latitude"] <- gsub("^0+", "", header["latitude"])
         header["longitude"] <- gsub("^0+", "", header["longitude"])
         header["longitude"] <- as.numeric(substr(header["longitude"],1,2)) + as.numeric(substr(header["longitude"],3,nchar(header["longitude"]))) / 60
         header["latitude"] <- as.numeric(substr(header["latitude"],1,2)) + as.numeric(substr(header["latitude"],3,nchar(header["latitude"]))) / 60
         
         # Parse variable names:
         vars <- x[grep("^#", x)]
         vars <- vars[-grep("bad_flag", vars)]
         vars <- unlist(lapply(strsplit(vars, " = "), function(x) x[2]))
         units <- gsub("[]]", "", unlist(lapply(strsplit(vars, "[[]"), function(x) x[2])))
         vars <- unlist(lapply(strsplit(vars, ": "), function(x) x[2]))
         vars <- tolower(unlist(lapply(strsplit(vars, "[, :]"), function(x) x[1])))
         
         # Parse data:
         x <- x[(grep("END", x)+1):length(x)]
         x <- strsplit(x, " ")
         data <- NULL 
         for (i in 1:length(vars)){
            tmp <- data.frame(as.numeric(unlist(lapply(x, function(x) x[i]))))
            if (i == 1) data <- tmp else data <- cbind(data, tmp)
         }
         names(data) <- vars
         
         # Saunders & Fofonoff algorithm to compute depth (m) from pressure (db):
         lat <- gsub("[ SN]", "", header["latitude"])      # latitude in degrees.
         lat <- as.numeric(substr(lat,1,2)) + as.numeric(substr(lat,3,nchar(lat))) / 60
         P <- data$pressure   # Hydrostatic pressure in decibars
         xx <- sin(abs(lat) / 57.29578)^2
         gr <- 9.780318 * (1.0 + (5.2788E-3 + 2.36E-5 * xx) * xx) + 1.092E-6 * P
         depthterm <- (((-1.82E-15 * P + 2.279E-10) * P - 2.2512E-5) * P + 9.72659) * P
         data <- cbind(data.frame(depth = depthterm / gr), data)
         vars <- names(data)
         units <- c("meters", units)
         names(units) <- names(data)
         
         attr(data, "header") <- header
         attr(data, "units")  <- units
         
         return(data)
      }else{
         # Read multiple files:
         units <- NULL
         data <- NULL
         for (i in 1:length(file)){
            cat(paste0("Reading ", i, " of ", length(file), " : '", file[i], "'\n"))
            
            # Read single file:
            tmp <- read.azmp(file = file[i])  
            
            if (!is.null(tmp)){
               units <- unique(c(units, attr(tmp, "units")))
               
               # Attach date, time and coordinate data:
               header <- attr(tmp, "header")
               tmp$date      <- as.character(header[["date"]])
               tmp$longitude <- as.numeric(header[["longitude"]])
               tmp$latitude  <- as.numeric(header[["latitude"]])
               tmp$station   <- header[["station"]]
               vars <- c("date", "station", "longitude", "latitude")
               tmp <- tmp[c(vars, setdiff(names(tmp), vars))]
               
               if (is.null(data)){
                  data <- tmp
               }else{
                  tmp[setdiff(names(data), names(tmp))] <- NA
                  data[setdiff(names(tmp), names(data))] <- NA
                  data <- rbind(data, tmp)
               }               
            }
         }
         
         return(data)
      }
   }
   
   if (missing(file)){
      # Get list of available stations:   
      stations <- unlist(strsplit(readLines("http://mixing.ent.dfo-mpo.ca/cgi-bin/viewvc.cgi/cvs/VikingCTD/"), " "))
      stations <- stations[grep("name", stations)]
      stations <- unlist(lapply(strsplit(stations, '\"'), function(x) x[2]))
      stations <- setdiff(stations, c("generator", "pathrev", "dirlist"))
      
      # Parse station argument:
      station <- stations[grep(station, tolower(stations))]
      if (length(station) != 1) stop("'station' not found.")
      
      # Get list of available years:
      years <- unlist(strsplit(readLines(paste0("http://mixing.ent.dfo-mpo.ca/cgi-bin/viewvc.cgi/cvs/VikingCTD/", station, "/")), " "))   
      years <- years[grep("name", years)]
      years <- unlist(lapply(strsplit(years, '\"'), function(x) x[2]))
      years <- as.numeric(setdiff(years, c("generator", "pathrev", "dirlist")))
      
      # Parse year argument:
      year <- year[year %in% years]
      if (length(year) == 0) stop("'year' not found.")
      
      # Get list of available files:
      name <- paste0("http://mixing.ent.dfo-mpo.ca/cgi-bin/viewvc.cgi/cvs/VikingCTD/", station, "/", year, "/1-cnv/")
      files <- unlist(strsplit(readLines(name), " "))
      files <- files[grep("name", files)]
      files <- unlist(lapply(strsplit(files, '\"'), function(x) x[2]))
      files <- files[grep("cnv", files)]
      ix <- grep("lowres", files)
      if (length(ix) > 0) files <- files[-ix]

      if (!missing(month)){
         dates <- unlist(lapply(strsplit(files, "_"), function(x) x[2]))
         ix <- (month(dates) %in% month)
         files <- files[ix]
      }
         
      # Complete file names:
      files <- paste0(name, files, "?view=co")
      
      # Read files:
      x <- read.azmp(file = files)
   }
   
   return(x)
}
