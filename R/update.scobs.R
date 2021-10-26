update.scobs <- function(year, zone, path, type = "sea", Rfile = TRUE, csv = TRUE, source = "oracle", ...){
   # UPDATE.SCOBS - Update snow crab observer data.
   
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   if (missing(zone)) zone <- c("12", "E", "F", "18", "19")
   zone <- toupper(zone)
   
   # Parse 'souce' argument:
   source <- match.arg(tolower(source), c("oracle", "ascii"))
   
   flag <- FALSE
   if (!missing(path)) flag <- TRUE
   
   # Loop over years:
   for (i in 1:length(year)){
      for (j in 1:length(zone)){
         if (!flag) path <- scobs.path.str(year = year[i], zone = zone[j], type = type)
   
         writeable <- Sys.chmod(path = path)
         if (!writeable) stop(paste("Unable to write to: ", path))
         
         # Read data:
         if (source == "ascii"){
            files <- scobs.file.str(year = year[i], zone = zone[j], type = type)
            #tmp <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
            # files <- files[!(toupper(substr(tmp, 1, 1)) %in% c("G", "J", "Q"))]
            x <- read.scobs(files, ...)
         }
         if (source == "oracle") x <- read.scobs(year = year[i], zone = zone[j], type = type, source = source, ...)
         
         # Write ASCII files, one for each trip:
         trips <- unique(x$trip.number)
         for (k in 1:length(trips)){
            file <- toupper(trips[k])
            file <- paste0(file, ".txt")
            cat(paste0("Writing to : '", paste0(path, "/", file), "'\n"))
            xx <- x[x$trip.number == trips[k], ]
            svars <-  c("trip.number", "trap.number", "crab.number")
            if (any(is.na(xx$trap.number))) svars <- setdiff(svars, "trap.number")
           # print(str(xx[svars]))
            xx <- sort(xx, by = svars)
            write(xx, file = paste0(path, "/", file), overwrite = TRUE)
         }
         
         pathmod <- paste0(strsplit(path, "Raw Data")[[1]][1], "Databases")
         if (!file.exists(pathmod)) dir.create(pathmod) 
         if (zone[j] %in% c("E", "F")) str <- paste0("12", zone[j]) else str <- zone[j]
         if (type == "sea") str <- paste0(str, "S") else str <- paste0(str, "P")
         if (Rfile) save(x, file = paste0(pathmod, "/", str, year[i], ".Rdata"))
         if (csv) write.table(x, file = paste0(pathmod, "/", str, year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
      }
   }
}
