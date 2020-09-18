#' Update Data
#' 
#' @name update
#' 
#' @description Functions to update \code{gulf.data} package data.
#' 
#' @param year  Project year(s) to be updated.   
#' @param path  Character string specifying the path to read or write snow crab biological data.
#' @param Rfile Logical value specifying whether to write an \code{R} data format file when 
#'              updating snow crab biological data.
#' @param csv Logical value specifying whether to write a comma-separated (\code{csv}) format file 
#'            when updating snow crab biological data.
#'                          
#' @section Methods:
#' \describe{
#'    \item{\code{update.scsset}}{Create an \code{scsset} object.}
#'    \item{\code{update.scsbio}}{Update snow crab survey biological data repositories.}
#' }
#' 
#' @seealso \code{\link{scsset}}, \code{\link{scsbio}}

#' @rdname update
#' @rawNamespace S3method(update,scsset)
#' @export update.scsset
update.scsset <- function(x, year, path = getwd(), package = TRUE, ...){
   # Parse 'x' as 'year':
   if (is.numeric(x) & missing(year)) year <- x
   
   # Get current year:
   if (missing(year)){
      tmp <- strsplit(as.character(date()), " ")[[1]]
      year <- as.numeric(tmp[length(tmp)])
   } 
   
   # Read trawl survey event times:
   touchdown.times <- read.csv(paste0(options()$gulf.path$snow.crab, "/Databases/Snow crab survey/scsset/touchdown.times.csv"), header = TRUE, stringsAsFactors = FALSE)
   liftoff.times <- read.csv(paste0(options()$gulf.path$snow.crab, "/Databases/Snow crab survey/scsset/liftoff.times.csv"), header = TRUE, stringsAsFactors = FALSE)
   haul.times <- read.csv(paste0(options()$gulf.path$snow.crab, "/Databases/Snow crab survey/scsset/haul.times.csv"), header = TRUE, stringsAsFactors = FALSE)
   
   # Read data files:
   for (i in 1:length(year)){
      # Read data:
      x <- read.scsset(year[i], source = "ascii")

      # Add year column for row matching:
      x$year <- as.numeric(substr(as.character(date(x)), 1, 4))

      # Trawl haul times:
      index <- match(x[c("year", "tow.id")], haul.times[c("year", "tow.id")])
      x$haul.time <- "        "
      x$haul.time[which(!is.na(index))] <- haul.times$time[index[which(!is.na(index))]]
      
      # Trawl touchddown times:
      index <- match(x[c("year", "tow.id")], touchdown.times[c("year", "tow.id")])
      x$touchdown.time <- "        "
      x$touchdown.time[which(!is.na(index))] <- touchdown.times$time[index[which(!is.na(index))]]
 
      # Trawl liftoff times:
      index <- match(x[c("year", "tow.id")], liftoff.times[c("year", "tow.id")])
      x$liftoff.time <- "        "
      x$liftoff.time[which(!is.na(index))] <- liftoff.times$time[index[which(!is.na(index))]]
            
      # Remove year:
      x <- x[setdiff(names(x), "year")]
      
      # Write to file:
      file <- paste0(path, "/inst/extdata/scs.set.", year[i], ".csv")
      cat(paste0("Writing to : '", file, "'\n"))
      write.csv(x, file = file, row.names = FALSE)
   }
}

#' @rdname update
#' @rawNamespace S3method(update,scsbio)
#' @export update.scsset
update.scsbio <- function(year, path = getwd(), ...){
   # Loop over years:
   for (i in 1:length(year)){
      # Read data:
      x <- read.scsbio(year[i], ...)
      
      # Add date:
      x <- cbind(data.frame(date = as.character(date(x)), stringsAsFactors = FALSE), x)
      x <- x[setdiff(names(x), c("year", "month", "day"))]
      
      # Add tow ID:
      y <- read.scsset(year[i])
      index <- match(x[c("date", "tow.number")], y[c("date", "tow.number")])
      x$tow.id <- y$tow.id[index]
      x$tow.id[is.na(x$tow.id)] <- ""
      
      # Write to file:
      file <- paste0(path, "/inst/extdata/scs.bio.", year[i], ".csv")
      cat(paste0("Writing to : '", file, "'\n"))
      write.csv(x, file = file, row.names = FALSE)      
   }
}

