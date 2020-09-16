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
   
   # Read data files:
   for (i in 1:length(year)){
      # Read data:
      x <- read.scsset(year[i], source = "ascii")
      
      # Write to file:
      file <- paste0(path, "/inst/extdata/scs.set.", year[i], ".csv")
      cat(paste0("Writing to : '", file, "'\n"))
      write.csv(x, file = file, row.names = FALSE)
   }
}

#' @rdname update
#' @rawNamespace S3method(update,scsbio)
#' @export update.scsset
update.scsbio <- function(year, path, Rfile = FALSE, csv = FALSE, ...){
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   flag <- FALSE
   if (!missing(path)) flag <- TRUE
   
   # Loop over years:
   for (i in 1:length(year)){
      if (!flag){
         path <- scsbio.path.str(year = year[i], ...)
         tmp <- strsplit(path, '/')[[1]]
         path <- paste0(tmp[1:which(tmp == "Raw Data")], collapse = "/")
      }
      
      writeable <- Sys.chmod(path = path)
      if (!writeable) stop(paste("Unable to write to: ", path))
      
      # Read data:
      x <- read.scsbio(year = year[i], source = "ascii", ...)
      index <- (x$carapace.width > 0) | !is.na(x$abdomen.width) | !is.na(x$chela.height) | !is.na(x$shell.condition) | 
         !is.na(x$gonad.colour) | !is.na(x$egg.colour) | !is.na(x$eggs.remaining) 
      x[which(index), ]
      
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".Rdata'\n"))
      if (Rfile) save(x, file = paste0(path, "/", "SCS", year[i], ".Rdata"))
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".csv'\n"))
      if (csv) write.table(x, file = paste0(path, "/", "SCS", year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
   }
}

