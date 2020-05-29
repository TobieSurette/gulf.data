#' Snow Crab Set/Tow Data
#'
#' @description The \code{scsset} class is a container for Snow Crab Survey Set/Tow data, i.e. 
#'              information about individual tows performed on the snow crab annual survey. 
#'              
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric 
#'          vector specifying the survey years.
#'          
#' @param year Survey year(s) to be loaded.   
#'   
#' @section Functions:
#' \describe{
#'   \item{\code{scsset}}{Generic \code{scsset} method.}
#'   \item{\code{scsset.default}}{Create an \code{scsset} object.}
#'   \item{\code{read.scsset}}{Read snow crab survey set/tow data.}
#'   \item{\code{update.scsset}}{Update  snow crab survey set/tow data repositories.}
#'   \item{\code{summary.scsset}}{Returns a summary of an \code{scsset} object.}
#'   \item{\code{\link{check.scsset}}}{Check \code{scsset} for data issues.}
#' }
#' 
#' @export scsset
#' @export scsset.default
#' @export read.scsset
#' 

#' @rdname scsset
scsset <- function(x, ...) UseMethod("scsset")

#' @rdname scsset
scsset.default <- function(x, ...){
   if ("scsset" %in% class(x)) return(x)
  
   # Define class:
   class(x) <- unique(c("scsset", class(x))) 
   
   # Define attributes:
   key(x) <- c("year", "tow.id")
   units(x, c("longitude", "latitude", "longitude.start.logbook", "longitude.end.logbook", "latitude.start.logbook", "latitude.end.logbook")) <- "degrees"
   units(x, "swept.area") <- "square.meters"
   units(x, c("depth", "warp")) <- "fathoms"
   units(x, "bottom.temperature") <- "degreesC"
   format(x, c("start.time", "end.time", "start.time.logbook", "end.time.logbook")) <- "hh:mm:ss"
   
   return(x)
}

#' @rdname scsset
read.scsset <- function(x, year, ...){
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Use 'gulf.data' as data source:
   if (!missing(year)){
      v <- NULL
      for (i in 1:length(year)){
         file <- file.locate(package = "gulf.data", pattern = c("scs", "set", "csv", year[i]))
         if (length(file) == 1) v <- rbind(v, read.csv(file, header = TRUE, stringsAsFactors = FALSE))
      }
   }  
   
   # Subset by specified variables:
   v <- base::subset.data.frame(v, ...)
   
   # Convert to 'scsset' object:
   v <- scsset(v)

   return(v)
}
