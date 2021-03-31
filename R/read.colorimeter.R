#' Locate and Read Colorimetry Data
#' 
#' @description Functions to find and access colour meter data from the snow crab survey.
#' 
#' @param x Data object.
#' @param year Survey year.
#' @param tow.id Character string(s) specifying a tow identifier(s) (e.g. \sQuote{GP313F}).
#' 
#' @examples 
#' # Locate data files:
#' locate.colorimeter()          # Find all snow crab colorimeter data files.
#' locate.colorimeter(2020)      # Find specific year.
#' locate.colorimeter(2017:2019) # Find set of years.
#' 
#' # Read data:
#' x <- read.colorimeter()
#' x <- read.colorimeter(2020)
#' x <- read.colorimeter(tow.id = "GP313F")
#' 
#' @seealso \code{\link{read.colorimeter}}

#' @export read.colorimeter
read.colorimeter <- function(...){
   files <- locate.colorimeter(...)

   if (length(files) > 0){
      for (i in 1:length(files)){
         tmp <- read.table(files[i], sep = ",", header = TRUE, stringsAsFactors = FALSE)
         if (i == 1){
            x <- tmp
         }else{
            vars <- unique(names(x), names(tmp))
            x[setdiff(vars, names(x))] <- NA
            tmp[setdiff(vars, names(tmp))] <- NA
            tmp <- tmp[vars]
            x <- x[vars]
            x <- rbind(x, tmp)
         }
         
      }
   }

   # Date conversion:
   x <- cbind(data.frame(date = as.character(date(x))), x[setdiff(names(x), c("year", "month", "day", "date"))])
   
   # Subset data:
   p <- list(...)
   if ((nrow(x) > 0) & (!is.null(names(p)))){
      for (i in 1:length(p)){
         ix <- 1:nrow(x)
         if (names(p)[i] %in% names(x)){
            ix <- x[, names(p)[i]] %in% p[[i]]
            x <- x[ix, ]
         }
      }
   }
   
   # Shell condition formatting:
   x$shell.condition <- toupper(x$shell.condition)
   
   # Define index key:
   key(x) <- key.scsbio()
  
   return(x)
}

# @describeIn read.colorimeter Read snow crab colorimetry files.
#' @export read.colormeter
read.colormeter <- read.colorimeter

#' @describeIn read.colorimeter Locate snow crab colorimetry files.
#' @export locate.colorimeter
locate.colorimeter <- function(x, year, source = "gulf.data", ...){
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if ("year" %in% names(x)) year <- sort(unique(x$year))
      if ("date" %in% names(x)) year <- as.numeric(substr(x$date, 1, 4))
   }
   
   # Use 'gulf.data' as data source:
   file <- locate(package = "gulf.data", file = c("scs", "colorimeter", "csv"))
   
   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @export locate.colormeter
locate.colormeter <- locate.colorimeter
