#' @title Project Identifiers
#' 
#' @description Functions to retrieve survey probe information or create a probe data object.
#'
#' @param x Character search string.
#' @param header File header information.
#' @param ... Other arguments (not used).
#' 
#' @examples 
#' probe()            # Load complete data probe table.
#' probe("min")       # Search for probe name.
#  probe("st")        # Search for probe name.

#' @export
probe <- function(x, ...) UseMethod("probe")

#' @describeIn probe Load data probe description table.
#' @export
probe.default <- function(x, ...){
   file <- locate(package = "gulf.data", file = "probe.csv")
   v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   return(v)
}

#' @describeIn probe Find a data probe by name.
#' @export 
probe.character <- function(x, ...){
   tab <- probe() # Load probe table.
   
   # Treat only unique cases:
   ux <- unique(x)
   ux <- ux[which(!is.na(ux) & ux != "")]
   if (length(ux) == 0) return(NULL)
   
   # Find matches:
   vx <- rep(NA, length(ux))
   for (i in 1:length(ux)){
      ix <- grep(tolower(ux[i]), tolower(tab$name))
      if (length(ix) > 1) ix <- ix[1]
      if (length(ix) == 1) vx[i] <- tab$name[ix]
   }
   
   return(vx[match(x, ux)])
}

#' @describeIn probe Create a \code{probe} class object.
#' @rawNamespace S3method(probe,data.frame)
probe.data.frame<- function(x, header, ...){
   # Store date and time stamp:
   v <- data.frame(date = as.character(gulf.utils::date(x)),
                   time = unlist(lapply(strsplit(as.character(gulf.utils::time(x)), " "), function(x) x[2])), 
                   stringsAsFactors = FALSE)
   
   # Add other variables:
   vars <- setdiff(names(x), c("date", "time", "year", "month", "day", "hour", "minute", "second"))
   v[vars] <- x[vars]
   
   # Define index key:
   key(v) <- c("date", "time")
   
   # Add header:
   if (!missing(header)) header(v) <- header
   
   # Assign additional arguments as attributes: 
   args <- list(...)
   if (length(args) > 0) for (i in 1:length(args)) attr(v, names(args)[i]) <- args[[i]]
   
   # Add date and time formats:
   fmt(v, "date") <- "YYYY-MM-DD"
   fmt(v, "time") <- "hh:mm:ss"
   
   class(v) <- unique(c("probe", class(v)))
   
   return(v)
}

   