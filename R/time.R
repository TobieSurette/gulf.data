#' Trawl Event Times
#' 
#' @description These are functions for extracting the times at which various events occur during trawling. 
#'              For example, there are the start and stop times determined by on-board observers or trawl touchdown and 
#'              liftoff times.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2020)
#' time(x, "start")
#' 
#' x <- read.esonar(2020, tow.id = "GP354F")
#' time(x, "start")
#' time(x, "stop")

# @describeIn time Extract event times for \code{scsset} data.
#' @export
time.scsset <- function(x, event = "start", ...){
   event <- match.arg(tolower(event), c("start", "end", "stop", "haul", "touchdown", "liftoff"))
   if (event == "end") if (length(intersect(grep("end", names(x)), grep("time", names(x)))) == 0) event <- "stop" 
   
   # Treat 'stop' and 'end' as synonyms:
   names(x) <- gsub("end", "stop", names(x))
   event <- gsub("end", "stop", event)
   
   # Result variable:
   v <- rep("", nrow(x))
   
   # Regular naming:
   var <- paste0(event, ".time")
   if (var %in% names(x)){
      index <- which((deblank(x[,var]) != "")  &  !is.na(x[,var]))
      v[index] <- x[index,var]
   }
   
   # Look in logbook variables:
   var <- paste0(event, ".time.logbook")
   if (var %in% names(x)){
      index <- which((v == "") & (deblank(x[,var]) != "")  &  !is.na(x[,var]))
      v[index] <- x[index,var]
   }
   
   v <- as.POSIXct(paste(as.character(gulf.utils::date(x)), v))
   
   return(v)
}

# @describeIn time Extract time stamps or event times for \code{probe} data.
#' @export
time.probe <- function(x, event, ...){
   if (!is.null(gulf.metadata::project(x))){
      if ((gulf.metadata::project(x) == "scs") & !missing(event)){
         year <- unique(gulf.utils::year(x))
         y <- data.frame(date = as.character(unique(gulf.utils::date(x))), tow.id = tow.id(x), stringsAsFactors = FALSE)
         z <- read.scsset(year)
         r <- gulf.utils::time(z[gulf.utils::match(y[gulf.metadata::key(z)], z[gulf.metadata::key(z)]), ], event = event, ...)
         return(r)
      }
   }
 
   r <- gulf.utils::time(as.data.frame(x), ...)
   
   return(r)  
}

