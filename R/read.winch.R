#' @title Read Trawl Winch Probe Data
#'
#' @description Read trawl winch speed and warp length data.
#'
#' @param file File name.

#' @export read.winch
read.winch <- function(x, ...) UseMethod("read.winch")

#' @describeIn read.winch Read a trawl winch data file.
#' @rawNamespace S3method(read.winch,default)
read.winch.default <- function(file){
   # Read file:
   x <- read.csv(file, stringsAsFactors = FALSE)

   # 2021 trawl experiment correction:
   if (unique(as.character(gulf.utils::date(x)) %in% c("2021-07-12", "2021-07-13"))){
       t <- table(x$warp)
       max.warp <- as.numeric(names(t)[which.max(t)])
       if (length(grep("test01[2-3]", file)) > 0) warp <-  150 * 1.8288 else warp <-  100 * 1.8288

       x$warp <- -abs(x$warp - max.warp) + warp
   }

   # Remove negative warp values:
   x$warp[x$warp < 0] <- NA
   x$speed <- x$speed / 60
   x$speed[x$speed > 2] <- NA
   return(x)
}

