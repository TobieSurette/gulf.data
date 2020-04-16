#' Read a GPS file
#' 
#' This function reads a GPS track file.
#' 
#' @param x File name(s).
#' 
#' @export read.gps
#' @return Returns a data.frame with date, time, latitude and longitude fields.
#' 

read.gps <- function(x){
   res <- NULL
   for (i in 1:length(x)){
      # Load file:
      data <- scan(x[i], what = "character", quiet = TRUE)
      
      # Parse latitude and longitude fields:
      data <- gsub('[\"]', "", data)
      latitude <- data[grep("^lat", data)]
      latitude <- as.numeric(gsub("lat=", "", latitude))
      longitude <- data[grep("^lon", data)]
      data <- longitude
      longitude <- gsub("><.+", "", longitude)
      longitude <- as.numeric(gsub("lon=", "", longitude))

      data <- gsub("lon.+<time>", "", data) 
      data <- gsub("</time>.+$", "", data) 
      
      # Parse out data and time fields:
      data <- gsub("lon.+<time>", "", data) 
      data <- gsub("</time>.+$", "", data) 
      date <- gsub("T.+$", "", data)
      time <- gsub("(.+T)|(Z)", "", data)

      # Combine data:
      data <- data.frame(date = date, 
                         time = time, 
                         latitude = latitude, 
                         longitude = longitude,   
                         stringsAsFactors = FALSE)
      
      # Catenate data:
      res <- rbind(res, data)
   }
   
   return(res)
}
