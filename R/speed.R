#' @title Survey Vessel Speed
#' 
#' @description Functions to extract survey vessel speed.
#' 
#' @param x Data object containing trawl wing spread measurements.
#' @param ... Not used.
#' 
#' @examples 
#' x <- read.scsset(2024, valid = 1)
#' speed(x)

#' @export wingspread
speed <- function(x, ...) UseMethod("speed")

speed.scsset <- function(x, stats = FALSE, ...){
   v <- rep(NA, nrow(x))
   years <- sort(unique(year(x)))
   for (i in 1:length(years)){
      print(years[i])
      files <- locate(package = "gulf.probe.data", keywords = c("scs", "esonar", years[i]))
      iz <- unique(c(grep("headline", files), grep("wingspread", files), grep("symmetry", files), grep("depth", files)))
      if (length(iz) > 0) files <- files[-iz]
      ix <- which(year(x) == years[i])
      for (j in 1:length(ix)){
         cat(paste0(j, ","))
         file <- files[grep(x$tow.id[ix[j]], toupper(files))]
         if (length(file) == 1){
            y <- read.esonar(file)
            y <- y[!is.na(y[, "speed"]), ]
            y <- y[c("date", "time", "speed")]
            t <- time(y) 
            y <- y[(t >= time(x[ix[j], ], "start")) & (t <= time(x[ix[j], ], "stop")), ]
            if (nrow(y) > 0) v[ix[j]] <- mean(y$speed)
         }
      }
      cat("\n")
   }
   return(v)
}
