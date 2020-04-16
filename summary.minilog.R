summary.minilog <- function(x, truncate = TRUE, round = TRUE, ...){
   # SUMMARY.MINILOG - Returns summary statistics of a 'minilog' object.
   
   # Read tow data:
   y <- read.scset(year = unique(x$year), print = FALSE)
  
   # Trim data:
   if (truncate) x <- truncate(x, ...)
   
   # Grouping variables:
   vars <- c("year", "month", "day", "tow.id")
     
   # Calculate maximal depth:
   res <- aggregate(data.frame(depth = x$depth), by = x[vars], mean, na.rm = TRUE)
   res$bottom.temperature <- NA
   
   # Index of data observations during trawling:
   x$time <- time(x)
   ii <- match(x[c("year", "tow.id")], y[c("year", "tow.id")])
   index <- which((x$time >= start.time(y)[ii]) & (x$time <= end.time(y)[ii]))
   x$depth.trawling <- NA
   x$depth.trawling[index] <- x$depth[index] 
   x$temperature.trawling <- NA
   x$temperature.trawling[index] <- x$temperature[index]
  
   # Calculate mean depth and temperature:
   ii <- match(res[c("year", "tow.id")], y[c("year", "tow.id")])
   res$depth <- aggregate(data.frame(depth = x$depth.trawling), by = x[vars], mean, na.rm = TRUE)[, "depth"]
   res$depth.logbook   <- y$depth[ii]
   res$bottom.temperature <- aggregate(data.frame(bottom.temperature = x$temperature.trawling), by = x[vars], mean, na.rm = TRUE)[, "bottom.temperature"]
   res$start.time <- start.time(y[ii,], ...)
   res$end.time   <- end.time(y[ii,], ...)
 
   # Function to extract sampling resolution for depth and temperature:                    
   interval <- function(x){
      f <- table(diff(sort(unique(x))))
      d <- as.numeric(names(f))
      return(d[which.max(f)])
   }
   res <- cbind(res, aggregate(data.frame(depth.delta = x$depth), by = x[vars], interval)["depth.delta"])
   res <- cbind(res, aggregate(data.frame(temperature.delta = x$temperature), by = x[vars], interval)["temperature.delta"])
         
   # Determine approximate median bottom time:
   res$start.time.minilog <- aggregate(data.frame(start.time = x$time), by = x[vars], min)[, "start.time"]
   res$end.time.minilog <- aggregate(data.frame(end.time = x$time), by = x[vars], max)[, "end.time"]

   # Reformat time fields:
   fun <- function(x) return(unlist(lapply(strsplit(as.character(x), " "), function(x) x[2])))
   res$start.time <- fun(res$start.time)
   res$end.time   <- fun(res$end.time)
   res$start.time.minilog <- fun(res$start.time.minilog)
   res$end.time.minilog   <- fun(res$end.time.minilog)
      
   # Round-off data:
   if (round){
      res$depth <- round(res$depth, 1)
      res$bottom.temperature <- round(res$bottom.temperature, 2)
   }
   
   return(res)
}
