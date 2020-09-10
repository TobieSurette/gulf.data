#' Probe Data
#' 
#' @name probe
#' 
#' @description Measurement probe class containing date and time stamps and measured values.

#' @rdname probe
#' @export
probe <- function(x, ...) UseMethod("probe")

#' @rdname probe
#' @export
probe.default <- function(x, header, ....){
   # Store date and time stamp:
   v <- data.frame(date = gulf.utils::date(x),
                   time = gulf.utils::time(x),
                   stringsAsFactors = FALSE)
   
   # Identify and store observations:
   remove <- c("date", "time", "year", "month", "day", "hour", "minute", "second")
   vars <- setdiff(names(x), remove)
   v[vars] <- x[vars]
   
   # Define index key:
   key(v) <- c("date", "time")
   
   # Add header:
   if (!missing(header)) header(x) <- header
   
   # Assign additional arguments as attributes: 
   args <- list(...)
   if (length(args) > 0) for (i in 1:length(args)) attr(x, names(args)[i]) <- args[[i]]
   
   class(v) <- unique(c("probe", class(v)))
}

# MATCH.ESONAR - Return the set card indices which match a 'esonar' object.





#' @export
match.probe <- function(x, y, by, method, min.distance = 500, ...){
   index <- rep(NA, nrow(y))
   if (("scsset" %in% class(y))){
      z <- data.frame(year = unique(x$year), tow.id = attr(x, "tow.id"))
      return(gulf.utils::match.data.frame(z, y[key(y)]))
   }
   
   # Parse 'method' argument:
   method <- match.arg(tolower(gsub("[ .]", method), c("tow.id", "latlong", "time"))

   # Match 'x' to set card using coordinates:
   if (method == "latlong"){
      d <- distance(mean(longitude(x), na.rm = TRUE), mean(latitude(x), na.rm = TRUE), longitude(y), latitude(y))[1,] * 1000
      if (d[which.min(d)] > min.distance) index <- NA else index <- which.min(d)
   }


   # Match 'x' to set card using file name:
   if (method == "time"){
      time 
      
      res <- aggregate(tm, by = x[c("year", "tow.number", "file.name")], mean)
      d <- abs(repvec(res[, 4], ncol = length(ts)) - repvec(ts, nrow = dim(res)[1]))
      res$index <- apply(d, 1, which.min)
      res$distance <- apply(d, 1, function(x) x[which.min(x)])
      res$index[res$distance > 600] <- NA
      index <- merge(x, res, by = c("year", "tow.number", "file.name"), names = "index", all.x = TRUE, sort = FALSE)$index
   }

   return(index)

   
   return(index)
}


start.time.probe <- function(x, ...){
   
}

end.time.probe

truncate.probe <- function(x, start.time, end.time, buffer = 0, ...){
   
   # Define start and end time:
   if (missing(start.time)) start.time <- start.time(x, ...)
   if (missing(end.time)) end.time <- end.time(x, ...)   
 
   buffer <- time2sec(buffer)
   t <- time(x)
   index <- (t >= (start.time - buffer)) & (t >= (end.time + buffer))
   x <- x[index, ]
   
   return(x)
}





