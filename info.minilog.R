info.minilog <- function(x, set.card = NULL){
   # INFO.MINILOG - Extract information for a 'minilog' object.

   if (!is.null(header(x))){
      v <- list()
      v$file.name <- header(x)$file.name
      v$tow.id <- header(x)$Study.ID
      v$touchdown$estimated <- touchdown(x)
      v$start.time <- header(x)$Start.Time
      v$finish.time <- header(x)$Finish.Time
      v$duration    <- as.numeric(max(time(x)) - min(time(x)))
      v$rows <- dim(x)[1]
      v$columns <- dim(x)[2]
      sensors <- setdiff(names(x), c("year", "month", "day", "hour", "minute", "second"))
      sensors <- sensors[sensors != ""]
      v$sensors <- paste("(", paste(sensors, collapse = ", "), ")", sep = "")
         
      t <- table(diff(time(x)))
      t <- t[t == max(t)]
      v$sampling.frequency <- as.numeric(names(t))
      
      if (!is.null(set.card)){
         index <- match(x, set.card)
         v$tow.number <- set.card[index, "tow.number"]
         v$year <- set.card[index, "year"]
         v$month <- set.card[index, "month"]
         v$day <- set.card[index, "day"]
         v$swept.area <- set.card[index, "swept.area"]
      }
   }else{
      return(NULL)
   }
   
   
   return(v)
}

