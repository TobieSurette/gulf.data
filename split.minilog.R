split.minilog <- function(x, by = "file.name", set.card, start, end, buffer = 0){
   # SPLIT.MINILOG - Split a 'minilog' object into its component tows.
   
   # Parse 'by' argument:
   by <- match.arg(tolower(by), c("file.name", "time", names(x)))
   
   # Initialize result variable:
   res <- NULL
   
   # Split using file name:
   if (by %in% names(x)){
      if (!("file.name" %in% names(x))) return(x)
      
      # Separate 'x' into component parts:
      res <- by(x, x[, by, drop = FALSE], function(x) return(x))
   
      # Remove 'by' attributes:
      attributes(res) <- NULL
   }
   
   # Split using start and end times:
   if (by == "time"){
      V <- list()

      # Extract start and end times:
      if (!missing(set.card)){
         start <- time(set.card)
         end <- time(set.card) + set.card$duration*60
      }
      
      # Check start and end times:
      if (missing(start) | missing(end)) 
         stop("Unable to proceed without tow start and end times.")
      if (length(start) != length(end)) 
         stop("'start' and 'end' time vectors are not the same length.")
         
      # Partition scanmar object 'x' by adjacent elements of 'time':
      t <- time(x)
      for (i in 1:length(start)){
         index <- (t >= (start[i] - buffer*60)) & (t <= (end[i] + buffer*60))
         V[[i]] <- x[index, ]
      }
      
      # Remove empty tows:
      index <- unlist(lapply(V, function(x) dim(x)[1] == 0))
      
      res <- V[!index]
   }
   
   return(res)
}
