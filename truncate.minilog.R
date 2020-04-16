truncate.minilog <- function(x, buffer = 0, ...){
   # TRUNCATE.MINILOG - Truncate a 'minilog' object.

   # Check input data:
   year <- unique(x$year)
   if (length(year) != 1) stop("'minilog' object 'x' must contain a single year of data.")
   
   # Load set card:
   tows <- read.scset(year = unique(x$year), print = FALSE)
   tows <- tows[tows$tow.id %in% unique(x$tow.id) , ]
   x <- x[x$tow.id %in% tows$tow.id, ]
   
   # Initialize result vector:
   index <- rep(FALSE, nrow(x))

   # Convert from minutes to seconds:
   if (length(buffer) == 1) buffer = c(buffer, buffer)
   if (!is.numeric(buffer) | !(length(buffer) == 2)) stop("'buffer' must be a numeric scalar or two-element vector.")  
   buffer <- buffer * 60  # Convert to seconds.
   
   # Remove data outside the define start and end times:
   for (i in 1:nrow(tows)){
      ii <- which(x$tow.id == tows$tow.id[i])
      t <- time(x[ii, ])
      ii <- ii[(t >= (start.time(tows[i,], ...) - buffer[1])) & (t <= (end.time(tows[i,], ...) + buffer[2]))]
      index[ii] <- TRUE
   }

   # Return subset of the data:
   x <- x[index, ]

   return(x)
}
