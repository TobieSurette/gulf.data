#' Eggs remaining
#' 
#' @description Returns a description for snow crab eggs remaining codes. Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying eggs remaining code(s).
#' 
#' @examples
#' # Get the table of all defined eggs remaining codes:
#' eggs.remaining(1)
#' eggs.remaining(0:2)

#' @export eggs.remaining
eggs.remaining <- function(x, ...){  
   # Define table of values:
   v <- c("Absent (no eggs)",
          "1 – 49%",
          "50 – 74%",
          "75 – 99%",
          "100%")

   names(v) <- 0:4
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}

