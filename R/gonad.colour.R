#' Gonad colour
#' 
#' @description Returns a description for snow crab gonad colour codes. Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying gonad colour code(s).
#' 
#' @examples
#' # Get the table of all defined gonad colour codes:
#' gonad.colour(1)
#' gonad.colour(0:2)

#' @export gonad.colour
gonad.colour <- function(x, ...){  
   # Define table of values:
   v <- c("white",
          "beige",
          "orange")
   names(v) <- 1:3
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}

#' @export gonad.color
gonad.color <- gonad.colour
