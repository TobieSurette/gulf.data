#' Egg colour
#' 
#' @description Returns a description for snow crab egg colour codes. Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying egg colour code(s).
#' 
#' @examples
#' # Get the table of all defined egg colour codes:
#' egg.colour()
#' egg.colour(1)
#' egg.colour(0:2)

#' @export egg.colour
egg.colour <- function(x, ...){  
   # Define table of values:
   v <- c("Light orange",
          "Dark orange",
          "Black",
          "Coccon")
   names(v) <- 1:4
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}

#' @export egg.color
egg.color <- egg.colour
