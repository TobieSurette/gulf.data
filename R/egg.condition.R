#' Egg Condition
#' 
#' @description Returns a description for an egg condition code.
#' 
#' Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying egg condition code(s).
#' 
#' @examples
#' # Get the table of all defined egg condition codes:
#' egg.condition()
#' egg.condition(1)
#' egg.condition(0:2)

#' @export
egg.condition <- function(x, ...){  
   # Define table of values:
   v <- c("no eggs", "new eggs", "old eggs", "eggs unspecified")
   names(v) <- c(0, 1, 2, 3)
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}
