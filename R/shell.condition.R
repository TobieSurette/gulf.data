#' Shell Condition
#' 
#' @description Returns a description for snow crab shell codes. Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying shell condition code(s).
#' 
#' @examples
#' # Get the table of all defined shell condition codes:
#' shell.condition()
#' shell.condition(1)
#' shell.condition(0:2)

#' @export shell.condition
shell.condition <- function(x, ...){  
   # Define table of values:
   v <- c("new soft",
          "new hard",
          "hard clean",
          "hard light moss",
          "old heavy moss")
   names(v) <- 1:5
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}
