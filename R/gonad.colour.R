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
          "orange",
          "light orange")
   names(v) <- 1:4
   
   if (missing(x)){
      return(v)
   }else{
      if (is.character(x)){
         x <- tolower(x)
         x[x %in% c("w", "white")] <- "1"
         x[x %in% c("b", "be", "bei", "beige")] <- "2"
         x[x %in% c("o", "orange")] <- "3"
         x[x %in% c("oc", "lo", "light orange", "orange clair")] <- "4"

         v <- as.vector(v[x])
      } 
      
      if (is.numeric(x)) v <- as.vector(v[as.character(x)])
   } 
   
   return(v)
}

#' @export gonad.color
gonad.color <- gonad.colour
