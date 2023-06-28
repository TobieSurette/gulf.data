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
   v <- c("light orange",
          "dark orange",
          "black",
          "coccoon",
          "orange")
   names(v) <- 1:5
   
   if (missing(x)){
      return(v)
   }else{
      if (is.character(x)){
         x <- tolower(x)
         x[x %in% c("oc", "lo", "orange clair", "light orange")] <- "1"
         x[x %in% c("of", "do", "orange fonce", "dark orange")] <- "2"
         x[x %in% c("b", "bl", "n", "noir", "black")] <- "3"
         x[x %in% c("c", "co", "coccon", "coccoon")] <- "4"
         x[x %in% c("o", "orange")] <- "5"
         v <- as.vector(v[x])
      } 

      if (is.numeric(x)) v <- as.vector(v[as.character(x)])
   } 
   
   return(v)
}

#' @export egg.color
egg.color <- egg.colour
