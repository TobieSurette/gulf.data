#' Egg Development
#' 
#' @description Returns a description for snow crab egg development codes. Unknown codes and NA values are returned as NA values.

#' @param x Numerical value(s) specifying egg development code(s).
#' 
#' @examples
#' 
#' egg.development()                # Table of all defined egg development codes.
#' egg.development(1)               # Single code.
#' egg.development(c(1,1,2,2,3,15)) # Vector of code values.

egg.development <- function(x, ...){  
   # Define table of values:
   v <- c("Prefuniculus formation",
          "Funiculus formation",
          "Cleavage and blastula",
          "Gastrula",
          "Lateral ectodermal band",
          "Prenauplius",
          "Nauplius",
          "Maxiliped formation",
          "Metanauplius",
          "Late metanauplius",
          "Eye-pigment formation",
          "Chromatophore formation",
          "Reduced yolk",
          "Prehatching")
   names(v) <- 1:length(v)
   
   if (missing(x)) return(v) else v <- as.vector(v[as.character(x)])
   return(v)
}
