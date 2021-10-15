#' @title Find Event Time Files
#' 
#' @description Locate files containing event times for various surveys.
#' 
#' @param year Survey year.
#' @param project Project identifier.
#' 

#' @export read.event
read.event <- function(x,  ...){
   # Find files:
   files <- locate.event(x, ...)
   
   # Read files:
   v <- NULL
   if (length(files) > 0) for (i in 1:length(files)) v <- rbind(v, read.csv(files[i]))
   if (is.null(v)) return(v)
      
   # Subset other variables:
   pars <- list(...)
   if (length(pars) > 0){
      ix <- 1:nrow(v)
      for (i in 1:length(pars)){
         if (names(pars)[i] %in% names(v)){
            ix <- intersect(ix, which(v[, names(pars)[i]] %in% pars[[i]]))
         }
      }
      v <- v[ix, ]
   }
   
   return(v)
}
