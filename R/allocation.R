#' @title Allocation Codes
#' 
#' @description Snow crab license allocation codes, used to partition landings by fisher groups.
#'
#' @param x Code.
#' @param language Character string specifying language (\sQuote{english} or \sQuote{french}).
#' @param ... Other arguments (not used).
#' 
#' @examples 
#' allocation()                             # Complete allocation code table.
#' 
#' codes <- allocation()$code
#' allocation(codes[1:5])                   # Look up particular codes.
#' allocation(codes[1:5], language = "fr")  # Look up particular codes and return descriptions in french.

#' @export
allocation <- function(x, ...) UseMethod("allocation")

#' @describeIn allocation return allocation code table.
#' @export
allocation.default <- function(x, ...){
   file <- locate(package = "gulf.data", file = "allocation.csv")
   v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   return(v)
}

#' @describeIn allocation return allocation code table.
#' @export 
allocation.character <- function(x, language = "english", ...){
   tab <- allocation() # Load vessel table.

   # Treat only unique cases:
   ux <- unique(x)
   ux <- ux[which(!is.na(ux) & ux != "")]
   if (length(ux) == 0) return(NULL)
   
   # Find matches:
   vx <- rep(NA, length(ux))
   for (i in 1:length(ux)){
      ix <- grep(tolower(ux[i]), tolower(tab$name))
      if (length(ix) > 1) ix <- ix[1]
      if (length(ix) == 1) vx[i] <- tab$name[ix]
   }
   
   return(vx[match(x, tab$code)])
}
