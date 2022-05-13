#' @title Project Identifiers
#' 
#' @description Functions to retrieve survey vessel information.
#'
#' @param x Character search string.
#' @param ... Other arguments (not used).
#' 
#' @examples 
#' vessel()               # Survey vessel table.
#' vessel("prince")       # Search for vessel name.
#' vessel("marco")        # Search for vessel name.
#' vessel("opilio")       # Search for vessel name.

#' @export
vessel <- function(x, ...) UseMethod("vessel")

#' @describeIn vessel Default \code{vessel} method. Returns the complete vessel data table.
#' @export
vessel.default <- function(x, ...){
   file <- locate(package = "gulf.data", file = "vessel.csv")
   v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   return(v)
}

#' @describeIn vessel Search for vessel name and return vessel specifications.
#' @export 
vessel.character <- function(x, verbose = FALSE, ...){
   tab <- vessel() # Load vessel table.
   
   # Spot corrections:
   x <- gulf.utils::deblank(x)
   x <- gsub(" +", " ", x)
   x <- gsub("j[ae][ea]n[ ]*mat[h]*ieu.*", "jean-mathieu", tolower(x))
   x <- gsub("[a]+v[ea]lon[ ]*voyage[u]*r.*", "avalon voyager", tolower(x))
   
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
   
   return(vx[match(x, ux)])
}
