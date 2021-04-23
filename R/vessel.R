#' Project Identifiers
#' 
#' @name vessel
#' 
#' @description Functions to retrieve survey vessel information.
#'
#' @param x Character search string.
#' @param ... Other arguments (not used).
#' 
#' @return An object with a \code{project} attribute attached to it.
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{vessel}}{Generic \code{vessel} method.}
#'    \item{\code{vessel.default}}{Default \code{vessel} method. Returns the \code{vessel.csv} data table.}
#'    \item{\code{vessel.character}}{Search for vessel name and return vessel specifications.}
#' }
#' 
#' @examples 
#' vessel()               # Survey vessel table.
#' vessel("prince")       # Search for vessel name.
#  vessel("marco")        # Search for vessel name.
#  vessel("opilio")       # Search for vessel name.

#' @export
vessel <- function(x, ...) UseMethod("vessel")

#' @rdname vessel
#' @export
vessel.default <- function(x, ...){
   file <- locate(package = "gulf.data", file = "vessel.csv")
   v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   return(v)
}

#' @rdname vessel
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
