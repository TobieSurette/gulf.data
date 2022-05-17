#' @title Administrative regions
#'
#' @description Functions to provide administrative regions identifiers and descriptions.
#'
#' @param x Character string containing region descriptors.
#' @param language Language to be returned.
#' @param output Output format, either \sQuote{region}, \sQuote{description} or \sQuote{abbreviation}.
#' @param ... Not used.
#' 
#' @examples
#' # Complete sex code and description table:
#' region()
#'
#' # Description for numeric sex codes:
#' region("g")
#'

#' @export
region <- function(x, ...) UseMethod("region")

#' @export
region.default <- function(x, language = "english", output = "abbreviation", ...){
   # Parse language argument:
   language <- gulf.utils::language(language)
   
   # Load data table:
   tab <- read.csv(locate(package = "gulf.data", file = "region.csv"), header = TRUE, stringsAsFactors = FALSE)
   
   # Parse output argument:
   output <- match.arg(tolower(output), c("abbreviation", "descrpition", "region"))
   if (output %in% c("descrpition", "region")) output <- paste0(output, ".", language)
       
   # Look up character input:
   if (!missing(x)){
      if (is.factor(x)) x <- as.character(x)
      if (is.character(x)){
         ux <- unique(x)
         v <- rep("", length(ux))
         for (i in 1:length(ux)){
            ix <- grep(ux[i], tab[,paste0("region.", language)])
            if (length(ix) > 1) v[i] <- tab[ix[1], output]
         }
         v <- v[match(x, ux)]
      } 
      
      return(v)
   }
   
   return(tab)
}
