#' @title Sex Codes and Descriptions.
#'
#' @description Functions to provide sex code options and descriptions.
#'
#' This function matches a sex code to a description of the sex of an animal.
#' If no \code{code} argument is given, then a named character vector of all
#' valid codes is returned.
#'
#' @param x Numerical or character vector containing sex code or descriptors.
#' @param language Language to be returned.
#'
#' @examples
#' # Complete sex code and description table:
#' sex()
#'
#' # Description for numeric sex codes:
#' sex(1)
#' sex(1, language = "fr")
#' sex(1:10) # Vector input.
#' 
#' # Mixture of character input formats:
#' sex(c("1", "2", "3", "m", "F", "mal", "fe", NA, "f", "femelle", "h", "female"), language = "french")

#' @export
sex <- function(x, ...) UseMethod("sex")

#' @export
sex.default <- function(x, language = "english", ...){
   language <- gulf.utils::language(language)
   
   # Define numeric values:
   tab <- data.frame(code = c(0, 1, 2, 3, 4, 9), 
                     english = c("unobserved", "male", "female", "hermaphrodite", "empty", "unsexed"),
                     french = c("non-observé", "mâle", "femelle", "hermaphrodite", "vide", "non-sexé"),
                     stringsAsFactors = FALSE)
   
   if (!missing(x)){
      # Character input:
      if (is.factor(x)) x <- as.character(x)
      if (is.character(x)){
         v <- tab$code[pmatch(tolower(x), tolower(tab[, "english"]), duplicates.ok = TRUE)]
         v[is.na(v)] <- tab$code[pmatch(tolower(x[is.na(v)]), tolower(tab[, "french"]), duplicates.ok = TRUE)]
         v[is.na(v)] <- tab$code[match(gulf.utils::deblank(x[is.na(v)]), as.character(tab[, "code"]))]
         x <- v
      } 
      
      # Numeric input:
      if (is.numeric(x)) tab <- tab[match(x, tab$code), language]
   }
   
   return(tab)
}
      
