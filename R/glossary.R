#' Glossary Terms for Gulf Data
#'
#' @description Returns glossary terms and definitions used in Southern Gulf of Saint-Lawrence data sets.
#'
#' @param x Search terms.
#'
#' @examples
#' glossary()         # Complete table of glossary terms.
#' glossary("moult")  # Look up 'moult' in glossary terms.

#' @export
glossary <- function(x, ...) UseMethod("glossary")

#' @describeIn glossary Extract glossary terms.
#' @export
glossary.default <- function(x, ...){
   v <- read.table(locate(package = "gulf.data", file = "glossary.tab"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
   
   # Search terms:
   if (!missing(x)){
      if (length(x) > 0){
         index <- NULL
         for (i in 1:length(x)) index <- c(index, grep(tolower(x[i]), tolower(v$name)))
         for (i in 1:length(x)) index <- c(index, grep(tolower(x[i]), tolower(v$keywords)))
         index <- sort(unique(index))
         v <- v[index, ]
      }
   }
   
   return(v)
}
