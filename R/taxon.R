#' Retrieve Taxonomic Information
#' 
#' @description Retrieve taxonomic information associated with identification codes.
#' 
#' @param x Species code.
#' @param aphia.id Aphia taxonomic identification codes.
#' @param rank Taxonomic rank (e.g. "kingdom", "phylum", "genus", etc ...). 
#' 
#' @examples 
#' taxon(10:100)             # Latin genus + species names.
#' taxon(10:100, "phylum")   # Phylum names.
#' taxon(10:100, "family")   # Family names.
#' taxon(10:100, c("class", "order", "family")) # Class, order and family names.
#' 
#' @seealso species

#' @export
taxon <- function(x, ...) UseMethod("taxon")

#' @export 
taxon.default <- function(x, rank = "species", aphia.id, ...){
   
   # Load species taxonomic information table:
   tab <- species()
   
   # Match using RV species coding:
   if (!missing(x)){
      if (is.numeric(x)){
         ix <- match(x, tab$code)
         rank <- rank[rank %in% names(tab)]
         return(tab[ix, rank])
      } 
   }
   
   # Match using WoRMS aphia ID codes:
   if (!missing(aphia.id)){
      if (is.numeric(aphia.id)){
         ix <- match(aphia.id, tab$code)
         rank <- rank[rank %in% names(tab)]
         return(tab[ix, rank])
      } 
   }
   
   return(rep("", length(x)))
}


