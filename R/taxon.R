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
#' @seealso \code{\link{taxon}}, \code{\link{data}}

#' @export
taxon <- function(x, ...) UseMethod("taxon")

#' @export 
taxon.default <- function(x, rank, aphia.id, drop = TRUE, ...){
   # Load species taxonomic information table:
   tab <- species()
   
   # Default 'rank': 
   remove <- c("code", "english", "latin", "french", "aphia.id")
   if (missing(rank)) rank <- setdiff(names(tab), remove) else drop <- FALSE
   
   # Initialize variables:   
   ix <- NULL
   v <- NULL
   
   # Match using RV species coding:
   if (!missing(x)) if (is.numeric(x)) ix <- match(x, tab$code)
   
   # Match using WoRMS aphia ID codes:
   if (!missing(aphia.id)) if (is.numeric(aphia.id)) ix <- match(aphia.id, tab$code)
   
   # Extract taxonomic information:
   if (length(ix) > 0){
      rank <- rank[rank %in% names(tab)]
      v <- tab[ix, rank]
      if (drop) v <- v[unlist(lapply(v, function(x) any(x != "")))]
   }
   
   # Expand empty result:
   if (length(v) == 0) v <- rep("", length(x))
   
   return(v)
}


