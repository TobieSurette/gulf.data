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
#' taxon(family = "gadidae") # Extract taxonomic table for a specified rank.
#' 
#' #' Check if species code is part of family 'Gadidae':
#' is.taxon(1:100, family = "Gadidae")
#' 
#' @seealso \code{\link{taxon}}, \code{\link{data}}

#' @export
taxon <- function(x, ...) UseMethod("taxon")

#' @describeIn taxon Default taxon function.
#' @export 
taxon.default <- function(x, rank, aphia.id, drop = TRUE, ...){
   # Load species taxonomic information table:
   tab <- species()
   if (!missing(aphia.id)) tab$code <- tab$aphia.id
   
   # Taxonomic rank is specified:
   rank <- list(...)
   if (is.list(rank)){
      rank <- rank[tolower(names(rank)) %in% names(tab)]
      if (length(rank) == 0) return(NULL)
      
      ix <- 1:nrow(tab)
      for (i in 1:length(rank)) ix <- intersect(ix, which(tolower(tab[, names(rank)[i]]) %in% tolower(rank[[i]])))    
      
      if (missing(x)) return(tab[ix, ])
   }
   
   # Default 'rank': 
   remove <- c("code", "english", "latin", "french", "aphia.id")
   if (missing(rank)) rank <- setdiff(names(tab), remove) else drop <- FALSE
   
   # Initialize variables:   
   ix <- NULL
   v <- NULL
   
   # Match using appropriate coding:

   tab <- tab[!is.na(tab$code), ]
   if (!missing(x)) if (is.numeric(x)) ix <- match(x, tab$code)  

   # Extract taxonomic information:
   if (length(ix) > 0){
      rank <- rank[rank %in% names(tab)]
      v <- tab[ix, rank]
      if (drop) v <- v[which(unlist(lapply(v, function(x) any(x != ""))))]
   }
   
   # Expand empty result:
   if (length(v) == 0) v <- rep("", length(x))
   
   return(v)
}

#' @export
taxon.NULL <- species

#' @export
is.taxon <- function(x, ...) UseMethod("is.taxon")

#' @describeIn taxon Check taxonomic membership.
#' @rawNamespace S3method(is.taxon,default)
is.taxon.default <- function(x, ..., aphia.id){
   rank <- list(...)
   tab <- taxon()
   if (!missing(aphia.id)) tab$code <- tab$aphia.id
   rank <- rank[tolower(names(rank)) %in% names(tab)]
   if (length(rank) == 0) return(NULL)
  
   ix <- 1:nrow(tab)
   for (i in 1:length(rank)) ix <- intersect(ix, which(tolower(tab[, names(rank)[i]]) == tolower(rank[[i]])))
      
   if (missing(x)) stop("'x' species code must be specified.")
   v <- rep(FALSE, length(x))
   v[which(x %in% tab$code[ix])] <- TRUE      

   return(v)
}
   


   