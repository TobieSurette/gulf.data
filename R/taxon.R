#' Retrieve Taxonomic Information
#' 
#' @description Retrieve taxonomic information associated with identification codes.
#' 
#' @param x Species code.
#' @param aphia.id Aphia taxonomic identification codes.
#' @param rank Taxonomic rank (e.g. "kingdom", "phylum", "genus", etc ...). 
#' @param drop Logical value specifying whether to remove empty columns from the taxonomic reference table.
#' @param ... Other taxonomic groups (see examples).
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
   
   # Look up specific taxa:
   lookup <- list(...)
   if (length(lookup) > 0){
      ix <- 1:nrow(tab)
      for (i in 1:length(lookup)) ix <- intersect(ix, which(tolower(tab[, names(lookup)[i]]) %in% tolower(lookup[[i]])))    
      if (missing(x)) return(tab[ix, ]) 
   }
   
   # Taxonomic rank is specified:
   remove <- c("code", "english", "latin", "french", "aphia.id")
   if (!missing(rank)) tab <- tab[, unique(c(remove, rank[tolower(rank) %in% names(tab)]))] 

   # Match using species coding:
   if (!missing(x)){
      tab <- tab[!is.na(tab$code), ]
      if (is.numeric(x)) if (length(ix) > 0) tab <- tab[match(x, tab$code), ] else return(NULL)
      
      # Remove irrelevant fields:
      tab <- tab[, setdiff(names(tab), remove), drop = FALSE]
   }
   
   # Extract taxonomic information:
   if (drop) tab <- tab[which(unlist(lapply(tab, function(x) any(x != ""))))]
   if (ncol(tab) == 1) tab <- tab[,1]
   
   return(tab)
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
   


   