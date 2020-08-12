#' Minimum Legal Size
#' 
#' @description Return the minimum legal sizes.
#' 
#' @export minimum.legal.size
#' @rawNamespace S3method(minimum.legal.size, default)

minimum.legal.size <- function(x, ...) UseMethod("minimum.legal.size")

#' @describeIn minimum.legal.size Default minimum legal size function.
minimum.legal.size.default <- function(x, species, year, fishing.zone, sub.fishing.zone, ...){
   if (missing(x)){
      x <- read.csv(system.file("extdata", "minimum.legal.sizes.csv", package = "gulf.data"), header = TRUE, stringsAsFactors = FALSE)
      return(x)
   }
   
   if (missing(species)) stop("'species' must be specified.")
   if ((species == 2550) & missing(year)) stop("'year' must be specified for lobster.")
   
   if (all(is.na(x))) return(x)
}

mls <- minimum.legal.size

