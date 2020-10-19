#' Import Data
#' 
#' @description Import from one object into another.
#' 
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' @param by Character string(s) specifying which variables use as an index key.
#' @param ... Other parameters (not used).
#' 
#' @examples 
#' x <- read.scsset(year = 2020, valid = 1)
#' b <- read.scsbio(2020)
#' import(x, fill = 0) <- catch(b, category = c("M", "COM", "MF"))
#' print(x$M)
#' 
#' @export
"import<-" <- function(x, ...) UseMethod("import<-")

#' @describeIn import-set Import assignment method for data frames.
#' @export
"import<-.data.frame" <- function(x, value, by, ...){
   # Check input arguments or determine index key:
   if (missing(by)) if (!is.null(key(x))) by <- gulf.metadata::key(x)
   if (missing(by)) by <- intersect(names(x), names(value))
   if (length(by) == 0) stop("Unable to determine index key for data import.")
   if (!all(by %in% names(x)) | !all(by %in% names(value))) stop("Some index key variables are not in source or target object.")
   if (any(duplicated(value[by]))) stop("Duplicate index keys in the source object.")
   
   # Identify variables to import:
   vars <- setdiff(names(value), by)
   
   # Append results:
   if (length(vars) > 0){
      index <- gulf.utils::match(value[by], x[by])
      x[vars] <- NA
      x[index, vars] <- value[vars]
   }

   return(x)
}

#' @describeIn import-set Snow crab set import assignment method.
#' @export
"import<-.scsset" <- function(x, value, fill.na, ...){
   names <- names(x)
   class <- class(x)
   class(x) <- "data.frame"
   import(x, ...) <- value
   class(x) <- class
   
   # Fill NA values:
   if (!missing(fill.na)){
      vars <- setdiff(names(value), gulf.metadata::key(x))
      tmp <- x[vars]
      tmp[is.na(tmp)] <- fill.na
      x[vars] <- tmp
   }
   
   return(x)
}
