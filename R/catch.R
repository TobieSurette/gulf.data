#' Attach Catch Data
#' 
#' @description Attach catch data.
#' 
#' @param x Target object.
#'
#' @section Functions:
#' \describe{
#'   \item{\code{catch}}{Generic \code{catch} assignment method.}
#'   \item{\code{catch<-.scsset}}{Import catch data into an \code{\link{scsset}} object. }
#' }
#'     
#' @export "catch<-"
#' @rawNamespace S3method("catch<-", scsset)
#' 

#' @rdname catch
"catch<-" <- function(x, ...) UseMethod("catch<-")

#' @rdname catch
"catch<-.scsset" <- function(x, value, ...){
   args <- list(...)
   if (length(args) == 0) r <- summary(value, category = category(), ...) else r <- summary(value, ...)  
   vars <- setdiff(names(r), key(x))
   x[vars] <- NA
   index <- match(x[key(x)], r[key(x)])
   r <- r[index, vars]
   r[is.na(r)] <- 0
   x[vars] <- r

   return(x)
}

