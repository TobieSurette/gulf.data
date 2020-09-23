#' Catch Data
#' 
#' @name catch
#' 
#' @description Extract or attach or catch data.
#' 
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' @param by Character string(s) specifying which variables to group by when summararizing.
#' @param category Biological category string(s). See \code{\link{cagetory}} for more details.
#' @param weight Logical value specifying whether to return a summary by weights rather than counts.
#' @param ... Other parameters (not used).
#' 
#' @section Functions:
#' \describe{
#'    \item{\code{catch}}{Generic \code{catch} method.}
#'    \item{\code{catch<-}}{Generic \code{catch} assignment method.}
#'    \item{\code{catch.scsset}}{Generate catch data summary for an \code{\link{scsset}} object.}
#'    \item{\code{catch.scsbio}}{Generate catch data summary for an \code{\link{scsbio}} object.}
#'    \item{\code{catch<-.scsset}}{Import catch data into an \code{\link{scsset}} object.}
#' }
#' 

#' @export
"catch" <- function(x, ...) UseMethod("catch")

#' @rdname catch
#' @export
catch.scsset <- function(x, ...){
   keyvars <- key.scsset()
   res <- catch(read.scsbio(x) , by = keyvars, ...)
   vars <- setdiff(names(res), keyvars)
   x[vars] <- 0
   index <- match(res[keyvars], x[keyvars])
   if (any(is.na(index))) stop("Some biological data was not matched to the tow data.")
   x[index, category] <- res[vars]
   
   return(x)
}

#' @rdname catch
#' @export
catch.scsbio <- function(x, by = key.scsset(), category, species, weight = FALSE, ...){
   if (!missing(category)){
      if (weight) w <- gulf.utils::repvec(weight(x, ...), ncol = length(category)) else w <- 1 
      I <- is.category(x, category)
      res <- stats::aggregate(I * w, by = x[by], sum, na.rm = TRUE)
      res <- sort(res, by = by)
   }
   
   return(res)
}
      
#' @rdname catch
#' @export
"catch<-" <- function(x, ...) UseMethod("catch<-")

#' @rdname catch
#' @export 
"catch<-.scsset" <- function(x, value, ...){
   keyvars <-  key.scsset()
   
   if (!all(keyvars %in% names(value))) stop("Assigned catches must contain 'scsset' index key.")
   
   # Identify numeric variables
   vars <- setdiff(names(x)[apply(value, is.numeric)], keyvars)
   
   if (length(vars) > 0){
      value <- aggregate(value[vars], by = value[keyvars], sum, na.rm = TRUE)
      index <- match(value[keyvars], x[keyvars])
      x[vars] <- 0
      x[index, vars] <- value[vars]
   }

   return(x)
}
