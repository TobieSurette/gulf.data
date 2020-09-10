#' Catch Data
#' 
#' @name catch
#' 
#' @description Extract or attach or catch data.
#' 
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' 
#' 
#' @section Functions:
#' \describe{
#'    \item{\code{catch}}{Generic \code{catch} method.}
#'    \item{\code{catch<-}}{Generic \code{catch} assignment method.}
#'    \item{\code{catch<-.scsset}}{Import catch data into an \code{\link{scsset}} object.}
#' }
#' 

#' @export
"catch" <- function(x, ...) UseMethod("catch")

#' @export
catch.scsset <- function(x, category = "T", ...){
   b <- read.scsbio(x, ...) 
   b <- b[b$tow.id != "" , ]
   res <- summary(b, category = category, by = c("year", "tow.id"), ...)
   x[category] <- 0
   index <- match(res[c("year", "tow.id")], x[c("year", "tow.id")])
   if (any(is.na(index))) stop("Some biological data was not matched to the tow data.")
   x[index, category] <- res[category]
   
   return(x)
}


#' @rdname catch
#' @export
"catch<-" <- function(x, ...) UseMethod("catch<-")

#' @rdname catch
#' @export 
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
