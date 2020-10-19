#' Catch Data
#' 
#' @name catch
#' 
#' @description Extract or generate catch data summaries.
#' 
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' @param by Character string(s) specifying which variables to group by when summararizing.
#' @param category Biological category string(s). See \code{\link{category}} for more details.
#' @param weight Logical value specifying whether to return a summary by weights rather than counts.
#' @param ... Other parameters (not used).
#' 
#' @examples
#' s <- read.scsset(2020)  # Tow data.
#' b <- read.scsbio(2020)  # Biological data.
#' 
#' # Generate catch summaries from biological data:
#' catch(b) # Total crab.
#' catch(b, category = c("M", "F"), by = c("date"))
#' catch(b, category = c("COM"), by = c("date", "tow.id"))
#' catch(b, category = category(1:10), by = c("date", "tow.id"))

#' @export
"catch" <- function(x, ...) UseMethod("catch")

#' @describeIn catch Snow crab biological data catch summaries.
#' @export
catch.scsbio <- function(x, by = key.scsset(), category, species, weight = FALSE, ...){
   if (!missing(category)){
      if (weight) w <- gulf.utils::repvec(weight(x, ...), ncol = length(category)) else w <- 1 
      I <- is.category(x, category)
      res <- stats::aggregate(I * w, by = x[by], sum, na.rm = TRUE)
      names(res) <- c(by, category)
      res <- sort(res, by = by)
   }else{
      if (weight) w <- gulf.utils::repvec(weight(x, ...), ncol = 1) else w <- rep(1, nrow(x)) 
      res <- stats::aggregate(w, by = x[by], sum, na.rm = TRUE)
      names(res) <- c(by, "total")
      res <- sort(res, by = by)      
   }
   
   return(res)
}
