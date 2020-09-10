#' Trip Identifiers
#' 
#' @description Return unique identifiers for each fishing trip in a data set.
#' 
#' @param x Object containing trip information.
#' 
#' @examples 
#' x <- read.scsset(2019)
#' trip(x)

#' @describeIn trip Generic \code{trip} method.
#' @export 
trip <- function(x) UseMethod("trip")

#' @describeIn trip \code{scsset} \code{trip} method, using sequence of dates.
#' @export 
trip.scsset <- function(x){
   index <- rep(FALSE, nrow(x))
   index[c(1, which(diff(gulf.utils::julian(sort(gulf.utils::date(x)))) > 2)+1)] <- TRUE
   return(cumsum(index))
}
