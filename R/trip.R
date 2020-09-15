#' Trip Identifiers
#' 
#' @description Return unique identifiers for each fishing trip in a data set.
#' 
#' @param x Object containing trip information.
#' 
#' @examples 
#' x <- read.scsset(2019)
#' trip(x)
#' 
#' @section Methods:
#' \describe{
#'    \item{\code{trip}}{Generic \code{trip} method.}
#'    \item{\code{trip.scsset}}{Determine snow crab survey trip using an analysis of date sequences.}
#' }
#' 
#' @seealso \code{\link{scsset}}

#' @rdname trip
#' @export trip
trip <- function(x) UseMethod("trip")

#' @rdname trip
#' @export 
trip.scsset <- function(x){
   v <- rep(FALSE, nrow(x))
   v[c(1, which(diff(gulf.utils::julian(base::sort(gulf.utils::date(x)))) > 2)+1)] <- TRUE
   v <- cumsum(v)
   return(v)
}
