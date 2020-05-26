#' Species Group Membership
#'
#' @description Functions to check membership in various species groups.
#' 
#' @param species Numerical species code(s).
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{is.fish}}{Determine whether a species is an invertebrate.}
#'   \item{\code{is.invertebrate}}{Determine whether a species is an invertebrate.}
#'   \item{\code{is.skate}}{Determine whether a species is a skate.}
#'   \item{\code{is.shrimp}}{Determine whether a species was catgeroized as a shrimp historically.} 
#' }
#'
#' @export is.fish
#' @export is.invertebrate
#' @export is.skate
#' @export is.shrimp
#' 
is.fish <- function(species) return(species %in% c(1:899, 940, 950, 960, 965))

#' @rdname is.fish 
is.invertebrate <- function(species) return((species >= 1700) & (species < 9000))

#' @rdname is.fish 
is.skate <- function(species) return(species %in% c(200:212, 217, 219))

#' @rdname is.fish 
is.shrimp <- function(species){
   v <- species %in% c(2211:2213, 2220:2222, 2230, 2312, 2313, 2315, 2316, 2319, 
                       2331:2333, 2411, 2414, 2415, 2417, 2420, 2421)
   return(v)
}
