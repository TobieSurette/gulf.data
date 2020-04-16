#' Sort a 'minilog' Object.
#' 
#' Sorts a 'minilog' object by time.
#' 
#' This method uses the \code{\link[base]{order}} function.
#' 
#' @param x A 'minilog' object.
#' @param decreasing A logical value stating whether the order to be returned
#' should be in decreasing order or not.
#' @return A sorted version of the 'minilog' object \code{x} is returned.
#' @author Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr Pablo Vergara
#' \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @seealso \code{\link[base]{order}}
#' @examples
#' # Read tow ID GP001F from the 2010 snow crab survey:
#' x <- read.minilog("GP001F, year = 2010)
#' x <- sort(x)

sort.minilog <- function(x, decreasing = FALSE) return(x[order(time(x), decreasing = decreasing), ])

