#' Truncate Trawl Probe or Sensor Data
#' 
#' Truncates time series data from a trawl's probe or acoustic sensor data
#' using touchdown and liftoff times.
#' 
#' If the set card if left unspecified, then the function attempts to load the
#' set card automatically. Start and end times (touch down and lift off times)
#' are then used to truncate the data.
#' 
#' @aliases truncate truncate.minilog truncate.scanmar truncate.netmind
#' truncate.esonar
#' @param x An \bold{R} object.
#' @param set.card A set card object.
#' @param buffer A scalar or two-element numeric vecotr specifying the number
#' of minutes prior to and after the observed start and end times (i.e. touch
#' down and lift off times) to retain after truncation.
#' @param ... Not used.
#' @return Returns a list of \sQuote{minilog}, \sQuote{scanmar},
#' \sQuote{netmind} or \sQuote{esonar} objects.
#' @author Tobie Surette \email{Tobie.Surette@@dfo-mpo.gc.ca} \cr Pablo Vergara
#' \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @seealso \code{\link[gulf]{minilog class}}, \code{\link[gulf]{scanmar
#' class}}, \code{\link[gulf]{netmind class}}, \code{\link[gulf]{esonar class}}
#' @keywords IO
#' @examples
#' 
#'    # Get list of available minilog data files from the 2013 snow crab survey:
#'    files <- minilog.file.str(year = 2013, survey = "sc")
#' 
#'    # Read the first netmind file:
#'    x <- read.minilog(file = files[1:10])
#' 
#'    # Truncate then split the data into individual tows:
#'    y <- split(truncate(x))
#' 
#'    # Plot the first tow:
#'    plot(y[[1]])
#' 
truncate <- function(x, ...){
   # TRUNCATE - Generic 'truncate' method.
   UseMethod("truncate")
}
