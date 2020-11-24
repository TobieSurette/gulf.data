#' Project Growth
#' 
#' @description Functions to estimate the deterministic or probabilistic future size of an organism. 
#' 
#' @param x Specimen size or length-frequency. Fish size units are usually in centimeters while those of crustaceans are 
#'          generally in millimeters. \code{x} may be a frequency table.
#' @param x0 Numeric value(s) at which probabilistic growth frequencies are to be estimated. Note that using 
#'           \code{x0} signals the function to use a probabilistic projection of growth, rather than a deterministic one.
#'           See examples.  
#' @param n Numeric value(s) specifying the number of iterations (e.g. number of moults) to apply the growth functions. 
#'          Non-integer values are allowed.
#'          
#' @examples
#' # Deterministic growth:
#' grow(60, species = "lobster")         # Post-moult size for a 60mm CL lobster.
#' grow(30:100, species = "lobster")     # Post-moult size for a range of lobster CL sizes.
#' grow(30:100, species = "snow crab")   # Post-moult size for a range of lobster CW sizes.
#' 
#' # Probabilistic growth:
#' x0 <- seq(60, 100, len = 1000)        # Post-moult size values at which we wish to know the probability density.
#' y0 <- grow(60, x0 = x0, species = "snow crab") # Estimate post-moult size probability densities for a single 60mm crab.
#' plot(x0, y0, type = "l", xlab = "Carapace width (mm)", ylab = "Probability density")
#' 
#' # Probabilistic growth for multiple observations:
#' x0 <- seq(60, 100, len = 1000)        # Post-moult size values at which we wish to know the probability density.
#' y0 <- grow(c(60, 60, 70, 75), x0 = x0, species = "snow crab") # Estimate post-moult size probability densities for a single 60mm crab.
#' plot(x0, y0, type = "l", xlab = "Carapace width (mm)", ylab = "Probability density")
#' 
#' # Probabilistic growth for large sample:
#' x <- read.scsbio(2020, category = "MI") # Immature males.
#' x0 <- seq(0, 140, len = 1000)
#' grow(x$carapace.width, species = "snow crab") # Deterministic.
#' plot(x0, grow(x$carapace.width, x0, species = "snow crab"), 
#'      type = "l", xlab = "Carapace width (mm)", ylab = "Frequency")
#'      
#' # Control number of moults:
#' grow(60, n = 0, species = "snow crab") # No moult.
#' grow(60, n = 1, species = "snow crab") # One moult (default).
#' grow(60, n = 2, species = "snow crab") # Two moults.
#' grow(c(60, 70, 80), n = c(0,1,2), species = "snow crab") # Three sizes with 0, 1 and 2 moults.

#' @export grow
grow <- function(x, ...) UseMethod("grow")

#' @export
grow.default <- function(x, x0, n, ...){
   # Parse 'n':
   if (!missing(n)){
      if (length(n) == 1) n <- rep(n, length(x))
      if (length(x) != length(n)) stop("'x' and 'n' have inconsistent lengths.")
   }else{
      n <- rep(1, length(x))
   }
   N <- max(floor(n)) + 1  # Maximum number of iterations to perform.
   
   # Apply probabilistic growth:
   if (!missing(x0)){
      # Define growth functions:
      mu <- growth(...)
      sigma <- growth(error = TRUE, ...)      
     
      # Generate frequency table:
      t <- NULL
      if (!is.null(names(x))) if (all(gsub("[.0-9-]", "", names(x)) == "")) t <- x
      if (is.null(t)) t <- table(x)
      
      # Sum over densities:
      v <- rep(0, length(x0))
      for (i in 1:length(t)){
         mi <- as.numeric(names(t[i])) # Initial size.
         fi <- as.numeric(t[i])        # Frequency. 
         v <- v +  fi * dnorm(x0, mi + mu(mi), sigma(mi))
      }
   }else{
      v <- x
      for (i in 1:N){
         ix <- which(n >= i)
         v[ix] <- v[ix] + growth(v[ix], ...)
      }
   }

   return(v)
} 

#' @export
grow.scsbio <- function(x, ...) return(grow(x$carapace.width, species = "snow crab", ...))
   
#' @export
grow.nssbio <- function(x, ...){
   v <- grow(x$length, species = "lobster", ...)
   return(v)
}

#' @export
grow.nsslen <- function(x, ...){
   v <- grow(x$length, species = "lobster", ...)
   return(v)
}

