#' Biological Growth Functions
#' 
#' @description Animal growth can be expressed in various ways. Fish species growth is 
#' generally modeled as length-at-age, while crustacean growth is normally modeled as
#' growth-at-moult size increment, i.e. the expected size increase. An common example 
#' of a fish growth model is the von Bertalanffy curve. For crustaceans, the discrete 
#' nature of growth and maturation frequently induce a stepwise behaviour. Thus, crustacean 
#' growth-at-moult increments are well modeled by a two-component piecewise linear model, 
#' with the first component describing immature growth, which is relatively fast, and the 
#' second component describing adolescent and mature growth, which has a lower relative 
#' growth rate.
#' 
#' @param x Specimen size, length-frequency or other object.
#' @param n Length-frequencies 
#' 
#' @details Crustacean growth-at-moult models have just three parameters, an initial slope
#' parameter, a transition window width (exp(log.w)) and a transition point xp. These 
#' functions generate the expected values for a given set of parameters. Also the assumed
#' error was assumed to be increasing with size.
#' 
#' @return 
#' 
#' If \code{x} is left unspecified, then a function is returned which can be used to 
#' evaluate growth if given inputs.

#' @export grow
grow <- function(x, ...) UseMethod("grow")

# Logistic function:
logistic <- function(x, xp = 0, w = 1){
   u <- (2 / w) * log(39) * (x-xp)
   v <- 1 / (1 + exp(-u))
   return(v)
}

# Integrated logistic function:
ilogistic <- function(x, xp = 0, w = 1){
   u <- (2 / w) * log(39) * (x-xp)
   v <- (w / (2 * log(39))) * log(1 + exp(u))
   return(v)
}

# Define two-component smoothed piecewise linear model mean:
mu <- function(x, xp, yp, log.w, slope, theta, ...){
   # Parse 'theta' parameter vector:
   if (!missing(theta)){
      xp = as.numeric(theta[grep("xp", names(theta))])
      yp = as.numeric(theta[grep("yp", names(theta))])
      w = exp(as.numeric(theta[grep("log.w", names(theta))]))
      slope = as.numeric(theta[grep("slope", names(theta))])
   }
   
   # Window width parameter:
   if (!missing(log.w)) w <- exp(log.w)
   
   # Smoothed piecewise-linear model:
   v <- yp + slope[1] * x + (slope[2] - slope[1]) * ilogistic(x, xp, w)
   
   return(v)
}

# Define two-component smoothed piecewise linear model standard error:
sigma <- function(x, mu, log.sigma, theta, ...){
   if (missing(mu)) mu <- mu(x, theta = theta, ...)
   
   # Define sigma parameters:
   if (missing(log.sigma)) log.sigma <- as.numeric(theta[grep("log.sigma", names(theta))])
   sigma <- exp(log.sigma)
   
   # Different errors for each phase:
   if (length(sigma) == 2){ 
      xp <- theta[["xp"]]
      w <- exp(theta[["log.w"]])
      p <- logistic(x, xp = xp, w = w)  
      
      sigma <- p * sigma[1] + (1-p) * sigma[2]
   }
   
   # Scale error with mean:
   v <- sigma * mu
   
   return(v)
}

#' @export
grow.default <- function(x, species, sex, theta, error = FALSE, ...){
   # Define growth parameters for various species:
   if (missing(theta)){
      if (missing(species)) stop("'species' must be specified.")
      
      # Snow crab:
      if (species == 2526){ 
         theta <- c(xp = 38.2, yp = 12.5, 
                    slope = c(0.32, 0.126),
                    log.w = log(5),
                    log.sigma = log(0.26))
      }
      
      # Lobster:
      if (species == 2550){ 
         theta <- c(xp = 40.63, yp = 10, 
                    slope = c(0.242, 0.242),
                    log.w = 3.69, 
                    log.sigma = -2.5)
      }      
   }

   # Define mean and standard error:
   m <- mu(x, theta = theta)
   if (!error) return(m)
   
   # Define standard error:
   s <- sigma(mu = m, theta = theta)
   v <- data.frame(mu = m, sigma = s)
   
   return(v)
}

#' @export
grow.scsbio <- function(x, ...) return(grow(x$carapace.width, species = 2526, ...))
   
