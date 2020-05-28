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
#' 
grow <- function(x, ...) UseMethod("grow")

# Crustacean growth model kernel:
f <- function(x){
   v <- rep(NA, length(x))
   i <- (x >= 0) & (x < 0.5)
   v[i] <- -(8/5)*x[i]^5 + 2*x[i]^4
   i <- (x >= 0.5) & (x < 1)
   v[i] <- (8/5)*(x[i]-0.5)^5 - 2*(x[i]-0.5)^4 + (x[i]-0.5)^2 + 0.5*(x[i]-0.5) + 3/40
   v[x <= 0] <- 0
   v[x >= 1] <- x[x >= 1] - 0.5
   return(v)
}

g <- function(x, xp, log.w = 0, slope) v <- slope * x - slope * exp(log.w) * f(((x-xp)/w) + 0.5)

#' @describeIn grow Defeult growth function.
grow.default <- function(x, n, species, sex, theta){
   # Snow crab growth increment:
   if (species == 2526){
      theta <- c(xp = 38.2, yp = 12.5, 
                 slope.immature = 0.32, slope.adolescent = 0.126, 
                 log.w = 5,
                 log.sigma = log(0.26))
   }
   
   # Atlantic lobster growth increment:
   if (species == 2550){
      theta <- c(xp = 40.63, yp = 10, 
                 slope.immature = 0.242, slope.adolescent = 0.242,
                 log.w = 3.69, 
                 log.sigma = -2.5)
   }

   mu <- g(x, xp = 40.63, log.w = 3.69, slope = 0.242)
   sigma <- exp(theta[["log.sigma"]]) * mu
     
   if (missing(x)) return()
   
   return(y)
}
   
