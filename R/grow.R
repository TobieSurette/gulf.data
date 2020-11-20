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

#' @export
grow.default <- function(x, species, sex, theta, error = FALSE, ...){
   # Define growth parameters for various species:
   if (missing(theta)){
      if (missing(species)) stop("'species' must be specified.")
      species <- species(species)
      
      # Snow crab:
      if (species == 2526) theta <- c(intercept = 0.276, transition = 38.2, slope = c(0.32, 0.126), window = 1.6) 
   
      # American lobster:      
      if (species == 2550) theta <- c(intercept = 0.168, transition = 40.63, slope = c(0.242, 0.242), window = 3.69)
   }

   # Define mean function:
   mu <- splm(theta = theta)

   if (!error){
      if (missing(x)) return(mu) else return(mu(x))
   }else{
      sigma <- function(x){
         # Logistic:
         eta <- (x - theta[["transition"]]) / exp(theta[["window"]])   
         p <-  1 / (1 + exp(-eta))   
         
         # Evaluate error portion:
         sigma <- theta[grep("sigma", names(theta))]
         sigma[order(names(sigma))]
         sigma <- exp(sigma)
         v <- p * sigma[[1]] + (1-p) * sigma[[2]]
         
         return(v)
      }
      
      # Evaluate function and error:
      if (missing(x)){
         return(sigma) 
      }else{
         v <- data.frame(mu = mu(x), sigma = sigma(x) * mu(x))  
      }
   } 
   
   return(v)
}

#' @export
grow.scsbio <- function(x, ...) return(grow(x$carapace.width, species = 2526, ...))
   
