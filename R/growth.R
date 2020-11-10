#' Biological Growth Functions
#' 
#' @description Animal growth can be expressed in various ways. Fish species growth is generally modeled as 
#'              length-at-age, while crustacean growth is normally modeled as growth-at-moult size increment, 
#'              i.e. the expected size increase. An common example of a fish growth model is the von Bertalanffy 
#'              curve. For crustaceans, the discrete nature of growth and maturation frequently induce a stepwise 
#'              behaviour. Thus, crustacean growth-at-moult increments are well modeled by a two-component piecewise 
#'              linear model, with the first component describing immature growth, which is relatively fast, and the 
#'              second component describing adolescent and mature growth, which has a lower relative growth rate.
#' 
#' @param x Specimen size, length-frequency or other object.
#' @param species Species.
#' @param sex Biological sex.
#' @param n Length-frequencies 
#' @param theta Named parameter for growth model.
#' @param error Logical value specifying whether to return the estimated standard error associated with growth. 
#'              Note that this does not correspond to estimation error, but rather the prediction error.
#' 
#' @return If \code{x} is left unspecified, then a function is returned which can be used for evaluating 
#'         growth for given size inputs. 
#'         
#' @note Note that growth increments are returned rather than the new size.
#'       
#' @examples 
#' # Snow crab growth parameters:
#' theta <- c(intercept = 0.276, transition = 38.2, slope = c(0.32, 0.126), window = 1.6, log_sigma = -2)
#' x <- seq(10, 120)
#' y <- growth(x, theta = theta, error = TRUE) # Include error estimate in the output.  
#' plot(x, y$mu, type = "l")
#' lines(x, y$mu - y$sigma, lty = "dashed")
#' lines(x, y$mu + y$sigma, lty = "dashed")

#' @export growth
growth <- function(x, ...) UseMethod("growth")

#' @describeIn growth Default growth function.
#' @export
growth.default <- function(x, species, sex, theta, error = FALSE, as.matrix = FALSE, ...){
   # Define growth parameters for various species:
   if (missing(theta)){
      if (missing(species)) stop("'species' must be specified.")
      
      # Default parameters:
      if (species == 2526) theta <- c(intercept = 0.276, transition = 38.2, slope = c(0.32, 0.126), window = 1.6, log_sigma = -2) # Snow crab.
      if (species == 2550) theta <- c(intercept = 0.168, transition = 40.63, slope = c(0.242, 0.242), window = 3.69, log_sigma = -2) # American lobster.
   }

   names(theta) <- tolower(names(theta))
   
   # Define mean function:
   mu <- splm(theta = theta)
   if (length(grep("sigma", names(theta))) == 0) error <- FALSE
   
   # Return mean:
   if (!error & missing(x)) return(mu) 

   # Define error function:
   sigma <- function(x){
      # Logistic transition:
      eta <- (x - theta[["transition"]]) / exp(theta[["window"]])   
      p <-  1 / (1 + exp(-eta))   
         
      # Evaluate error portion:
      sigma <- theta[grep("sigma", names(theta))]
      sigma[order(names(sigma))]
      sigma <- exp(sigma)
      if (length(sigma) == 1) sigma <- rep(sigma, 2)
      v <- p * sigma[[1]] + (1-p) * sigma[[2]]
         
      return(v)
   }
      
   # Evaluate function and error:
   if (!missing(x)){
      # Check if inputs are size-frequencies:
      if (!is.null(names(x))){
         if (all(gsub("[0-9.-]", "", names(x)) == "")){
            m <- growth.matrix(as.numeric(names(x)), theta = theta)
            v <- (as.numeric(x) %*% m[names(x), ])[1, ]
            return(v)
         }
      }
      
      if (!error) return(mu(x)) else return(data.frame(mu = mu(x), sigma = sigma(x) * mu(x))) 
   }else{
      return(sigma)
   }
}

growth.matrix <- function(x, y0, theta, ...){
   ux <- sort(unique(x))
   dx <- min(diff(ux))
   x0 <- seq(min(ux), max(ux), by = dx)
   if (length(x0) > 1000) stop("Growth matrix dimensions exceed 1000x1000.")
   
   # Define growth means and standard errors:
   m <- growth(theta = theta)(x0) 
   s <- growth(theta = theta, error = TRUE)(x0)
   s <- s * m # Error proportional to mean.
   
   # Calculate corresponding gamma parameters:
   phi <- s^2 / m # Scale parameter.
   k <- m^2 / s^2 # Shape parameter.
   
   # Define growth output vector:
   if (missing(y0)) y0 <- seq(0, max(m + 3 * s), by = dx) 
   
   # Growth increment matrix:
   I <- matrix(NA, nrow = length(x0), ncol = length(y0))
   dimnames(I) <- list(x = x0, y = y0)
   for (i in 1:length(x0)){
      p <- pgamma(y0+dx/2, k[i], 1/phi[i]) - pgamma(y0-dx/2, k[i], 1/phi[i])
      I[i,] <- pgamma(y0+dx/2, k[i], 1/phi[i]) - pgamma(y0-dx/2, k[i], 1/phi[i])
   }
   
   # Map growth increments onto growth matrix:
   G <- matrix(0, nrow = length(x0), ncol = length(seq(min(x0), max(x0) + max(y0), by = dx)))
   dimnames(G) <- list(x = x0, y = seq(min(x0), max(x0) + max(y0), by = dx))
   for (i in 1:length(x0)){
      gvars <- as.character(seq(x0[i], x0[i] + max(y0), by = dx))
      vvars <- as.character(seq(0, max(y0), by = dx))
      index <- gvars %in% colnames(G)
      G[i,gvars[index]] <- I[i,vvars[index]]
      
   }
   
   return(G)
}

