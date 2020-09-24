#' Biological Growth Functions
#' 
#' @name growth
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

#' @export
grow <- function(x, ...) UseMethod("grow")

# Smoothed piecewise-linear model polynomial kernel:
phi <- function(x){
   v <- rep(NA, length(x))
   i <- (x >= 0) & (x < 0.5)
   v[i] <- -(8/5)*x[i]^5 + 2*x[i]^4
   i <- (x >= 0.5) & (x < 1)
   v[i] <- (8/5)*(x[i]-0.5)^5 - 2*(x[i]-0.5)^4 + (x[i]-0.5)^2 + 0.5*(x[i]-0.5) + 3/40
   v[x <= 0] <- 0
   v[x >= 1] <- x[x >= 1] - 0.5
   return(v)
}

mu <- function(x, xp, yp, log.w, slope, theta){
   if (!missing(theta)){
      xp = as.numeric(theta[grep("xp", names(theta))])
      yp = as.numeric(theta[grep("yp", names(theta))])
      w = exp(as.numeric(theta[grep("log.w", names(theta))]))
      slope = as.numeric(theta[grep("slope", names(theta))])
   }
   if (!missing(log.w)) w <- exp(log.w)
   
   if (length(slope) == 1) slope <- rep(slope, 2)
   
   # Smoothed piecewise-linear model:
   v <- yp + slope[1] * x + (slope[2] - slope[1]) * w * phi(((x-xp)/w) + 0.5)
   
   return(v)
}

sigma <- function(x, mu, log.sigma){
   
   
}

#' @rdname growth
#' @export
grow.default <- function(x, n, species, sex, theta){
   if (missing(species)) stop("'species' must be specified.")
   
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

   m <- mu(x, xp = theta[["xp"]], yp = theta[["xp"]], 
           log.w = theta[["log.w"]], slope = c(theta[["slope.immature"]], theta[["slope.adolescent"]])
   
   
   sigma <- exp(theta[["log.sigma"]]) * mu
     
   if (missing(x)) return()
   
   return(y)
}
   
# Define Hiatt growth matrix:
growth.matrix <- function(beta = c(0.32, 0.126), 
                          xp = 38.2, yp = 12.5, k = 7,
                          sigma = c(0.37, 0.25),
                          xlim = c(0, 140), step = 1, output = "matrix"){
   
   w <- step / 2
   
   # Name input arguments:
   names(beta) <- c("immature", "pubescent")
   names(sigma) <- c("immature", "pubescent")
   
   x <- seq(xlim[1], xlim[2], by = step)
   u <- exp(-k * diff(beta) * (x - xp))
   mu <- -(1/k) * log(1+u) + beta[["immature"]]*(x-xp) + yp + x 
   p <-  u / (1 + u)
   s <- (p * sigma[["immature"]] + (1-p)*sigma[["pubescent"]]) * (mu-x)
   s <- 0.5 * s
   
   # Calculate growth transition probabilities:
   G <- matrix(0, nrow = length(x), ncol = length(x))
   G[,1] <- pnorm((x[1]-mu+w) / s)
   for (j in 2:(length(x)-1)) G[,j] <- pnorm((x[j] - mu + w) / s) - pnorm((x[j] - mu - w) / s)
   G[,length(x)] <- 1-pnorm((x[length(x)]-mu-w)/s)
   G[length(x), ] <- 0
   G[length(x), length(x)] <- 1
   dimnames(G) <- list(pre = x, post = x)
   
   if (output == "matrix") return(G)
}

