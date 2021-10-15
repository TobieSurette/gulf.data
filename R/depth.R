#' @title Depth
#' 
#' @description Return or calculate depth

#' @export
depth <- function(x, ...) UseMethod("depth")

#' @export
depth.data.frame <- function(x, ...){
   names(x) <- tolower(names(x))
   if ("depth" %in% names(x)) return(x$depth)
   if (length(grep("depth", names(x))) > 0) return(x[, grep("depth", names(x))[1]])
   return(NULL)
}
   
#' @export depth.esonar
depth.esonar <- function(x, ...){
   # Define log-likelihood function:
   loglike <- function(theta, x){
      x <- x[!is.na(x)]
   
      # Data range:
      xlim <- range(x)
   
      # Mixture parameters:
      p <- 1/ (1 + exp(theta[["logit.p"]]))
      mu <- theta[["mu"]]
      sigma <- exp(theta[["log.sigma"]])

      # Log-likelihood:
      v <- -sum(log((1-p) / diff(xlim) + p * dnorm(x, mu, sigma)))
   
      return(v)
   }
   
   # Initialize model parameters:
   x <- x[which(x$depth > 20), ]
   tab <- table(round(x$depth))
   theta <- c(logit.p = 0, mu = as.numeric(names(tab)[which.max(tab)]), log.sigma = log(5))
   
   # Fit mixture model:
   theta <- optim(theta, loglike, x = x$depth, control =  list(trace = 0))$par

   return(theta[["mu"]])
}

#' @rawNamespace S3method(depth,star.oddi)
depth.star.oddi <- function(x, method = "esonar"){
   tow.id <- tow.id(x)
   year <- unique(year(x))
   s <- read.scsset(tow.id = tow.id, year = year)

   # Look up depth using survey tow coordinates:
   if (method == "tow.id"){
      ix <- which((time(x) >= time(s, "start")) &  (time(x) <= time(s, "stop")))
      depth <- 1.852 * s$depth
      v <- depth * x$pressure / median(x$pressure[ix])
   }

   # Look up depth using survey tow coordinates:
   if (method == "latlon"){
      ix <- which((time(x) >= time(s, "start")) &  (time(x) <= time(s, "stop")))
      depth <- gulf.spatial::depth(gulf.spatial::lon(s), gulf.spatial::lat(s))
      v <- depth * x$pressure / median(x$pressure[ix])
   }

   # Look up depth using survey tow coordinates:
   if (method == "esonar"){
      e <- read.esonar(tow.id = tow.id, year = year)

      # Define time interval to perform analysis:
      start <- time(s, "start") - 90
      stop  <- time(s, "stop") + 90

      # Weed out eSonar data:
      e <- e[!is.na(e$depth), ]
      e <- e[(time(e) >= start) & (time(e) <= stop), ]

      ix <- match(time(e), time(x))
      beta <- coef(lm(e$depth ~ x$pressure[ix]))
      sigma <- sd(e$depth - beta[1] - beta[2] * x$pressure[ix])
      log.sigma <- log(c(sigma / 10, sigma))

      theta <- c(beta = as.numeric(rev(beta)), logit.p = 0, log.sigma = as.numeric(log.sigma))
      loglike <- function(theta, x, y){
         sigma <- exp(theta[grep("log.sigma", names(theta))])
         sigma[2] <- sigma[1] + sigma[2]
         p <- 1 / (1 + exp(theta[["logit.p"]]))
         beta <- theta[grep("beta", names(theta))]
         mu <- beta[1] + beta[2] * x
         d <- (1-p) * dnorm(y, mu, sigma[2]) + p * dnorm(y, mu, sigma[1])
         return(-sum(log(d)))
      }

      for (i in 1:10) theta <- optim(theta, loglike, x = x$pressure[ix], y = e$depth)$par
      beta <- theta[grep("beta", names(theta))]
      v <- beta[1] + beta[2] * x$pressure
   }

   return(v)
}
