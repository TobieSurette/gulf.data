#' @title Chela Height 
#' 
#' @description Functions to extract or estimate crab chela height measurements.
#' 
#' 

#' @export chela.height
chela.height <- function(x, ...) UseMethod("chela.height")

#' @rawNamespace S3method(chela.height,scsbio)
chela.height.scsbio <- function(x) return(x$chela.height)
   
#' @rawNamespace S3method(chela.height,scobs)
chela.height.scobs <- function(x, by = "observer", adjust = FALSE, recalculate = FALSE){
   # CHELA.HEIGHT.SCOBS - Chela height adjustment function for snow crab observer data.
   
   if (!adjust) return(x$chela.height.right)
   
   # Fit regression mixture to chela height versus carapace width from the snow crab survey:
   if (recalculate){
      b <- read.scbio(year = unique(x$year))
      index <- b$sex == 1 & !is.na(b$carapace.width) & !is.na(b$chela.height) & (b$carapace.width > 0) & (b$carapace.width < 160) & (b$chela.height > 0)
      b <- b[index, ]
   
      # Mixture regression log-likelihood function:
      ll <- function(theta, x, y){
         # Parse parameter vector:
         alpha <- theta[1:2]     # Intercepts.
         beta <- theta[3:4]      # Slopes.
         sigma <- exp(theta[5:6])# Errors.
         phi <- theta[7:8]       # Logistic parameters.
   
         # Regression proportions:
         p <- exp(phi[1] + phi[2] * x)
         p <- p / (1 + p)
      
         # Linear models:
         mu <- cbind(alpha[1] + beta[1] * log(x), alpha[2] + beta[2] * log(x))
      
         # Log-probability density:
         v <- log(p * dnorm(log(y), mu[,1], sigma[1]) + (1-p) * dnorm(log(y), mu[,2], sigma[2]))
      
         return(-sum(v))
      }
   
      # Initialize parameter values:
      b$mature <- is.mature.scbio(b)
      m1 <- lm(log(chela.height) ~ log(carapace.width), data = b[b$mature, ])
      m2 <- lm(log(chela.height) ~ log(carapace.width), data = b[!b$mature, ])
      m3 <- glm(mature ~ carapace.width, data = b, family = "binomial")
      theta <- c(coef(m1)[1], coef(m2)[1], coef(m1)[2], coef(m2)[2], log(summary(m1)$sigma), log(summary(m2)$sigma), coef(m3))
   
      # Evaluate log-likelihood as a test prior to fitting:
      ll(theta, b$carapace.width, b$chela.height) 
      
      # Estimate parameters:
      res <- optim(theta, ll, x = b$carapace.width, y = b$chela.height, control = list(trace = 3, maxit = 5000))
      theta <- res$par
   }else{ 
      # Use parameters estimated from the 2016 snow crab survey:
      theta <- c(-3.051,-2.846,1.356,1.259,-2.994,-2.92,-6.262,0.069)
   }
   
   # Probability density function for a fixed regression mixture, but with linearly transformed chela values:
   obj <- function(delta, x, y, theta){
      # Parse 'theta' parameter vector:
      alpha <- theta[1:2]     # Intercepts.
      beta <- theta[3:4]      # Slopes.
      sigma <- exp(theta[5:6])# Errors.
      phi <- theta[7:8]       # Logistic parameters.
   
      # Regression proportions:
      p <- exp(phi[1] + phi[2] * x)
      p <- p / (1+ p)
      
      # Linear models:
      mu <- cbind(alpha[1] + beta[1] * log(x), alpha[2] + beta[2] * log(x))
      
      ym <- delta[1] + y 
            
      # Log-probability density:
      v <- p * dnorm(log(ym), mu[,1], sigma[1]) + (1-p) * dnorm(log(ym), mu[,2], sigma[2])
      v[v == 0] <- 1E-9

      return(-sum(log(v)))
   }
   
   # Break data frame into component parts:
   tmp <- by(x, x[by], function(x) return(x))
   
   v <- rep(NA, nrow(x))
   # clg()
   for (i in 1:length(tmp)){
      index <- which(tmp[[i]]$sex == 1 & !is.na(tmp[[i]]$carapace.width) & !is.na(tmp[[i]]$chela.height.right) & 
                     (tmp[[i]]$carapace.width > 0) & (tmp[[i]]$carapace.width < 160) & (tmp[[i]]$chela.height.right > 0))
   
      # Remove odd values:
      m <- lm(chela.height.right ~ carapace.width, data = tmp[[i]][index, ])
      r <- residuals(m, type = "pearson")
      index <- index[(r >= -6) & (r <= 6)]

      # Find initial value:
      init <- mean(tmp[[i]]$chela.height.right[index] - exp(1.356 * log(tmp[[i]]$carapace.width[index]) -3.051))
      res <- optim(init, obj, x = tmp[[i]]$carapace.width[index], y = tmp[[i]]$chela.height.right[index], theta = theta, control = list(trace = 0, maxit = 5000))
      delta <- res$par
      
      ref <- obj(0, x = tmp[[i]]$carapace.width[index], y = tmp[[i]]$chela.height.right[index], theta = theta)
   
      index <- match(tmp[[i]][key(x)], x[key(x)])
      if ((ref - res$value) > 10) v[index] <- delta[1] + tmp[[i]]$chela.height.right else v[index] <- tmp[[i]]$chela.height.right
      
      #windows()
      #plot(b$carapace.width, b$chela.height)
      #points(tmp[[i]]$carapace.width, tmp[[i]]$chela.height.left, bg = "red", pch = 21)
      #points(tmp[[i]]$carapace.width, tmp[[i]]$chela.height.right, bg = "green", pch = 21)
   }
   
   return(v)
}


