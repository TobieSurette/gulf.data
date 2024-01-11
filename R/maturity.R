#' Sexual Maturity Functions
#'
#' @name maturity
#' 
#' @description Determines whether an organism is sexually mature, based on qualitative or 
#'              morphometric observational data.
#'
#' @param x Object or numeric maturity code.
#' @param probability Logical value specifying whether values are to be returned as 
#'                    probabilities of being mature. For individuals with missing morphometric 
#'                    data or observational sexual characters, a GAM binary regression is 
#'                    performed on the observations by year and the predicted probabilties are 
#'                    returned.
#' @param year Survey year.
#' @param species Species name or code.
#' @param ... Further arguments passed onto internal functions.
#' 
#' @return Logical vector, probability (when \code{probability = TRUE}), or character string.
#' 
#' @examples
#' x <- read.scsbio(2010)
#' index <- is.mature(x)
#' plot(x$carapace.width[index], x$chela.height[index], xlim = c(0, 140), ylim = c(0, 40), 
#'      xlab = "Carapace width (mm)", ylab = "Chela height (mm)", type = "n")
#' points(x$carapace.width[index], x$chela.height[index], pch = 21, bg = "grey", cex = 0.4)
#' points(x$carapace.width[!index], x$chela.height[!index], pch = 21, bg = "red", cex = 0.4)
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{is.mature}}{Generic \code{is.mature} function.}
#'   \item{\code{is.mature.scsbio}}{Snow crab biological data (\code{scsbio}) maturity method.}
#'   \item{\code{is.primiparous}}{Generic \code{is.primiparous} function.}
#'   \item{\code{is.primiparous.scsbio}}{Returns whether a female is primiparous, i.e. a first-time spawner.}
#'   \item{\code{is.multiparous}}{Generic \code{is.multiparous} function.}
#'   \item{\code{is.multiparous.scsbio}}{Returns whether a female is multiparous, i.e. has previously spawned.}
#'   \item{\code{is.senile}}{Generic \code{is.senile} function.}
#'   \item{\code{is.senile.scsbio}}{Returns whether an animal is senile, i.e. has decreased spawning capability because of old age.}
#'   \item{\code{maturity}}{Generic \code{weight} method.}
#'   \item{\code{maturity.default}}{Return sexual maturity status.}
#'   \item{\code{maturity.numeric}}{Convert numeric codes to descriptive maturity character strings.}
#' }
#' 
 
#' @export is.mature
is.mature <- function(x, ...) UseMethod("is.mature")

#' @rdname maturity
#' @export is.mature.scsbio
#' @export
is.mature.scsbio <- function(x, probability = FALSE, ...){
   # Initialize result vector:
   mat <- as.logical(rep(NA, dim(x)[1]))

   # Male discriminant function:
   ix <- which(!is.na(x$sex) & !is.na(x$carapace.width) & !is.na(x$chela.height) & (x$sex == 1))
   if (length(ix) > 0) mat[ix] <- (-0.7889259*log(x$carapace.width[ix]) + 0.6144883*log(x$chela.height[ix]) + 1.7605142) >= 0
   
   # New correction for chela heights that are too large:
   #iy <- which(log(x$chela.height[ix]) > (-2.88 + 1.35 * log(x$carapace.width[ix])))
   #mat[ix[iy]] <- NA
   
   # Small males:
   mat[(x$sex == 1) & (x$carapace.width < 40)] <- FALSE
   # Large males:
   mat[(x$sex == 1) & (x$carapace.width > 125)] <- TRUE

   # Females:
   if (length(which((x$sex == 2))) > 0){
      ix <- !is.na(x$sex) & !is.na(x$carapace.width) & !is.na(x$abdomen.width) & (x$sex == 2)
      mat[ix] <- (16.422757 * log(x$carapace.width[ix]) - 14.756163 * log(x$abdomen.width[ix]) - 14.898721) < 0
      mat[is.na(mat) & !is.na(x$sex) & (x$sex == 2) & !is.na(x$gonad.colour) & (x$gonad.colour >= 1)] <- FALSE
      mat[is.na(mat) & !is.na(x$sex) & (x$sex == 2) & (!is.na(x$egg.colour) | !is.na(x$eggs.remaining))] <- TRUE
   }

   # Perform GAM regressions for individuals with NA maturity values:
   if (probability){
      years <- sort(unique(gulf.utils::year(x)))
      for (i in 1:length(years)){
         ix <- !is.na(mat) & (gulf.utils::year(x) == years[i]) & !is.na(x$carapace.width) & !is.na(x$sex)
         sexf <- as.factor(x$sex)
         mat <- as.numeric(mat)
         y <- data.frame(mat = mat[ix], cw = x$carapace.width[ix], sex = sexf[ix])
         model <- mgcv::gam(mat ~ s(cw, by = sex), family = binomial, data = y)
         ix <- is.na(mat) & (gulf.utils::year(x) == years[i]) & !is.na(x$carapace.width) & !is.na(x$sex)
         newdata <- data.frame(cw = x$carapace.width[ix], sex = sexf[ix])
         mat[ix] <- predict(model, newdata = newdata)
         mat[ix] <- exp(mat[ix]) / (1 + exp(mat[ix]))
      }
   }

   return(mat)
}

#' @rdname maturity
#' @export is.pubescent
is.pubescent <- function(x, ...) UseMethod("is.pubescent")

#' @rdname maturity
#' @export is.pubescent.scsbio
is.pubescent.scsbio <- function(x, ...){
   v <- rep(NA, nrow(x))
   x$year <- year(x)
   years <- unique(x$year)
   if (length(years) > 0){
      for (i in 1:length(years)){
         # Immture female index:
         ix <- which((x$sex == 2) & !is.mature(x) & (x$year == years[i]) & !is.na(x$carapace.width) & x$carapace.width <= 102)
         
         if (length(ix) > 0){
            # Immture female with white or orange gonads:
            iy <- which(x$gonad.colour[ix] %in% c(1,3))
            
            if (length(iy) > 0){
               # Hard classification into pubescents or immatures:
               z <- rep(NA, length(iy))
               z[(x$gonad.colour[ix][iy] == 3) & (x$carapace.width[ix][iy] >= 30)] <- 1
               z[(x$gonad.colour[ix][iy] == 3) & (x$carapace.width[ix][iy] < 30)]  <- 0
               z[x$gonad.colour[ix][iy] == 1] <- 0
               v[ix[iy]] <- z # Add classification.
               
               # Identify crab with beige gonads:
               iz <- which(x$gonad.colour[ix] == 2)
               
               if (length(iz) > 0){
                  # Logistic regression of pubescents:
                  cw <- x$carapace.width[ix][iy]
                  model <- glm(z ~ cw, family = binomial())
                  
                  # Classify females with beige gonads according to logistic regression:
                  newdata <- data.frame(cw = x$carapace.width[ix][iz])
                  p <- predict(model, newdata = list(cw = x$carapace.width[ix][iz]))
                  v[ix[iz]] <- (p >= 0) + 1 - 1 
               }
            }
         }
      }
   }
   
   return(v == 1)
}

#' @rdname maturity
#' @export is.primiparous
is.primiparous <- function(x, ...) UseMethod("is.primiparous")

#' @rdname maturity
#' @export is.primiparous.scsbio
is.primiparous.scsbio <- function(x, ...){
   # Returns whether a crab is newly moulted.

   # Contruct logical vextor:
   ix <- rep(TRUE, dim(x)[1])
   names(x) <- tolower(names(x))
   ix <- ix * is.mature(x, ...) * (x$sex == 2) * is.new.shell(x)
   ix[x$sex == 1] <- 0
   ix[is.na(ix) & (x$sex == 2) & !is.na(x$gonad.colour)] <- 0
   ix[is.na(ix) & (x$sex == 2) & !is.na(x$egg.colour) & !is.na(x$eggs.remaining) & is.new.shell(x)] <- 1

   # Convert to logical if there are no fractions:
   if (all((ix[!is.na(ix)] %% 1) == 0)) ix <- (ix == 1)
   
   return(ix)
}

#' @rdname maturity
#' @export is.multiparous 
is.multiparous <- function(x, ...) UseMethod("is.multiparous")

#' @rdname maturity
#' @export is.multiparous.scsbio
is.multiparous.scsbio <- function(x, ...){
   # Returns whether a crab is newly moulted.

   # Construct logical vector:
   ix <- rep(TRUE, dim(x)[1])
   names(x) <- tolower(names(x))
   ix <- ix * is.mature(x, ...) * (x$sex == 2) * !is.new.shell(x)
   ix[x$sex == 1] <- 0
   ix[is.na(ix) & (x$sex == 2) & !is.na(x$egg.colour) & (x$eggs.remaining > 1) & !is.new.shell(x)] <- 1
      
   # Convert to logical if there are no fractions:
   if (all((ix[!is.na(ix)] %% 1) == 0)) ix <- (ix == 1)
 
   return(ix)
}

#' @rdname maturity
#' @export is.senile
is.senile <- function(x, ...) UseMethod("is.senile")

#' @rdname maturity
#' @export is.senile.scsbio
is.senile.scsbio <- function(x, ...){
   # Construct logical vector:
   ix <- rep(TRUE, dim(x)[1])
   names(x) <- tolower(names(x))
   
   # Determine maturity:
   maturity <- is.mature(x, ...)
   
   # Set variable:
   temp <- (gulf.utils::year(x) <= 1991)
   ix[temp] <- maturity[temp] * (x$sex[temp] == 2) * (x$shell.condition[temp] %in% 3) * (x$eggs.remaining[temp] %in% 0:1)
   ix[!temp] <- maturity[!temp] * (x$sex[!temp] == 2) * (x$shell.condition[!temp] %in% 4:5) * (x$eggs.remaining[!temp] %in% 0:1)
   ix[x$sex == 1] <- 0
   
   # Convert to logical if there are no fractions:
   if (all((ix[!is.na(ix)] %% 1) == 0)) ix <- (ix == 1)
   
   return(ix)
}

#' @rdname maturity
#' @export maturity
maturity <- function(x, ...) UseMethod("maturity")

#' @rdname maturity
#' @export
maturity.default <- function(x, ...){
   ix <- is.mature(x)
   v <- rep("", length(ix))
   v[ix] <- "mature"
   v[!ix] <- "immature"
   return(v)
}

#' @rdname maturity
#' @export
maturity.numeric <- function(x, year, species, ...){
   if (missing(years)) year <- as.numeric(substr(Sys.time(), 1, 4))
   
   # Standard codes from 1983-:
   values <- c(0, 1, 2, 4, 5, 6, 8, 9) # State acceptable code values.
   descriptions <- c("observed but undetermined", #0
                     "immature",                  #1
                     "almost ripe",               #2
                     "ripe",                      #4
                     "running ripe",              #5
                     "spent",                     #6
                     "not ripe",                  #8
                     "not observed")              #9
                    
   # Standard codes up to 1982:
   if (year <= 1982){      
      values <- 0:9 # State acceptable code values.
      descriptions <- c("observed but undetermined", #0
                        "immature",                  #1
                        "ripening 1",                #2
                        "ripening 2",                #3
                        "ripe",                      #4
                        "spawning",                  #5
                        "spent",                     #6
                        "recovering 1",              #7
                        "resting or recovering 2",   #8
                        "not observed")              #9
   }   

   # Codes for American plaice (1997- ):
   if (!missing(species)){
      if (species == 40){
         if (year >= 1997){ 
            values <- 0:9 # State acceptable code values.
            descriptions <- c("observed but undetermined", #0
                              "immature",                  #1
                              "early development",         #2
                              "pre-maturing",              #3
                              "maturing",                  #4
                              "ripe",                      #5
                              "spawning",                  #6
                              "spent",                     #7
                              "resting",                   #8
                              "not observed")              #9
         }
      }
     # Codes used for skates:
     if (is.skate(species)){
       
       values <- 1:5 # State acceptable code values.
       descriptions <- c("immature",                  #1
                         "maturing 1",                #2
                         "maturing 2",                #3
                         "mature",                    #4
                         "spawning")                  #5
     }
   }
                             
   # Lookup codes:
   v <- descriptions[match(x, values)]
   
   return(v)
}

