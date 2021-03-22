#' @title Harvest Control Rules
#'
#' @description Calculate harvest control rates.
#'
#' @param species Species name or code.
#'
#' @examples
#' x <- seq(0, 125000, len = 1000)
#' y <- TAC(x, species = "snow crab")
#' plot(range(x) / 1000, c(0, 0.5), type = "n", xlab = "Commercial biomass (x1000 t)", ylab = "Exploitation rate", yaxs = "i")
#' grid()
#' lines(x / 1000, y, lwd = 2)
#' box()

#' @export reference.points
reference.points <- function(species){
   if (missing(species)) stop("'species' must be specified.")
   if (is.character(species)) species <- species(species)

   # Snow crab:
   if (species == 2526){
      v <- c(Flim = 0.346,         # Limit reference point for exploitation rate.
             Blim = 9970,          # Limit reference point for residual commercial snow crab biomass.
             Bmax = 103427.5)      # Maximum observed commercial biomass.
      v["Busr"] <- 0.4 * v["Bmax"] # Upper stock reference point for commercial biomass of residual commercial snow crab.
   }

   return(v)
}

#' @export total.allowable.catch
total.allowable.catch <- function(x, limit.reference.point, upper.stock.reference, species){
   # Parse 'species' arguments:
   if (missing(species)) stop("'species' must be specified.")
   if (is.character(species)) species <- species(species)

   # Snow crab:
   if (species == 2526){
      # Define refrence points:
      r <- reference.points("snow crab")

      # Define biomass reference points and corresponding exploitation rate:
      biomass <- (1-r[["Flim"]]) * r[["Busr"]]
      biomass[2] <- 35929 # 2010 Commercial biomass.
      biomass[3] <- 1.1 * (0.5 * r[["Bmax"]] * 0.8)
      biomass[4] <- r[["Bmax"]]
      F <- c(0.2, 0.29, r[["Flim"]], 0.45)

      # Define harvest control rule:
      fun <- function(x){
         y <- NA * x
         y[x < biomass[1]] <- 0
         for (i in 2:length(biomass)){
            ix <- which((x >= biomass[i-1]) & (x < biomass[i]))
            y[ix] <- F[i-1] + (F[i] - F[i-1]) * ((x[ix] - biomass[i-1]) / (biomass[i] - biomass[i-1]))
         }
         y[x >= biomass[length(biomass)]] <- F[length(F)]

         return(y)
      }

      if (!missing(x)) return(fun(x)) else return(fun)
   }

   return(NULL)
}

#' @export TAC
TAC <- total.allowable.catch
