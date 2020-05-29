#' Individual or Catch Weight
#'
#' @description Returns an estimate of the weight of individual organisms for a specified length. 
#'              Estimated weights of sample subsets can also be calculated.
#'              
#'              This function makes use of a set of allometric length-weight coefficients to
#'              estimate the weight of an organism given its length. The species must be
#'              specified and the sex may be specified for dimorphic species.
#'
#'              If \code{year} is specified, then survey data is loaded and the length-weight 
#'              coefficients are calculated directly.
#'
#' @param x A numerical vector of organism lengths, or alternatively, a
#'          frequency table or a data frame of length-frequencies as produced by
#'          \code{\link[gulf]{freq}} or \code{\link[gulf]{lenfreq}}. The presence of
#'          \sQuote{species}, \sQuote{sex} or \sQuote{year} fields in the data frame
#'          will be passed onto the corresponding function arguments.
#'  
#' @param species Species code.
#' 
#' @param sex Numerical code specifying sex.
#' 
#' @param coefficients A two-element numerical vector specifying the a and b coefficients, 
#'                     respectively. The the \sQuote{a} coefficient is assumed to be on the 
#'                     log-10 scale and the units in grams.
#' 
#' @param units Units of the weight vector to be returned. It may be either in grams 
#'              (\code{units = "g"}) or kilograms (\code{units = "kg"}).
#'              
#' @param year Survey years to use as data when calculating the length-weight coefficients. 
#'             If left unspecified, a table of default values are used.
#'             
#' @param ... Arguments passed onto the \code{\link[gulf]{length.weight}}.
#' 
#' @param category A character string specifying a snow crab or crustacean category for syntax.
#' 
#' @param by Character string(s) specifying the fields by which to group the estimated weights.
#' 
#' @param probability Logical value specifying whether maturity values are to
#'                    be filled in with probabilties when morphometric values are unavailable. In
#'                    this case, the numbers returned may be non-integer values.
#' 
#' @return Returns a numerical vector the same size as \code{length} containing the expected weight 
#'         of an organism for a given length measurement.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{weight}}{Generic \code{weight} method.}
#'   \item{\code{weight.default}}{Returns the expected weight for a given organism length.}
#'   \item{\code{weight.scsbio}}{Returns expected weight for an \code{scsbio} object.}
#'   \item{\code{weight.scobs}}{Returns expected weight for a snow crab from observer data.}
#' }
#'       
#' @examples
#' # Weights for Atlantic cod:
#' weight(0:100, species = 10)
#'
#' # Weights for female white hake:
#' weight(0:100, species = 12, sex = 2)
#'
#' # Weights for female white hake based on 2010 September survey data:
#' weight(0:100, species = 12, sex = 2, year = 2010)
#'
#' # Weights for white hake based on pooled sexes and data from 2010-2013 September surveys:
#' weight(0:100, species = 12, sex = 2, year = 2010:2013)
#'
#' # Transform length-frequencies to weight-length:
#' x <- read.gulf(year = 2014, species = 40, card = "len")
#'
#' # Simple example:
#' f <- freq(x, scale = TRUE)  # Pooled length-frequencies.
#' weight(f, species = 40)
#'
#' # Length-frequencies by stratum and sex:
#' f <- freq(x, scale = TRUE, by = c("species", "stratum", "sex"))
#' weight(f)
#'
#' # Length-frequencies by stratum and sex, use RV 2014 length-eight coefficients:
#' f <- freq(x, scale = TRUE, by = c("species", "stratum", "sex", "year"))
#' weight(f)
#' 
#' # Load 2010 snow crab data:
#' x <- read.scsbio(year = 2012)
#'
#' # Calculate weight for each crab:
#' weight(x)
#'
#' # Calculate weights by tow:
#' weight(x, by = "tow.number")
#'
#' # Calculate total weights by day:
#' weight(x, by = c("year", "month", "day"), category = c("TM", "TMM", "TMSC12", "TMSC345"))
#'
#' @export weight
#' @export weight.default
#' @export weight.scsbio
#' @export weight.scobs
#' 

#' @rdname weight
weight <- function(x, ...) UseMethod("weight")

#' @rdname weight
weight.default <- function(x, species, sex, coefficients, units = "kg",  ...){
   # Parse 'units' argument:
   units <- match.arg(tolower(units), c("grams", "kg", "kilograms"))
   if (units == "kg") units <- "kilograms"

   # Parse 'species' argument:
   if (!("species" %in% tolower(names(x)))){
      if (missing(species) & missing(coefficients)) stop("'species' or 'coefficients' must be specified.")
      if (!missing(species) & !is.data.frame(x)){
         if (length(species) == 1) species <- rep(species, length(x))
         if (length(species) != length(x)) stop("'x' and 'species' have incompatible lengths.")
      }
   }

   # Parse 'sex' argument:
   if (!missing(sex) & !is.data.frame(x)){
      if (length(sex) == 1) sex <- rep(sex, length(x))
      if (length(sex) != length(x)) stop("'x' and 'sex' have incompatible lengths.")
   }

   # Input 'x' is a table or named vector of length-frequencies:
   if (is.numeric(x) & is.table(x) | (is.null(nrow(x)) & !is.null(names(x)))){
      # 'x' is a frequency vector:
      if (length(grep("^[0-9]+$", names(x))) == length(x)){
         f <- x
         x <- as.numeric(names(f))
         v <- f * weight(x, species, sex, coefficients, units, ...)
         names(v) <- names(f)
         return(v)
      }
   }

   # Input 'x' are length-frequencies in a data frame:
   if (is.data.frame(x)){
      # Extract frequency matrix:
      fvars <- names(x)[grep("^[ 0-9]+$", names(x))]
      vars <- setdiff(names(x), fvars)
      temp <- x[vars]
      names(x) <- tolower(names(x))
      # Check that frequency variables are numeric:
      if (length(fvars) > 0){
         flag <- TRUE
         for (i in 1:length(fvars)) flag <- flag & is.numeric(x[, fvars[i]])
         if (flag){
            f <- x[fvars]
            if ("sex" %in% names(x)) sex <- as.vector(repvec(x$sex, ncol = length(fvars)))
            if ("species" %in% names(x)) species <- as.vector(repvec(x$species, ncol = length(fvars)))
            x <- repvec(as.numeric(fvars), nrow = nrow(x))
            d <- dim(x)
            x <- as.vector(x)
            if (!("year" %in% names(list(...))) & ("year" %in% names(temp))){
               v <- weight(x, species, sex, coefficients, units, year = unique(temp$year), ...)
            }else{
               v <- weight(x, species, sex, coefficients, units, ...)
            }
            dim(v) <- d
            v <- f * v
            v <- cbind(temp, v)
            return(v)
         }
      }
   }

   # Loop over species:
   if (length(unique(species)) > 1){
       species.list <- unique(species)
       v <- rep(NA, length(x))
       for (i in 1:length(species.list)){
          index <- species == species.list[i]
          if (missing(sex)){
             v[index] <- weight.default(x[index], species = species[index], units = "g", ...)
          }else{
             v[index] <- weight.default(x[index], species = species[index], sex = sex[index], units = "g", ...)
          }
       }
   }else{
      # Fetch length-weight coefficients:
      if (!missing(coefficients)){
         if (is.data.frame(coefficients)){
            if (!all(c("a", "b") %in% names(coefficients)))
               stop("'a' and 'b' must be column names if the length-weight coefficients are specified as a data frame.")
            if (nrow(coefficients) == 1){
               coefficients <- c(coefficients$a, coefficients$b)
            }else{
               stop("'coefficients' must be a two-element numeric vector.")
            }
         }
         if (is.numeric(coefficients) & length(coefficients)){
            beta <- data.frame(a = coefficients[1], b = coefficients[2])
         }else{
            stop("'coefficients' must be a two-element numeric vector.")
         }
      }else{
         if (missing(sex)){
            beta <- length.weight(units = "g", log10 = TRUE, species = unique(species), ...)
         }else{
            by <- "sex"
            if ("year" %in% names(list(...))) by <- c("year", by)
            beta <- length.weight(units = "g", log10 = TRUE, species = unique(species), sex = unique(sex), by = by, ...)
         }
      }

      # Calculate weights:
      if (is.null(beta)) stop("Corresponding length-weight coefficients were not found.")
      if (nrow(beta) == 1){
         v <- (10^beta$a) * exp(beta$b * log(x))
      }else{
         # Match entries to corresponding length-weight coefficients:
         res <- data.frame(x = x, species = species)
         if (!missing(sex)) res$sex <- sex
         index <- match(res[setdiff(names(res), "x")], beta)
         v <- (10^beta$a[index]) * exp(beta$b[index] * log(x))
      }
   }

   # Convert weight to proper units:
   if (units == "kilograms") v <- v / 1000

   return(v)
}

#' @rdname weight
weight.scsbio <- function(x, category, by, hard.shelled, units = "g", ...){
   y <- x

   # Parse input arguments:
   units <- match.arg(tolower(units), c("g", "kg", "grams", "kilograms", "tons", "tonnes", "mt", "t"))
   if (units %in% c("tons", "tonnes", "mt")) units <- "t"
   if (units == "kilograms") units <- "kg"
   if (units == "grams") units <- "g"

   # Parse 'hard.shelled' argument:
   if (!missing(hard.shelled)){
      if (!is.logical(hard.shelled)) stop("'hard.shelled' must be a logical value.")
      if (hard.shelled) y$shell.condition <- 3
   }else{
      hard.shelled <- FALSE
   }

   if (is.null(category) & is.null(by)){
      # Initialize weight vector:
      w <- rep(0, dim(y)[1])

      # New adult:
      w <- w + is.category(y, "TMMSC12", ...) * exp(-9.399 + 3.315 * log(x$carapace.width))

      # New adolescent:
      w <- w + is.category(y, "TMISC12", ...) * exp(-10.154 + 3.453 * log(x$carapace.width))

      # Intermediate adult:
      w <- w + is.category(y, "TMMSC345", ...) * exp(-8.230136 + 3.098 * log(x$carapace.width))

      # Intermediate adolescent:
      w <- w + is.category(y, "TMISC345", ...) * exp(-7.512 + 2.899 * log(x$carapace.width))

      # Immature females:
      w <- w + is.category(y, "TFI", ...) * exp(-7.275 + 2.804 * log(x$carapace.width))

      # Mature females:
      w <- w + is.category(y, "TFM", ...) * exp(-7.162 + 2.816 * log(x$carapace.width))
   }else{
      # Define indicator vector of category membership:
      if (!is.null(category)){
         n <- is.category(x, category = category, ...) + 1 - 1
         n <- as.data.frame(n)
         names(n) <- category
      }else{
         n <- matrix(rep(1, dim(x)[1]))
         dimnames(n) <- list(NULL, "n")
         n <- as.data.frame(n)
      }
      w <- n * repvec(weight(x, ...), ncol = dim(n)[2])

      # Return indicator vector if 'by' is NULL:
      if (is.null(by)) return(w)

      # Sum within 'by' groupings:
      w <- stats::aggregate(w, by = x[by], sum, na.rm = TRUE)
   }

   # Weight unit conversion:
   if (units == "kg") w <- w / 1000
   if (units == "t")  w <- w / 1000000

   return(w)
}

#' @rdname weight
weight.scobs <- function(x, ...){
   # Buffer variables:
   if (!("chela.height" %in% names(x)))   x$chela.height <- x$chela.height.right
   if (!("gonad.colour" %in% names(x)))   x$gonad.colour <- NA
   if (!("egg.colour" %in% names(x)))     x$egg.colour <- NA
   if (!("eggs.remaining" %in% names(x))) x$eggs.remaining <- NA
   
   x$chela.height <- chela.height(x)
   
   w <- weight.scsbio(x, ...)
   
   return(w)
}

