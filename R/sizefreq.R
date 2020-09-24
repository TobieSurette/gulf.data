#' Size-Frequency Functions
#' 
#' @description Functions to generate size-frequency distributions.
#' 
#' 


#' @export
freq.default <- function(x, n, by, fill = TRUE, ...){
   # FREQ.DEFAULT - Build frequency table.

   # Parse 'x' argument:
   if (is.data.frame(x) | is.matrix(x)) if (ncol(x) != 1) stop("'x' must be a vector.") else x <- x[, 1]

   # Check 'n' argument:
   if (!missing(n)) if (is.data.frame(n) | is.matrix(n)) if (ncol(n) != 1) stop("'n' must be a vector.") n <- n[, 1] 

   # Convert vector 'x' to frequencies:
   if (missing(n) & missing(by)) f <- table(x)

   # Convert 'x' and 'n' to frequencies:
   if (missing(by) & !missing(n)){
      if (length(x) != length(n)) stop("'x' and 'n' must have the same number of elements.")
      t <- stats::aggregate(n, by = list(x = x), sum)
      f <- t[, 2]
      names(f) <- t[, 1]
   }

   # Fill-in missing regular values with zeroes:
   if (fill){
      step <- min(diff(sort(as.numeric(names(f)))))
      vnew <- seq(min(v), max(v), by = step)
      names(vnew) <- vnew
      vnew <- vnew * 0
      vnew[names(v)] <- v
      v <- vnew
   }
   
   # Recursive calls:
   if (!missing(by)){
      if (is.vector(by)) by <- data.frame(by)
      if (nrow(by) != length(x)) stop("'by' must have the same number of rows as elements in 'x'")
      if (missing(n)) n <- rep(1, length(x))
      tapply(by(data.frame)
   }  

   return(v)
}


sizefreq.scbio <- function(x, category = NULL, by = NULL, start = 0, bin = 1, sort = TRUE, ...){
   # FREQ.SCBIO - Size-frequency counts for a 'scbio' object.
   
   # Remove irrelevant data:
   x <- subset(x, x$carapace.width >= start, ...)
   
   if (is.null(category) & is.null(by)){
      # Check and remove NA carapace width measurements:
      index <- is.na(x$carapace.width)
      if (sum(index) > 0) cat(paste("There were ", sum(index), " crab with no carapace width measurement.\n", sep = ""))
      
      # Create binned carapace width column:
      x$cwbin <- round((x$carapace.width - start) / bin) * bin + start
      
      # Create frequency vector:
      f <- seq(start, max(x$cwbin, na.rm = TRUE), by = bin)
      names(f) <- f
      f <- 0 * f 
      r <- table(x$cwbin)
      f[names(r)] <- as.numeric(r)
   }else{
      # Create binned carapace width column:
      x$cwbin <- round((x$carapace.width - start) / bin) * bin + start
      f <- seq(start, max(x$cwbin, na.rm = TRUE), by = bin)
      names <- c(by, "category", as.character(f))
      f <- as.data.frame(matrix(0, ncol = length(names)))
      names(f) <- names
      if (is.null(category)) category <- "T"
      k <- 1
      for (i in 1:length(category)){
         y <- subset(x, index = !is.na(x$carapace.width), category = category[i])
         if (is.null(by)) temp <- list(y) else temp <- by(y, y[by], FUN = function(x) return(x))
         for (j in 1:length(temp)){
            if (is.null(temp[[j]])){
               f[k, setdiff(names(f), c(by, "category"))] <- 0
            }else{
               res <- freq(temp[[j]], start = start, bin = bin)
               f[k, setdiff(names(f), c(by, "category"))] <- 0
               f[k, names(res)] <- as.numeric(res)
               if (!is.null(by)){
                  f[k, by] <- unique(temp[[j]][by])
               }
               f[k, "category"] <- category[i]
               k <- k + 1
            }
         }
      }
   }
   
   # Sort results:
   if (sort){
      if (!is.null(by) & !is.null(category)) f <- sort(f, by = c(by, "category"))
      if (!is.null(by) & is.null(category))  f <- sort(f, by = by)
      if (is.null(by) & !is.null(category))  f <- sort(f, by = "category")
   }
   
   return(f)
}
