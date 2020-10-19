#' Frequency Functions
#' 
#' @description Functions to generate size-frequency distributions.
#' 
#' @param x Vector of numeric values.
#' @param n Vector of numeric frequency values for each \code{x}.
#' @param by Grouping variable.
#' @param index Numeric or logical indices specifying which elements of \code{x} to be analyzed as a subset. 
#' @param fill Logical value specifying whether to interpolate missing frequency bins.
#' @param step Numeric value specifying the width of the frequency bins.
#' @param range Two-element numeric vector specifying the range of values to be included in 
#'              the resulting frequency table. Use NA or +/- Inf to specify lower or upper open bounds.
#' @param category Character string specifying biological categories. See \code{\link{category}} for details.
#' @param variable Character string specifying the variable used to generate the frequency table. 
#' 
#' @examples 
#' # Simple example:
#' x <- data.frame(values = c(11, 11, 12, 13, 15),
#'                 n      = c(2,1,3,1,1),
#'                 group  = c(1,1,1,2,2),
#'                 sub    = c(1,2,2,3,3))
#'                 
#' freq(x$values, x$n)  # Frequency table.
#' freq(x$values, x$n, by = x["group"])  # Frequency table by group.
#' freq(x$values, x$n, by = x[c("group", "sub")])  # Frequency table by group and subgroup.
#'   
#' # Load snow crab biological data:       
#' x <- read.scsbio(2020)                        
#'
#' # Default frequency function examples:
#' freq(x$carapace.width, step = 1) # Size-frequencies using millimeter bins.
#' freq(x$carapace.width, step = 0.5) # Size-frequencies using half-millimeter bins.
#' freq(x$carapace.width, by = x["sex"], step = 0.5) # Size-frequencies by sex.
#' freq(x$carapace.width, by = x[c("sex", "shell.condition")], step = 0.5) # Size-frequencies by sex and shell condition.
#' freq(x$carapace.width, index = is.category(x, c("MM", "FM")), step = 1) # Size-frequencies for mature male and mature females.
#' 
#' # 'scsbio' frequency function examples:
#' freq(x) # Size-frequencies.
#' freq(x, by = "sex") # Size-frequencies by sex.
#' freq(x, by = c("sex", "shell.condition")) # Size-frequencies by sex and shell condition.
#' freq(x, category = c("MM", "FM")) # Size-frequencies for mature male and mature females.

#' @export freq
freq <- function(x, ...) UseMethod("freq")

#' @describeIn freq Default \code{freq} method.
#' @export
freq.default <- function(x, n, index, by, fill = TRUE, step, range, ...){
   # FREQ.DEFAULT - Build frequency table.

   # Parse 'x' argument:
   if (!is.null(dim(x))) stop("'x' must be a vector.") 

   # Check 'n' argument:
   if (!missing(n)){
      if (!is.null(dim(n))) stop("'n' must be a vector.") 
      if (length(x) != length(n)) stop("'x' and 'n' must have the same number of elements.")
   }else{
      # Treat 'x' as a frequency table vector:
      if (!is.null(names(x))){
         if (all(gsub("[-0-9.]", "", names(x)) == "")){
            n <- as.numeric(x)
            x <- as.numeric(names(x))
         } 
      }else{
         n <- rep(1, length(x))
      }
   } 

   # Round off frequency values:
   if (!missing(step)) x <- round(x / step) * step
   
   # # Use grouping variables to parse dataset:
   if (!missing(by)){
      if (is.vector(by)) by <- data.frame(group = by)
      
      if (nrow(by) != length(x)) stop("'by' must have the same number of rows as elements in 'x'")
      groups <- unique(by)

      # Calculate frequencies by grouping variables:
      f <- list()
      for (i in 1:nrow(groups)){
         ii <- rep(TRUE, length(n))  
         for (j in 1:ncol(groups)) ii <- ii & (by[,j] == groups[i,j])
         f[[i]] <- freq(x[ii], n[ii], fill = FALSE, ...)
      }
      
      # Square-off results into matrix form:
      values <- sort(as.numeric(unique(unlist(lapply(f, names)))))
      fnew <- matrix(0, nrow = nrow(groups), ncol = length(values))
      colnames(fnew) <- values

      for (i in 1:nrow(fnew)) fnew[i, names(f[[i]])] <- as.numeric(f[[i]])
      fnew <- as.data.frame(fnew)

      # Combine groups and frequency matrix:
      f <- cbind(groups, fnew)
   }else{
      if missing(index)){
         # Convert to frequencies:
         r <- stats::aggregate(list(n = n), by = list(x = x), sum)
         f <- r$n 
         names(f) <- r$x      
      }else{
         if (is.vector(index)) index <- t(t(index))
         index <- as.matrix(index)
         
         # Convert indices to logical values:
         if (!is.logical(index)){
            tmp <- NULL
            for (i in 1:ncol(index)) tmp <- cbind(tmp, 1:length(x) %in% index[,i])
            colnames(tmp) <- colnames(index)
            index <- tmp
         }
         
         # Calculate frequencies by grouping variables:
         f <- list()
         for (i in 1:ncol(index)) f[[i]] <- freq(x[which(index[,i])], n[which(index[,i])], fill = FALSE, ...)
         
         # Square-off results into matrix form:
         values <- sort(as.numeric(unique(unlist(lapply(f, names)))))
         fnew <- matrix(0, nrow = ncol(index), ncol = length(values))
         colnames(fnew) <- values
         for (i in 1:nrow(fnew)) fnew[i, names(f[[i]])] <- as.numeric(f[[i]])
         fnew <- as.data.frame(fnew)
         f <- fnew
         
         # Combine groups and frequency matrix:
         if (!is.null(colnames(index))) f <- cbind(data.frame(category = colnames(index)), f) 
      }
   }
   
   # Fill-in missing regular values with zeroes:
   if (fill){
      fvars <- names(f)[gsub("[-0-9.]", "", names(f)) == ""]
      values <- sort(as.numeric(fvars))
      if (missing(step)) step <- min(diff(values))
      varnew <- as.character(seq(min(values), max(values), by = step))
      f[setdiff(varnew, fvars)] <- 0
      
      f <- f[c(setdiff(names(f), varnew), varnew)] 
   }     
   
   # Impose range constraints:
   if (!missing(range)){
      if (!is.numeric(range) | length(range) != 2) 
         stop("'range' must be a two element vector. Use NA or +/- Inf to specify open bounds.")
      if (missing(by)) fvars <- names(f) else fvars <- setdiff(names(f), names(by))
      remove <- fvars[which((as.numeric(fvars) < range[1]) | (as.numeric(fvars) > range[2]))]
      f <- f[setdiff(names(f), remove)]
   }
   
   rownames(f) <- NULL
   
   return(f)
}

#' @describeIn freq \code{scsbio} \code{freq} method.
#' @export
freq.scsbio <- function(x, category, by, step = 1, variable = "carapace.width", ...){
   # Remove NA values from data set:
   var <- x[, variable]
   x <- x[!is.na(var), ]
   var <- x[, variable]
   
   # Parse 'by' argument:
   if (!missing(by))
      if (is.character(by)) 
         if (!all(by %in% names(x))) stop("Some 'by' variables not variables in 'x'.") else by <- x[by]

   if (missing(category) & missing(by))   f <- freq(var, step = step, ...)
   if (missing(category) & !missing(by))  f <- freq(var, by = by, step = step, ...)
   if (!missing(category) & missing(by))  f <- freq(var, index = is.category(x, category = category, drop = FALSE), step = step, ...)
   if (!missing(category) & !missing(by)) f <- freq(var, by = by, index = is.category(x, category = category, drop = FALSE), step = step, ...)
   
   return(f)
}

