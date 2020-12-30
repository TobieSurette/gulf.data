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
#' freq(x$carapace.width, index = is.category(x, category()), step = 1)    # All default categories.
#' 
#' # 'scsbio' frequency function examples:
#' freq(x) # Size-frequencies.
#' freq(x, by = "sex") # Size-frequencies by sex.
#' freq(x, by = c("sex", "shell.condition")) # Size-frequencies by sex and shell condition.
#' freq(x, category = c("MM", "FM")) # Size-frequencies for mature male and mature females.
#' freq(x, by = c("date" "tow.id"), category = c("MM", "FM")) # Size-frequencies for mature male and mature females.
#' 
#' @export freq
freq <- function(x, ...) UseMethod("freq")

#' @describeIn freq Default \code{freq} method.
#' @export
freq.default <- function(x, n, index, by, fill = TRUE, step, range, ...){
   # FREQ.DEFAULT - Build frequency table.

   if (length(x) == 0) return(NULL)
   
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
   
   if (missing(index) & missing(by)){
      # Convert to frequencies:
      r <- stats::aggregate(list(n = n), by = list(x = x), sum)
      f <- r$n 
      names(f) <- r$x  
   }
   
   # Convert 'by' to data.frame:
   if (!missing(by)) if (is.vector(by)) by <- data.frame(group = by)
   
   # # Use grouping variables to parse dataset:
   if (!missing(by) & missing(index)){
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
   }
   
   if (!missing(index)){ 
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
      for (i in 1:ncol(index)){
         j <- which(index[,i])
         if (length(j) == 0){
            ux <- sort(unique(x))
            f[[i]] <- rep(0, length(ux))
            names(f[[i]]) <- ux
         }else{
            if (missing(by)){
               f[[i]] <- freq(x[j], n[j], fill = FALSE, ...)
            }else{
               f[[i]] <- freq(x[j], n[j], by = by[j, ], fill = FALSE, ...)
            }
         }
      }
      
      # Square-off results into matrix form:
      fvars <- unique(unlist(lapply(f, names)))
      fvars <- fvars[gsub("[-0-9.]", "", fvars)  == ""]
      fvars <- fvars[order(as.numeric(fvars))]
      
      fnew <- NULL
      for (i in 1:length(f)){
         tmp <- f[[i]]
         if (is.vector(tmp)) tmp <- t(as.matrix(tmp))
         if (!is.data.frame(tmp)) tmp <- as.data.frame(tmp)
         rownames(tmp) <- NULL
         tmp[setdiff(fvars, names(tmp))] <- 0
         tmp <- tmp[c(setdiff(names(tmp), fvars), fvars)]
         if (!is.null(colnames(index))){
            tmp$category <- colnames(index)[i]
            tmp <- tmp[, c(ncol(tmp), 1:(ncol(tmp)-1)), drop = FALSE]
         }
         fnew <- rbind(fnew, tmp)
      }
      f <- fnew
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

   # Process all different combinations:
   if (missing(category) & missing(by))   f <- freq(var, step = step, ...)
   if (missing(category) & !missing(by))  f <- freq(var, by = by, step = step, ...)
   if (!missing(category) & missing(by))  f <- freq(var, index = is.category(x, category = category, drop = FALSE), step = step, ...)
   if (!missing(category) & !missing(by)) f <- freq(var, by = by, index = is.category(x, category = category, drop = FALSE), step = step, ...)
   
   return(f)
}

#' @describeIn freq \code{scslen} \code{freq} method.
#' @export
freq.scslen <- function(x, variable = "length", by, units = "cm", ...){
   # Remove NA values from data set:
   x <- x[!is.na(x[, variable]), ]

   # Unit conversions:
   if (units == "cm"){
      ix <- which(x$length.unit == "mm")
      x[ix, variable] <- x[ix, variable] / 10
   }
   if (units == "mm"){
      ix <- which(x$length.unit == "cm")
      x[ix, variable] <- x[ix, variable] * 10      
   }
   
   # Parse 'by' argument:
   if (!missing(by)){
      if (is.character(by)) if (!all(by %in% names(x))) stop("Some 'by' variables not variables in 'x'.") else by <- x[by]
   }
   
   # Process all different combinations:
   if (missing(by)) f <- freq(x[, variable], ...)  else f <- freq(x[, variable], by = by, ...)
   
   return(f)
}

#' @describeIn freq \code{nsslen} \code{freq} method.
#' @export
freq.nsslen <- function(x, variable = "length", by, units = "cm", ...){
   # Remove NA values from data set:
   x <- x[!is.na(x[, variable]), ]
   
   # Unit conversions:
   if (units == "cm"){
      ix <- which(x$length.unit == "mm")
      x[ix, variable] <- x[ix, variable] / 10
   }
   if (units == "mm"){
      ix <- which(x$length.unit == "cm")
      x[ix, variable] <- x[ix, variable] * 10      
   }
   
   # Parse 'by' argument:
   if (!missing(by)){
      if (is.character(by)) if (!all(by %in% names(x))) stop("Some 'by' variables not variables in 'x'.") else by <- x[by]
   }
   
   # Process all different combinations:
   if (missing(by)) f <- freq(x[, variable], ...)  else f <- freq(x[, variable], by = by, ...)
   
   return(f)
}


