#' Missing Legs
#' 
#' @description Functions for formatting and checking whether a crustacean has missing legs.
#' 
#' @param x Numerical code(s), character string(s) or \code{list} objects.
#' 
#' @return Character string(s) of logical values.
#' 
#' @examples
#' # Return all missing legs code descriptions:
#' missing.legs()
#'    
#' # Missing legs descriptions for specified codes:
#' missing.legs(c(1, 1, 2, 3)) 
#' 
#' # A sample of test cases:
#' str <- c("11 L234 R34R", "15 R2M L3R L4M L5", "  ", "", "56", "r3",
#'          "15 L2,3,5,R2,4", "That's R2 right", "R234  L2,3r", "l3r", " 34 what",
#'          "L1,3,2R", "This a comment", "R234r5r, l12r3, 49")
#' 
#' # Convert to standard format:
#' missing.legs(str)
#' 
#' # Read 2010 crab data:
#' x <- read.scsbio(year = 2010)
#'    
#' # Remove specimens with missing legs:
#' x <- x[is.missing.legs(x), ]
#' 
#' @section Standard missing legs format:
#' 
#' The standard missing leg format is a 10-character string with the proper missing legs codes.
#' The first five characters correspond to the left-hand side pereiopds, numbered starting with the chela. 
#' The last 5 characters correspond to those of the right-hand side. 
#' 
#' For example \sQuote{**2***1***} indicates that the 3rd leg on the left-hand side is regenerated,
#' while the 2nd leg on the right-hand side is missing.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{missing.legs}}{Generic \code{missing.legs} method.}
#'   \item{\code{missing.legs.default}}{Default \code{missing.legs} method.}
#'   
#'   \item{\code{missing.legs.numeric}}{Returns a missing legs description for a specified missing legs code. 
#'                                      If no \code{code} is given, then a named character vector of all missing 
#'                                      legs names for a given survey is returned. Unknown codes and NA values are 
#'                                      returned as NA values.}
#'   
#'   \item{\code{missing.legs.character}}{Function to convert non-standard character string input to the standard missing 
#'                                        legs format. The function identifies certain character string fragments as relevant
#'                                        for missing legs codes. For example, \sQuote{"R2M L3R"} specifies that the 2nd
#'                                        leg on the right-hand side is missing while the 3rd leg on the left-hand
#'                                        side is regenerated. This string translates to \sQuote{**2***1***} in the
#'                                        standard format. It is also possible to use a more compact coding available such as
#'                                        \sQuote{"R23 L12R3R"}, which specifies that the 2nd and 3rd legs on the
#'                                        right-hand side is missing, and the first leg on the left-hand side is
#'                                        missing, while the 2nd and 3rd ones are regenerated. This string translates
#'                                        to \sQuote{122***11**} in the standard format.}
#'                                        
#'   \item{\code{is.missing.legs}}{Generic \code{is.missing.legs} function.}
#'   
#'   \item{\code{is.missing.legs.character}}{Returns whether a crustacean has missing legs.}
#'   
#'   \item{\code{is.missing.legs.list}}{Returns whether a crustacean has missing legs for an given object containing missing
#'                                      leg data observations.}
#' }
#' 

#' @rdname missing.legs
missing.legs <- function(x, ...) UseMethod("missing.legs")

#' @rdname missing.legs
missing.legs.default <- function(x){
   if (missing(x)){ 
      v <- missing.legs.numeric()
      names(v) <- 1:length(v)
      return(v)
   }
}

#' @rdname missing.legs
missing.legs.numeric <- function(x){
   # Define code descriptions:
   descriptions <- c("Missing (M)",
                     "Regenerated (R)",
                     "Peu (A)",
                     "Moyen (B)",
                     "Beaucoup (C)",
                     "Bacterie inconnue (virus) (O)")

   if (missing(x)) return(descriptions) else return(descriptions[x])
}

#' @rdname missing.legs
missing.legs.character <- function(x){
   x <- toupper(x)

   # Replace commas with spaces:
   x <- gsub(",", " ", x, fixed = TRUE)
   x <- gsub(".", " ", x, fixed = TRUE)
   x[gsub(" ", "", x) == ""] <- " "

   # Split strings using spaces:
   y <- strsplit(x, " ")
     
   # Remove blank entries:
   y <- lapply(y, function(x) x <- x[x != ""])
   
   # Function to isolate missing or regenerating legs:
   f <- function(x){
      # Remove leading strings that do not contain 'R' or 'L' as leading characters:
      x <- x[grep("^[RL]", x)]
      x <- gsub("RR", "R", x)
      if (length(x) > 1){
         chr <- substr(x[1], 1, 1)
         for (i in 2:length(x)){
            if (substr(x[i], 1, 1) %in% as.character(1:5)) x[i] <- paste(chr, x[i], sep = "")
            if (substr(x[i], 1, 1) != chr){
               if (substr(x[i], 1, 1) %in% c("R", "L"))
                  chr <- substr(x[i], 1, 1)
               else x[i] <- ""
            }
         }
      }
      return(x)
   }

   # Isolate missing leg strings:
   y <- lapply(y, f)

   # Function to convert long-form to condensed format:
   g <- function(x){
      left <- right <- ""
      # Right-hand side:
      index <- which((substr(x, 1, 1) == "R") & (substr(x, 2, 2) %in% as.character(1:5)))
      if (length(index) >= 1){
         for (i in 1:length(index)){
            right <- paste(right, substr(x[index[i]], 2, nchar(x[index[i]])), sep = "")
         }
         right <- paste("R", right, sep = "")
      }
      # Left-hand side:
      index <- which((substr(x, 1, 1) == "L") & (substr(x, 2, 2) %in% as.character(1:5)))
      if (length(index) >= 1){
         for (i in 1:length(index)){
            left <- paste(left, substr(x[index[i]], 2, nchar(x[index[i]])), sep = "")
         }
         left <- gsub("L", "", left)
         left <- paste("L", left, sep = "")
      }

      return(paste(left, right))
   }

   # Convert to condensed format:
   y <- unlist(lapply(y, g))
   y <- gsub("[A-K,M-Q,S-Z]", "", y)
   y <- strsplit(y, " ")
   y <- lapply(y, function(x) x <- x[x != ""])
   
   h <- function(x){
      m <- rep("*", 10)
      if (length(x) == 0) return(paste(m, collapse = ""))
      for (i in 1:length(x)){
      	 if (substr(x[i], 1, 1) == "R") offset <- 5 else offset <- 0
      	 str <- substr(x[i], 2, nchar(x[i]))
      	 while (nchar(str) > 0){
      	 	temp <- as.numeric(substr(str, 1, 1))
      	 	if (substr(str, 2, 2) == "R"){
      	       m[temp + offset] <- 2
      	       str <- substr(str, 3, nchar(str))
      	    }else{
      	       m[temp + offset] <- 1
      	       str <- substr(str, 2, nchar(str))
      	    }
      	 }
      }
      m <- paste(m, collapse = "")

      return(m)
   }

   res <- unlist(lapply(y, h))

   return(res)
}

#' @rdname missing.legs
is.missing.legs <- function(x, ...) UseMethod("is.missing.legs")

#' @rdname missing.legs
is.missing.legs.character <- function(x, side, include.regenerated = FALSE, ...){
   # Convert to data frame:
   x <- data.frame(missing.legs = x)

   # Parse 'side' argument:
   if (!missing(side)){
      side <- unique(side)
      if (length(side) > 2) stop("'side' must be of length two or less.")
      if (is.numeric(side)) if (!all(side %in% c(1,2))) stop("'side' must be 1 or 2.")
      if (is.character(side)) side <- pmatch(tolower(side), c("left", "right"))
   }
                 
   # Contruct logical vector:
   index <- rep(FALSE, dim(x)[1])
   names(x) <- tolower(names(x))
   if ("missing.legs" %in% names(x)){
      if (!missing(side)){
         if (is.numeric(side)){
            if (all(sort(side) == c(1,2))){
               i1 <- index; i2 <- index;
               i1[unique(grep("1", substr(x$missing.legs, 1, 5)))] <- TRUE
               if (include.regenerated) i1[unique(grep("2", substr(x$missing.legs, 1, 5)))] <- TRUE
               i2[unique(grep("1", substr(x$missing.legs, 6, 10)))] <- TRUE
               if (include.regenerated) i2[unique(grep("2", substr(x$missing.legs, 6, 10)))] <- TRUE 
               index <- i1 & i2  
            }else{
               index[unique(grep("1", substr(x$missing.legs, 5*(side-1) + 1, 5*(side-1) + 5)))] <- TRUE
               if (include.regenerated) index[unique(grep("2", substr(x$missing.legs, 5*(side-1) + 1, 5*(side-1) + 5)))] <- TRUE
            }
         } 
      }else{
         index[unique(grep("1", x$missing.legs))] <- TRUE
         if (include.regenerated) index[unique(grep("2", x$missing.legs))] <- TRUE      
      }
   }else{
      stop("Object has no 'missing.legs' field.")
   }

   return(index)
}

#' @rdname missing.legs
is.missing.legs.data.frame <- function(x, side, include.regenerated = FALSE, ...){
   names(x) <- tolower(names(x))
   if (!("missing.legs" %in% names(x))) stop("No missing legs field in target object.")
      
   return(is.missing.legs(x$missing.legs)) 
}

#' @rdname missing.legs
bin2str.missing.legs <- function(x){
   # BIN2STR.MISSING.LEGS - Convert missing legs binary string to character string format.
   
   # x <- c("10100", "11111", "00000", "01010", "11100", "11001", "", NA, "10110", "**12*")
   
   x[is.na(x)] <- "     "
     
   # Normalize input data:
   if (any(nchar(x) > 10)) stop("Some missing leg strings have length longer than 10.")
   
   x <- gsub("*", "0", x, fixed = TRUE)
   x <- gsub(" ", "0", x, fixed = TRUE)
   
   
   # Buffer strings with irregular lengths:
   index <- which(nchar(x) < 5)
   while (length(index) > 0){
      x[index] <- paste0(x[index], "0")
      index <- which(nchar(x) < 5)
   }
   if (!all(nchar(x) == 5)){
      index <- which(nchar(x) < 10)
      while (length(index) > 0){
         x[index] <- paste0(x[index], "0")
         index <- which(nchar(x) < 10)
      }
      v <- paste0("L", bin2str.missing.legs(substr(x, 1, 5)), " ", "R", bin2str.missing.legs(substr(x, 6, 10)))   
      v <- gsub("^L ", "", v)
      v <- gsub("R$", "", v)
   }else{
      v <- rep("", length(x))
      for (i in 1:5){
         index <- which((substr(x, i, i) == 1))
         v[index] <- paste0(v[index], i)
      }   
   }
 
   return(v) 
}
