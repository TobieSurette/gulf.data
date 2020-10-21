#' Carapace Moult Status
#' 
#' @name moulting
#' @aliases molting
#' 
#' @description Determines the carapace moult or hardness status.
#' 
#' @param x Object.
#' 
#' @return Logical vector.
#' 
#' @examples
#' # Read 2020 crab data:
#' x <- read.scsbio(2020)
#' index <- is.new.shell(x)    # Newly moulted crab.
#' index <- is.soft.shell(x)   # Soft-shelled crab.
#' index <- is.skip.moulter(x) # Skip-moulters.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{is.new.shell}}{Generic \code{is.new.shell} method.}
#'   \item{\code{is.new.shell.scsbio}}{Returns whether a crab is newly-moulted based on snow crab survey data.}
#'   \item{\code{is.hard.shell}}{Generic \code{is.hard.shell} method.}
#'   \item{\code{is.hard.shell.scsbio}}{Returns whether a carapace is considered hard based on snow crab survey data.}
#'   \item{\code{is.hard.shell.scsobs}}{Returns whether a carapace is considered hard based on snow crab observer data.}
#'   \item{\code{is.soft.shell}}{Returns whether a crustacean carapace is considered soft.}
#'   \item{\code{is.skip.moulter}}{Generic \code{is.skip.moulter} method.}
#'   \item{\code{is.skip.moulter.scsbio}}{Returns whether a crustacean is considered to have skipped a moult, based on snow crab survey data.}
#' } 
#' 

#' @export is.new.shell
is.new.shell <- function(x, ...) UseMethod("is.new.shell")

#' @rdname moulting
#' @rawNamespace S3method(is.new.shell,scsbio)
is.new.shell.scsbio <- function(x){
   index <- rep(FALSE, dim(x)[1])
   index[(gulf.utils::year(x) <= 1991) & (x$shell.condition == 1)] <- TRUE
   index[(gulf.utils::year(x) > 1991) & (x$shell.condition %in% 1:2)] <- TRUE

   return(index)
}

#' @rdname moulting
#' @export is.hard.shell
is.hard.shell <- function(x, ...) UseMethod("is.hard.shell")

#' @rdname moulting
#' @rawNamespace S3method(is.hard.shell,scsbio)
is.hard.shell.scsbio <- function(x, durometer, shell.condition, zone, ...){
   # Parse input arguments:
   if (is.data.frame(x)) names(x) <- tolower(names(x))
   if (missing(durometer) & ("durometer" %in% names(x))) durometer <- x$durometer
   if (missing(shell.condition) & ("shell.condition" %in% names(x))) shell.condition <- x$shell.condition
   if (missing(zone) & ("zone" %in% names(x))) zone <- x$zone
   
   # Expand missing input arguments:
   if (missing(durometer) & missing(shell.condition)) stop("'durometer' or 'shell.condition' must be specified.")
   if (missing(durometer)) durometer <- rep(NA, length(shell.condition))
   if (missing(shell.condition)) shell.condition <- rep(NA, length(durometer))
   if (missing(zone)) zone <- rep(NA, length(durometer))
   durometer <- as.numeric(durometer)
   
   # Standardize 'shell.condition' variable: 
   shell.condition <- as.character(shell.condition)
   mossy <- toupper(substr(shell.condition, 2, 2))
   mossy[mossy != "M"] <- ""
   shell.condition <- substr(shell.condition, 1, 1)
   shell.condition[!(shell.condition %in% as.character(1:5))] <- NA
   shell.condition <- as.numeric(shell.condition)
   shell.condition <- paste(as.character(shell.condition), mossy) 
   shell.condition <- gsub(" ", "", shell.condition)
   shell.condition <- gsub("NA", "", shell.condition)
  
   # Standardize 'zone' variable:
   if (length(zone) == 1) zone <- rep(zone, length(durometer))
   zone <- toupper(as.character(zone))
   zone <- gsub(" ", "", zone)
   zone[is.na(zone)] <- ""
   
   # Determine softness criteria:
   v <- rep(FALSE, length(durometer))
   v[which(shell.condition %in% c("3M", "4", "4M",  "5", "5M"))] <- TRUE   
   
   # Zone 12, 12E and 12F:
   v[which((durometer < 68) & (zone != "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- FALSE
   v[which((durometer >= 68) & (zone != "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- TRUE
   
   # Zone 19:
   v[which((durometer < 72) & (zone == "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- FALSE
   v[which((durometer >= 72) & (zone == "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- TRUE
      
   return(v)
}

#' @rdname moulting
#' @rawNamespace S3method(is.hard.shell,scsobs)
is.hard.shell.scsobs <- function(x, durometer, shell.condition, zone, ...){
   # Parse input arguments:
   if (is.data.frame(x)) names(x) <- tolower(names(x))
   if (missing(durometer) & ("durometer" %in% names(x))) durometer <- x$durometer
   if (missing(shell.condition) & ("shell.condition" %in% names(x))) shell.condition <- x$shell.condition
   if (missing(zone) & ("zone" %in% names(x))) zone <- x$zone
   
   # Expand missing input arguments:
   if (missing(durometer) & missing(shell.condition)) stop("'durometer' or 'shell.condition' must be specified.")
   if (missing(durometer)) durometer <- rep(NA, length(shell.condition))
   if (missing(shell.condition)) shell.condition <- rep(NA, length(durometer))
   if (missing(zone)) zone <- rep(NA, length(durometer))
   durometer <- as.numeric(durometer)
   
   # Standardize 'shell.condition' variable: 
   shell.condition <- as.character(shell.condition)
   mossy <- toupper(substr(shell.condition, 2, 2))
   mossy[mossy != "M"] <- ""
   shell.condition <- substr(shell.condition, 1, 1)
   shell.condition[!(shell.condition %in% as.character(1:5))] <- NA
   shell.condition <- as.numeric(shell.condition)
   shell.condition <- paste(as.character(shell.condition), mossy) 
   shell.condition <- gsub(" ", "", shell.condition)
   shell.condition <- gsub("NA", "", shell.condition)
  
   # Standardize 'zone' variable:
   if (length(zone) == 1) zone <- rep(zone, length(durometer))
   zone <- toupper(as.character(zone))
   zone <- gsub(" ", "", zone)
   zone[is.na(zone)] <- ""
   
   # Determine softness criteria:
   v <- rep(NA, length(durometer))
   
   # No durometer reading:
   v[which(is.na(durometer) &  shell.condition %in% c("1", "1M", "2", "2M"))] <- FALSE
   v[which(is.na(durometer) &  shell.condition %in% c("3", "3M", "5", "5M"))] <- NA
   v[which(is.na(durometer) &  shell.condition %in% c("4", "4M"))] <- TRUE
   
   # Zone 12, 12E and 12F:
   v[which((durometer < 68) & (zone != "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- FALSE
   v[which((durometer < 68) & (zone != "19") & (shell.condition %in% c("3M", "4", "4M")))] <- TRUE
   v[which((durometer >= 68) & (zone != "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3", "3M", "4", "4M")))] <- TRUE
   
   # Zone 19:
   v[which((durometer < 72) & (zone == "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3")))] <- FALSE
   v[which((durometer < 72) & (zone == "19") & (shell.condition %in% c("3M", "4", "4M")))] <- TRUE
   v[which((durometer >= 72) & (zone == "19") & (shell.condition %in% c("1", "1M", "2", "2M", "3", "3M", "4", "4M")))] <- TRUE
      
   # Coded as soft by the observer:
   v[which(durometer == 0)] <- FALSE
   
   return(v)
}

#' @rdname moulting
#' @export is.soft.shell
is.soft.shell <- function(x, ...) return(!is.hard.shell(x, ...))

#' @rdname moulting
#' @export is.skip.moulter
is.skip.moulter <- function(x, ...) UseMethod("is.skip.moulter")

#' @rdname moulting
#' @rawNamespace S3method(is.skip.moulter,scsbio)
is.skip.moulter.scsbio <- function(x, ...){
   # IS.SKIP.MOULTER - Returns whether a crab a skip moulter.

   # Contruct logical vextor:
   index <- rep(TRUE, dim(x)[1])
   index <- index & is.mature(x, ...) * !is.new.shell(x)
      
   # Convert to logical if there are no fractions:
   if (all((index[!is.na(index)] %% 1) == 0)) index <- (index == 1)
 
   return(index)
}
