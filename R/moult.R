#' Carapace Moult Status
#' 
#' @description Determines the carapace moult or hardness status.
#' 
#' @param x Object.
#' 
#' @return Logical vector.
#' 
#' @examples
#' # Read 2010 crab data:
#' x <- read.scsbio(2010)
#' index <- is.new.shell(x)    # Newly moulted crab.
#' index <- is.multiparous(x)  # Multiparous females.
#' index <- is.skip.moulter(x) # Skip-moulters.
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{is.new.shell}}{Returns whether a crustacean is newly moulted.}
#'   \item{\code{is.new.shell.scsbio}}{Returns whether a crab is newly moulted for snow crab survey data.}
#'   \item{\code{is.hard.shell}}{Returns whether a carapace is considered hard.}
#'   \item{\code{is.hard.shell.scsbio}}{Returns whether a carapace is considered hard for snow crab survey data.}
#'   \item{\code{is.hard.shell.scsobs}}{Returns whether a carapace is considered hard for snow crab observer data.}
#'   \item{\code{is.soft.shell}}{Returns whether a carapace is considered soft.}
#'   \item{\code{is.skip.moulter}}{Returns whether a crustacean is a skip-moulter.}
#'   \item{\code{is.skip.moulter.scsbio}}{Returns whether a snow crab is a skip-moulter.}
#' }
#' 
#' @export is.new.shell
#' @export is.new.shell.scsbio
#' @export is.hard.shell
#' @export is.hard.shell.scsobs
#' @export is.hard.shell.scsbio
#' @export is.soft.shell
#' @export is.skip.moulter 
#' @export is.skip.moulter.scsbio

#' @rdname moult
is.new.shell <- function(x, ...) UseMethod("is.new.shell")

#' @rdname moult
is.new.shell.scsbio <- function(x){
   index <- rep(FALSE, dim(x)[1])
   names(x) <- tolower(names(x))
   index[(x$year <= 1991) & (x$shell.condition == 1)] <- TRUE
   index[(x$year > 1991) & (x$shell.condition %in% 1:2)] <- TRUE

   return(index)
}

#' @rdname moult
is.hard.shell <- function(x, ...) UseMethod("is.hard.shell")

#' @rdname moult
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

#' @rdname moult
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

#' @rdname moult
is.soft.shell <- function(x) return(!is.hard.shell(x))

#' @rdname moult
is.skip.moulter <- function(x, ...) UseMethod("is.skip.moulter")

#' @rdname moult
is.skip.moulter.scsbio <- function(x, ...){
   # IS.SKIP.MOULTER - Returns whether a crab a skip moulter.

   # Contruct logical vextor:
   index <- rep(TRUE, dim(x)[1])
   index <- index & is.mature(x, ...) * !is.new(x)
      
   # Convert to logical if there are no fractions:
   if (all((index[!is.na(index)] %% 1) == 0)) index <- (index == 1)
 
   return(index)
}
