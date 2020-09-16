#' Snow Crab Biological Data
#'
#' @name scsbio
#' 
#' @description The \code{scsbio} class is a container for Snow Crab Survey Biological data, i.e. information 
#'              about individual organisms sampled on the snow crab annual survey. 
#'              
#' @param x A \code{data.frame} object. When reading data, \code{x} may be a numeric 
#'          vector specifying the survey years to be loaded or an \code{\link{scsset}} object for which we want to
#'          read the corresponding biological data.
#'          
#' @param year Numeric vector specifying the survey years to be loaded.
#' 
#' @param format A \code{data.frame} specifying the ASCII printing format for an \code{scsbio} object.
#' 
#' @param key Character string(s) specifying which data columns form the index key.
#'            This key can then be used as a default for such operations as merging data.
#' 
#' @param path string specifying the path to read or write snow crab biological data.
#' 
#' @param Rfile Logical value specifying whether to write an \code{R} data format file when 
#'              updating snow crab biological data.
#'              
#' @param csv Logical value specifying whether to write a comma-separated (\code{csv}) format file 
#'              when updating snow crab biological data.
#'              
#' @param package Logical value specifying whether to write a comma-separated (\code{csv}) format
#'                to the \code{gulf.data} package clone.
#' 
#' @param by Character string(s) specifying which variables to group by when summararizing.
#' 
#' @param category Biological category string(s). See \code{\link{cagetory}} for more details.
#' 
#' @param weight Logical value specifying whether to return a summary by weights rather than counts.
#'                         
#' @param ... Other parameters (not used).
#' 
#' @examples
#' # Create empty 'scsbio' object:
#' x <- scsbio()
#'    
#' # Create 'scsbio' object with specified 'tow.number' and 'sex' fields:
#' x <- scsbio(data.frame(tow.number = 1:10, sex = 1))
#' 
#' # Read data:    
#' x <- read.scsbio()                 # Read all avaliable data.
#' x <- read.scsbio(year = 2019)      # Read single year.
#' x <- read.scsbio(year = 2010:2015) # Read range of years.
#' 
#' summary(x)
#' 
#' @section Functions:
#' \describe{
#'   \item{\code{key.scsbio}}{Define or extract \code{scsbio} object index key.}
#'   \item{\code{scsbio}}{Generic \code{scsbio} method.}
#'   \item{\code{scsbio.default}}{Create an \code{scsbio} object.}
#'   \item{\code{locate.scsbio}}{Locate snow crab biological data files.}
#'   \item{\code{read.scsbio}}{Read snow crab survey biological data.}
#'   \item{\code{scsbio.scsset}}{Load biological associated with snow crab survey tow data.}
#'   \item{\code{summary.scsbio}}{Returns a data summary of an \code{scsbio} object.}
#' } 
#' 

#' @rdname scsbio
#' @export
key.scsbio <- function(x, ...){
   if (missing(x)) return(c("year", "tow.id", "crab.number")) else return(gulf.metadata::key(x))
}

#' @export
scsbio <- function(x, ...) UseMethod("scsbio")

#' @rdname scsbio
#' @export
scsbio.default <- function(x, ...){
   if ("scsbio" %in% class(x)) return(x)

   # Define attributes:
   gulf.metadata::project(x) <- "scs"
   gulf.metadata::key(x) <- key.scsbio()
   gulf.metadata::units(x, c("carapace.width", "chela.height", "abdomen.width")) <- "millimeters"
   gulf.metadata::units(x, "weight") <- "grams"

   # Define class:
   class(x) <- unique(c("scsbio", class(x))) 
   
   return(x)
}

#' @rdname scsbio
#' @export
scsbio.scsset <- function(x, ...){
   v <- read.scsbio(x, ...)
   v <- summary(v, by = key(x), ...)
   index <- match(x[key(x)], v[key(x)])
   x <- cbind(x, v[index, setdiff(names(x), key(x))])
}

#' @rdname scsbio
#' @export locate.scsbio
locate.scsbio <- function(x, year, source = "gulf.data", remove = "bad", ...){
   # Parse survey year:
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if ("year" %in% names(x)) year <- sort(unique(x$year))
   }
   if (!missing(year)) year <- sort(year)
   
   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      if (missing(year)){
         path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}", full.names = TRUE)
      }else{
         path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/Fishing Year ", year)
      }
      path <- paste0(path, "/Trawl Data/South Western Gulf/Raw Data")
      file <- locate(path = path, pattern = "*.txt")
   }
   
   # Data source is 'gulf.data' package:
   if (source == "gulf.data") file <- locate(package = "gulf.data", pattern = c("scs", "bio", "csv"), ...)

   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Remove irrelevant files:
   if (length(remove) > 0){
      index <- rep(TRUE, length(file))
      for (i in 1:length(remove)) index[grep(tolower(remove[i]), tolower(file))] <- FALSE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @rdname scsbio
#' @export read.scsbio
read.scsbio <- function(x, ...){
   # Define file(s) to be read:
   file <- NULL
   if (!missing(x)) if (is.character(x)) file <- x
   if (length(file) == 0){
      if (missing(x)) file <- locate.scsbio(...) else file <- locate.scsbio(x, ...)
   } 

   # Load data:
   v <- NULL
   for (i in 1:length(file)){
      if (length(file) > 10) cat(paste0(i, ") Reading :'", file[i], "'\n"))
         
      # Determine file extension:
      ext <- tolower(unlist(lapply(strsplit(file[i], "[.]"), function(x) x[length(x)])))

      tmp <- NULL
      # Read fixed-width file:
      if (ext == "txt"){
         print(i)
         tmp <- read.fortran(file = file[i], 
                             format = c("A1", "A2", "A2", "A4", "A1", "A2", "A1", "A3", "A1", "A1", "A3", "A4", "A1", "A1", "A6", 
                                        "A5", "A1", "A5", "A1", "A1", "A1", "A1", "A2", "A1", "A1", "A1", "A8", "A1", "A10", "A1",
                                        "A2", "A1", "A8", "A1", "A8", "A1", "A3", "A1", "A1", "A3", "A1", "A4", "A7", "A21", "A6",  
                                        "A1", "A25", "A8"))

         names(tmp) <- c("blank1",  "day", "month", "year", "blank2",  "zone",  "subzone", "blank3", "data.type", 
              "blank4", "tow.number", "crab.number", "blank5", "sex", "carapace.width", "abdomen.width", 
              "blank6", "chela.height", "maturity", "blank7",  "shell.condition", "shell.condition.mossy", 
              "gonad.colour", "blank8", "egg.colour", "eggs.remaining", "tag.number", "blank9", "missing.legs", 
              "blank10", "position.type", "blank11", "latitude.start", "blank12", "longitude.start", "blank13", 
              "depth", "blank14", "soak.days", "durometer", "blank15", "trap.code",  "blank16", "samplers",   
              "weight", "blank17", "comments", "tow.id")  
         
         # Remove blank columns:
         tmp <- tmp[, -grep("blank", names(tmp))]
         
         # Remove blanks:
         for (j in 1:ncol(tmp)) if (is.character(tmp[, j])) tmp[, j] <- deblank(tmp[, j])
          
         # Numeric conversions:
         nvars <- c("day", "month", "year", "tow.number", "crab.number", "carapace.width", "abdomen.width", 
                    "chela.height", "shell.condition", "gonad.colour", "egg.colour", "latitude.start", "longitude.start",
                    "soak.days", "depth", "weight")
         f <- function(x) return(as.numeric(gsub("[*]", "", x)))
         for (j in 1:length(nvars)) tmp[, nvars[j]] <- f(tmp[, nvars[j]])
      }
      
      # Read comma-delimited file:
      if (ext == "csv") tmp <- read.csv(file[i], header = TRUE, stringsAsFactors = FALSE)
      
      # Append data:
      v <- rbind(v, tmp) 
   }
   
   # Subset by specified variables:
   args <- list(...)
   args <- args[names(args) %in% names(v)]
   if (length(args) > 0){
      index <- rep(TRUE, nrow(v))
      for (i in 1:length(args)) index <- index & (v[,names(args)[i]] %in% args[[i]])
      v <- v[index, ]
   }
   
   # Convert to 'scsset' object:
   v <- scsbio(v)
      
   return(v)
}

#' @rdname scsbio
#' @export
summary.scsbio <- function(x, by, category, weight = FALSE, ...){
   # Parse input arguments:
   if (!missing(category)) if (!is.character(category)) stop("'category' must be a vector of character strings.")
   if (!missing(by)) if (!is.character(by)) stop("'by' must be a vector of character strings.")

   if (!missing(category)){
      if (missing(by)) by <- c("year", "tow.id")
      if (weight) x$weight <- weight(x, ...) else x$weight <- 1
      res <- stats::aggregate(list(n = is.category(x, category[1]) * x$weight), by = x[by], sum, na.rm = TRUE)
      if (length(category) > 1){
         for (i in 2:length(category)){
            res <- cbind(res, stats::aggregate(list(n = is.category(x, category[i]) * x$weight), by = x[by], sum, na.rm = TRUE)["n"])
         }
      }
      tmp <- res[grep("n", names(res))]
      names(tmp) <- category
      res <- cbind(res[setdiff(names(res), "n")],  tmp)
      res <- sort(res, by = by)

      return(res)
   }

   # Print data summary:
   describe(x)

   cat("\nData Summary : \n")
   cat(paste0("                             Crab : ", nrow(x), "\n"))
   cat(paste0("                            Males : ", sum(is.category(x, "M"), na.rm = TRUE), "\n"))
   cat(paste0("                          Females : ", sum(is.category(x, "F"), na.rm = TRUE), "\n"))
   cat(paste0("                     Mature Males : ", sum(is.category(x, "MM"), na.rm = TRUE), "\n"))
   cat(paste0("                   Immature Males : ", sum(is.category(x, "MI"), na.rm = TRUE), "\n"))
   cat(paste0("                Legal-sized Males : ", sum(is.category(x, "MGE95"), na.rm = TRUE), "\n"))
   cat(paste0("               Male Skip-Moulters : ", sum(is.category(x, "MISC345"), na.rm = TRUE), "\n"))
   cat(paste0("   Legal-sized Male Skip-Moulters : ", sum(is.category(x, "MIGE95SC345"), na.rm = TRUE), "\n"))
   cat(paste0("                 Commercial Males : ", sum(is.category(x, "COM"), na.rm = TRUE), "\n"))
   cat(paste0("              Commercial Recruits : ", sum(is.category(x, "TMMSC12GE95"), na.rm = TRUE), "\n"))
   cat(paste0("             Commercial Residuals : ", sum(is.category(x, "TMMSC345GE95"), na.rm = TRUE), "\n"))
   cat(paste0("                   Mature Females : ", sum(is.category(x, "FM"), na.rm = TRUE), "\n"))
   cat(paste0("                 Immature Females : ", sum(is.category(x, "FI"), na.rm = TRUE), "\n"))
   
   # Print data issues:
   cat("\nIrregularity summary : \n")
   check(x)
}
