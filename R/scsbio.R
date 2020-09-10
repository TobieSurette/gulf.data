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
#'   \item{\code{fmt.scsbio}}{ASCII file format for reading snow crab biological data.}
#'   \item{\code{convert.scsbio}}{Convert snow crab biological ASCII data to standard format.}
#'   \item{\code{update.scsbio}}{Update snow crab survey biological data repositories.}
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
scsbio.default <- function(x, format = fmt.scsbio(), ...){
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
#' @export
fmt.scsbio <- function(x){
   #           variable name                   format  fill.char  description
   fmt.str = c("blank1",                        "A1",     " ",    "Blank.",
               "day",                           "A2",     "0",    "Day.",
               "month",                         "A2",     "0",    "Month.",
               "year",                          "A4",     " ",    "Year.",
               "blank2",                        "A1",     " ",    "Blank.",
               "zone",                          "A2",     " ",    "Fishing zone.",
               "subzone",                       "A1",     " ",    "Fishing sub-zone.",
               "blank3",                        "A3",     " ",    "Blank.",
               "data.type",                     "A1",     " ",    "Trawl = 3.",
               "blank4",                        "A1",     " ",    "Blank.",
               "tow.number",                    "A3",     "0",    "Tow number.",
               "crab.number",                   "A4",     "0",    "Crab ID number.",
               "blank5",                        "A1",     " ",    "Blank.",
               "sex",                           "A1",     "*",    "Sex.",
               "carapace.width",                "A6",     "*",    "Carapace width(mm).",
               "abdomen.width",                 "A5",     "*",    "Abdomen width(mm).",
               "blank6",                        "A1",     " ",    "Blank.",
               "chela.height",                  "A5",     "*",    "Chela height.",
               "maturity",                      "A1",     "*",    "Maturity.",
               "blank7",                        "A1",     " ",    "Blank.",
               "shell.condition",               "A1",     "*",    "Shell condition.",
               "shell.condition.mossy",         "A1",     " ",    "Mossy shell condition.",
               "gonad.colour",                  "A2",     "*",    "Gonad colour code.",
               "blank8",                        "A1",     " ",    "Blank.",
               "egg.colour",                    "A1",     "*",    "Egg colour code.",
               "eggs.remaining",                "A1",     "*",    "Eggs remaining code.",
               "tag.number",                    "A8",     "0",    "Crab tag number.",
               "blank9",                        "A1",     " ",    "Blank.",
               "missing.legs",                  "A10",    " ",    "Missing leg codification.",
               "blank10",                       "A1",     " ",    "Blank.",
               "position.type",                 "A2",     " ",    "Position type.",
               "blank11",                       "A1",     " ",    "Blank.",
               "latitude.start",                "A8",     " ",    "Start latitude of tow.",
               "blank12",                       "A1",     " ",    "Blank.",
               "longitude.start",               "A8",     " ",    "Start longitude of tow.",
               "blank13",                       "A1",     " ",    "Blank.",
               "depth",                         "A3",     " ",    "Depth(fathoms).",
               "blank14",                       "A1",     " ",    "Blank.",
               "soak.days",                     "A1",     " ",    "Soak days for traps.",
               "durometer",                     "A3",     "*",    "Durometer measurement.",
               "blank15",                       "A1",     " ",    "Blank.",
               "trap.code",                     "A4",     " ",    "Trap code.",
               "blank16",                       "A7",     " ",    "Blank.",
               "samplers",                      "A21",    " ",    "Sampler names.",
               "weight",                        "A6",     " ",    "Crab weight.",
               "blank17",                       "A1",     " ",    "Blank.",
               "comments",                      "A25",    " ",    "Comments.",
               "tow.id",                        "A8",     " ",    "Tow ID string.")

   n <- length(fmt.str) # Total number of fields.
   k <- 4 # Number of columns to be parsed.

   # Recast file.info as a 'fmt' (format) object:
   x <- data.frame(variable = fmt.str[seq(1,n,k)], format = fmt.str[seq(2,n,k)],
                   fill.char = fmt.str[seq(3,n,k)], description = fmt.str[seq(4,n,k)])

   return(x)
}

#' @rdname scsbio
#' @export
convert.scsbio <- function(x, ...){
   if (!attr(x, "converted")){
      temp <- attributes(x) # Get catch attributes.
      x <- as.data.frame(x) # Convert 'x' to data frame.

      # Remove blank columns:
      variables <- names(x)
      index <- grep("blank", variables)
      x <- x[, setdiff(variables, variables[index])]
      temp$format <- temp$format[setdiff(variables, variables[index]), ]

      # Define variables which are to be converted to numeric format:
      vars <- c("day", "month", "year", "data.type", "sex",
                "tow.number", "crab.number", "carapace.width",
                "abdomen.width", "chela.height", "shell.condition",
                "gonad.colour", "egg.colour", "soak.days", "durometer",
                "eggs.remaining", "egg.colour", "weight", "maturity")
      for (i in 1:length(vars)){
         x[, vars[i]] <- gsub("*", " ", x[, vars[i]], fixed = TRUE)
         x[, vars[i]] <- as.numeric(x[, vars[i]], fixed = TRUE)
      }
      type(temp$format[vars, ]) <- "i"
      temp$format["carapace.width", "format"] <- "F6.0"
      temp$format["abdomen.width", "format"]  <- "F5.0"
      temp$format["chela.height", "format"]   <- "F5.0"

      # Convert coordinates:
      x$longitude.start[x$longitude.start == "99999999"] <- NA
      x$latitude.start[x$latitude.start == "99999999"] <- NA
      vars <- c("longitude.start", "latitude.start")
      for (i in 1:length(vars)){
         index <- !is.na(x[, vars[i]])
         x[index, vars[i]] <- as.numeric(substr(x[index, vars[i]], 1, 3)) +  as.numeric(substr(x[index, vars[i]], 4, 8)) / 60000
      }
      if (is.character(x$longitude.start)) x$longitude.start <- as.numeric(x$longitude.start)
      if (is.character(x$latitude.start))  x$latitude.start <- as.numeric(x$latitude.start)
      x$longitude.start <- -x$longitude.start
      temp$format[vars, "format"] <- "F9.7"

      # Correct inconsistencies:
      index <- (x$sex == 1)
      x$eggs.remaining[index] <- NA
      x$gonad.colour[index] <- NA
      x$egg.colour[index] <- NA
      
      # Add species code:
      x <- cbind(x, data.frame(species = rep(2526, dim(x)[1])))
      temp$format <- rbind(temp$format, fmt(var = "species", format = "I4", desc = "Species code."))

      # Restore format attribute:
      temp$names <- rownames(temp$format)
      attributes(x) <- temp

      # Remove spaces from 'tow.id':
      x$tow.id <- gsub(" ", "", x$tow.id)
      
      # Replace zero values with NA:
      x$carapace.width[which(x$carapace.width <= 0)] <- NA
      
      # Remove blank records:
      index <- is.na(x$sex) & is.na(x$carapace.width) & is.na(x$abdomen.width) & is.na(x$chela.height) & is.na(x$shell.condition)
      x <- x[!index, ]
      
      # Remove leading and trailing spaces:
      x$samplers <- gsub("(^[ ]+)|([ ]+$)", "", x$samplers)
      x$comments <- gsub("(^[ ]+)|([ ]+$)", "", x$comments) 
      
      # Add 'tow.id' from tow data:
      y <- read.scsset(year = unique(x$year))
      vars <- c("year", "month", "day", "tow.number")
      index <- match(x[vars], y[vars])
      index[x$tow.id != ""] <- NA
      ii <- !is.na(index)
      x$tow.id[ii] <- y$tow.id[index[ii]]
     
      # Import 'zone' identifier:
      ii <- which(((gsub(" ", "", x$zone) == "") | is.na(x$zone)))
      index <- match(x$tow.id[ii], y$tow.id) 
      x$zone[ii] <- y$zone[index]
      
      # Update 'converted' attribute:
      attr(x, "converted") <- TRUE
   }else{
      temp <- attributes(x) # Get biological attributes.
      format <- fmt(eval(call(class(x)[1]))) # Get catch format.
      x <- as.data.frame(x)

      # Recreate columns which were deleted or absent:
      if (!is.empty(x)){
         x[, setdiff(row.names(format), names(x))] <- NA
      }else{
         x <- cbind(x, data.frame(names = setdiff(row.names(format), names(x))))
      }

      # Re-order columns of x to familiar order and delete standard format columns:
      x <- x[, row.names(format)]

      # Import information from old format if variables are present in new format:
      index <- intersect(row.names(format), row.names(temp$format))
      format[index, ] <- temp$format[index, ]

      # Convert coordinates:
      vars <- c("longitude.start", "latitude.start")
      x$longitude.start <- abs(x$longitude.start)
      for (i in 1:length(vars)) x[, vars[i]] <- deg2dmm(x[, vars[i]]) * 1000
      format[vars, "format"] <- "I8"

      # Restore format attribute:
      temp$names <- rownames(format)
      temp$format <- format
      attributes(x) <- temp

      # Update 'converted' attribute:
      attr(x, "converted") <- FALSE
   }

   return(x)
}

#' @rdname scsset
#' @export 
locate.scsbio <- function(x, year, ...){
   # Parse survey year:
   if (!missing(x) & missing(year)){
      if (is.numeric(x)) year <- x
      if ("year" %in% names(x)) year <- sort(unique(x$year))
   }
   if (!missing(year)) year <- sort(year)
   
   # Parse data file:
   file <- NULL; if (!missing(x)) if (is.character(x)) file <- x
   
   # Use 'gulf.data' as data source:
   if (length(file) == 0){
      if (file.exists(options()$gulf.path$snow.crab)){
         if (missing(year)){
            path <- dir(paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/"), pattern = "Fishing Year [0-9]{4,4}")
         }else{
            path <- paste0("Fishing Year ", year)
         }
         path <- paste0(options()$gulf.path$snow.crab, "/Offshore Crab Common/", path, "/Trawl Data/South Western Gulf/Raw Data")
         file <- locate(pattern = "*.csv", path = path)
      }
      
      if (length(file) == 0) file <- locate(package = "gulf.data", pattern = c("scs", "bio", "csv"), ...)
   } 
   
   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(year[i], file)] <- TRUE
      file <- file[index]
   }
   
   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}

#' @rdname scsbio
#' @export read.scsbio
read.scsbio <- function(x, ...){
 
   # Find data file:
   file <- locate.scsbio(x, ...)
   
   # Load data:
   v <- NULL
   for (i in 1:length(file)) v <- rbind(v, read.csv(file[i], header = TRUE, stringsAsFactors = FALSE))  
   
   # Subset by specified variables:
   v <- base::subset.data.frame(v, ...)
   
   # Convert to 'scsset' object:
   v <- scsbio(v)

   # Subset by 'year' and 'tow.id':
   if (!missing(x)){
      vars <- c("year", "tow.id")
      if (all(vars %in% names(x)))  v <- v[!is.na(match(v[vars], x[vars])), ]
   }  
   
   return(v)
}

#' @rdname scsbio
#' @export update.scsbio
update.scsbio <- function(year, path, Rfile = TRUE, csv = TRUE, ...){
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   flag <- FALSE
   if (!missing(path)) flag <- TRUE
   
   # Loop over years:
   for (i in 1:length(year)){
      if (!flag){
         path <- scsbio.path.str(year = year[i], ...)
         tmp <- strsplit(path, '/')[[1]]
         path <- paste0(tmp[1:which(tmp == "Raw Data")], collapse = "/")
      }
      
      writeable <- Sys.chmod(path = path)
      if (!writeable) stop(paste("Unable to write to: ", path))
         
      # Read data:
      x <- read.scsbio(year = year[i], source = "ascii", ...)
      index <- (x$carapace.width > 0) | !is.na(x$abdomen.width) | !is.na(x$chela.height) | !is.na(x$shell.condition) | 
               !is.na(x$gonad.colour) | !is.na(x$egg.colour) | !is.na(x$eggs.remaining) 
      x[which(index), ]
      
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".Rdata'\n"))
      if (Rfile) save(x, file = paste0(path, "/", "SCS", year[i], ".Rdata"))
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".csv'\n"))
      if (csv) write.table(x, file = paste0(path, "/", "SCS", year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
   }
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
