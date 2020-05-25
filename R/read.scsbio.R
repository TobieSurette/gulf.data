#' Read and Write Snow Crab Biological Data
#' 
#' @description Read and write individual biological data from the southern Gulf snow crab survey.
#'              
#' @param x A \sQuote{data.frame} object. When reading data, \code{x} may be a numeric vector specifying 
#'          the survey years to be loaded or an \code{\link{scsset}} object for which we want to
#'          read the corresponding biological data.
#' @param year Numeric vector specifying the survey years to be loaded.
#' 
#' @param format An \sQuote{fmt} object which specifies the ASCII printing
#' format for an \sQuote{scbio} object.
#' 
#' @param key A character vector of column names which specify which data
#' columns are to be used for specifying the \sQuote{scbio} object's index key.
#' This key is then used as a default for such operations as merging data.
#' @param Character string specifying the path to read or write snow crab biological data.
#' @param Rfile Logical value specifying whether to write an \code{R} data format file when 
#'              updating snow crab biological data.
#' @param csv Logical value specifying whether to write a comma-separated (\code{csv}) format file 
#'              when updating snow crab biological data.
#' @param ... Other parameters (not used).
#' 
#' @examples
#' 
#'    # Create empty 'scbio' object:
#'    x <- scbio()
#'    
#'    # Create 'scbio' object with specified 'tow.number' and 'sex' fields:
#'    x <- scbio(data.frame(tow.number = 1:10, sex = 1))
#'    
#'    # Print column names:
#'    print(names(x))
#'    
#'    # Extract ASCII format:
#'    fmt(x)
#'    
#'    # Convert to character format:
#'    as.character(x)
#' 
#' @seealso \code{\link{scsbio}}
#' 
#' @export read.scsbio
#' @export convert.scsbio
#' @export update.scsbio
#' 
read.scsbio <- function(x, year, ...){
   if (!missing(x)) if ("scsset" %in% class(x)) year <- sort(unique(x$year))
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Use 'gulf.data' as data source:
   if (!missing(year)){
      v <- NULL
      for (i in 1:length(year)){
         file <- file.locate(package = "gulf.data", pattern = c("scs", "bio", "csv", year[i]))
         if (length(file) == 1) v <- rbind(v, read.csv(file, header = TRUE, stringsAsFactors = FALSE))
      }
   }  
   
   # Remove tows that are not in 'scsset' object:
   if (!missing(x)){
      if ("scsset" %in% class(x)){
         vars <- c("year", "month", "day", "tow.id")
         v <- !is.na(match(v[index], x[vars]))
      }
   }
   
   # Convert to 'scbio' object:
   v <- scsbio(v)
   
   # Subset by specified variables:
   v <- base::subset.data.frame(v, ...)

   return(v)
}

#' @describeIn read.scsbio Convert ASCII-source snow crab biological data into a user-friendly format.
convert.scbio <- function(x, ...){
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
      y <- read.scset(year = unique(x$year))
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

#' @describeIn read.scsbio Update snow crab biological data files.
update.scbio <- function(year, path, Rfile = TRUE, csv = TRUE, ...){
   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   flag <- FALSE
   if (!missing(path)) flag <- TRUE
   
   # Loop over years:
   for (i in 1:length(year)){
      if (!flag){
         path <- scbio.path.str(year = year[i], ...)
         tmp <- strsplit(path, '/')[[1]]
         path <- paste0(tmp[1:which(tmp == "Raw Data")], collapse = "/")
      }
      
      writeable <- Sys.chmod(path = path)
      if (!writeable) stop(paste("Unable to write to: ", path))
         
      # Read data:
      x <- read.scbio(year = year[i], source = "ascii", ...)
      index <- (x$carapace.width > 0) | !is.na(x$abdomen.width) | !is.na(x$chela.height) | !is.na(x$shell.condition) | 
               !is.na(x$gonad.colour) | !is.na(x$egg.colour) | !is.na(x$eggs.remaining) 
      x[which(index), ]
      
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".Rdata'\n"))
      if (Rfile) save(x, file = paste0(path, "/", "SCS", year[i], ".Rdata"))
      cat(paste0("Writing to : '", path, "/", "SCS", year[i], ".csv'\n"))
      if (csv) write.table(x, file = paste0(path, "/", "SCS", year[i], ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")
   }
}


