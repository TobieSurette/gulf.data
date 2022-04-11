#' @title Read Notus Acoustic Trawl Data.
#'
#' @description Functions to read Notus acoustic trawl data, such as depth/temperature or acoustic trawl monitoring data.
#'
#' @param x Survey year or file name.
#' @param file File name(s).
#' @param year Survey year(s).
#' @param survey Survey type, as determined by the \link{survey.scsset} function.
#' @param tow.id Numeric value or character string specifying the ID corresponding to a particular tow sampling station.
#' @param offset Numeric value specifying the offset time (in minutes) to include as a corrective in the data time stamps.
#' @param repeats Logical value specifying whether to keep or average out data records with identical time stamps.
#' @param ... Other parameters passed onto \code{locate} functions or used to subset data.

#' @export read.notus
read.notus <- function(x, file, echo = FALSE, ...){
  # Define file(s) to be read:
  if (!missing(x) & missing(file)) if (is.character(x)) file = x
  if (missing(file)){
    if (missing(x)) file <- locate.notus(...) else file <- locate.notus(x, ...)  
  }
  if (length(file) == 0) return(NULL)
  
  # Read multiple netmind files and concatenate them:
  if (length(file) == 0) return(NULL)
  if (length(file) > 1){
    x <- vector(mode = "list", length = length(file))
    k <- 0
    for (i in 1:length(file)){
      if (echo) cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
      x[i] <- list(expand(read.notus(file[i])))
      k <- k + nrow(x[[i]])
    }
    
    # Standardize data frame formats:
    vars <- unique(unlist(lapply(x, names)))
    for (i in 1:length(x)){
      ix <- setdiff(vars, names(x[[i]]))
      if (length(ix) > 0){
        x[[i]][ix] <- ""
        x[[i]] <- x[[i]][vars]
      }
    }
    
    # Efficiently catenate data frames:
    while (length(x) >= 2){
      ix <- seq(2, length(x), by = 2)
      for (i in ix) x[i] <- list(rbind(x[[i-1]], x[[i]]))
      if (i < length(x)) ix <- c(ix, length(x))
      x <- x[ix]
    }
    x <- x[[1]]
    
    attr(x, "header") <- NULL
    
    return(x)
  }
  
  # Read single file, line-by-line:
  x <- readLines(file) 

  # Parse variable field names:
  i <- min(grep("^Code", x))
  vars <- strsplit(x[i], ",")[[1]]

  # Extract date from header and reformat:
  date <- x[grep("Tow Date:", x)]
  date <- strsplit(strsplit(date, ": ")[[1]][2], " ")[[1]][1]
  str <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:length(str)) if (length(grep(str[i], date)) > 0) date <- gsub(str[i], ifelse(i<10, paste0("0", i), i), date)
  date <- strsplit(date, "-")[[1]]
  date <- paste0(date[3:1], collapse = "-")

  # Build transponder table:
  ix <- grep("transponder", tolower(x))
  tab <- data.frame(transponder = unlist(lapply(strsplit(x[ix], ": "), function(x) x[2])),
                    location    = gsub(" ", ".", tolower(unlist(lapply(strsplit(x[ix+1], ": "), function(x) x[2])))),
                    stringsAsFactors = FALSE)
  
  # Remove non-data lines:
  x <- x[grep("^[A-Z][0-9][0-9][A-Z]", x)]

  # Split using commas:
  y <- strsplit(x, ",")

  # Convert to data frame:
  for (i in 1:length(vars)){
    if (i == 1) data <- data.frame(unlist(lapply(y, function(x) x[i]))) else data <- cbind(data, data.frame(unlist(lapply(y, function(x) x[i]))))
  }
  names(data) <- vars
  data$Date <- date
  data$location <- tab$location[match(data$Code, tab$transponder)]
  data <- cbind(data["Date"], data["Time"], data["Code"], data["location"], data[setdiff(names(data), c("location", "Date", "Time", "Code"))])

  # Convert factors to character strings:
  for (i in 1:ncol(data)) if (is.factor(data[, i])) data[, i] <- as.character(data[, i])

  # Convert numeric fields:
  for (i in 2:ncol(data)) data[, i] <- gsub("-", "", data[, i])
  for (i in 2:ncol(data)) if (!all(is.na(data[, i]))) if (all(gsub("[.0-9/-]", "", data[, i]) == ""))  data[, i] <- as.numeric(data[, i])
  names(data) <- tolower(names(data))
  
  # Format comments:
  data$comment[is.na(data$comment)] <- ""
  
  # Format coordinates:
  if ("latitude" %in% names(data))  data$latitude  <- gulf.spatial::dmm2deg(as.numeric(gsub("['° SNEW]", "", data$latitude)))
  if ("longitude" %in% names(data)) data$longitude <- -gulf.spatial::dmm2deg(as.numeric(gsub("['° SNEW]", "", data$longitude)))
  
  # Sort by time stamp:
  data <- data[order(gulf.utils::time(data)), ]
  rownames(data) <- NULL
  
  return(data)
}
