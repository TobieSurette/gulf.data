#' Notus Acoustic Trawl Data
#'
#' @description Read Notus trawl acoustic monitoring data files. Acoustic sensors are attached to bottom trawls 
#'              to measure various aspects of the trawl, such as the door spread, wing spread, water depth and 
#'              temperature.
#' 
#' @param file Data file name.
#' 
#' @seealso \code{\link{esonar}}, \code{\link{netmind}}, \code{\link{scanmar}}
#' 

#' @export read.notus
read.notus <- function(file){
  x <- readLines(file) # Read each line separately:
 
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
  data <- cbind(data["Date"], data["Time"], data["Code"], data[setdiff(names(data), c("Date", "Time", "Code"))])
 
  # Convert factors to character strings:
  for (i in 1:ncol(data)) if (is.factor(data[, i])) data[, i] <- as.character(data[, i])
 
  # Convert numeric fields:
  for (i in 2:ncol(data)) data[, i] <- gsub("-", "", data[, i])
  for (i in 2:ncol(data)) if (!all(is.na(data[, i]))) if (all(gsub("[.0-9/-]", "", data[, i]) == ""))  data[, i] <- as.numeric(data[, i])
 
  return(data)
}
