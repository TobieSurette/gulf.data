#' File Header Information
#' 
#' @description Extracts header information from certain data files.
#' 
#' @param x File name or object.
#' 
#' @section **Minilog** Header Variables:
#' 
#' \describe{
#'   \item{Source.File}{Source fie from which the text version was generated.}
#'   \item{Source.Device}{Device identifier.}
#'   \item{Study.Description}{French species names.}
#'   \item{Minilog.Initialized}{Date when device was initialized.}
#'   \item{Study.Start.Time}{When data recording began.}
#'   \item{Study.Stop.Time}{When data recording stopped.}
#'   \item{Sample.Interval}{Sampling rate at which data were recorded.}
#' }
#'   
#' @examples 
#' file <- system.file(
#' header.minilog(
#' 
#' @export header
#' @export header.default
#' @export header.minilog
#' 
#' @seealso \code{\link[gulf.data]{minilog}}
#' 
header <- function(x, ...) UseMethod("header")

#' @describeIn header Retrieve \code{header} attribute.
header.default <- function(x) return(attr(x, "header"))

#' @describeIn header Retrieve Minilog header information.
header.minilog <- function(x){
   # Extract header attributes:
   if ("minilog" %in% class(x)) return(header.default(x))
   
   # Read header from file:
   if (is.character(x)){
      if (length(x) > 1){
         v <- NULL
         for (i in 1:length(x)) v <- rbind(v, header.minilog(x[i]))
         return(v)
      }else{
         y <- readLines(x, n = 20, encoding = "UTF-8")
         k <- sum(substr(y, 1, 1) %in% c("*", letters, LETTERS))-1
         y <- strsplit(y, "([:][ ])|([=])")[1:k] # Split header fields and their values.
         header <- unlist(lapply(y, function(x) x[2]))
         names(header) <- gsub(" ", ".", unlist(lapply(y, function(x) x[1])))
         str <- strsplit(header["Source.File"], "\\\\")[[1]]
         header["Source.File"] <- str[length(str)]
         header <- as.data.frame(t(header))
      }
   }
   
   return(header)
}
