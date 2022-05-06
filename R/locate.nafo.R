#' Locate Historical NAFO Landings Data
#' 
#' @description Functions to locate yearly data file(s) of historical NAFO landings.
#' 
#' @param x Data object.
#' @param year Calendar year of the landings, must be between 1960 and 1997.
#' @param source Character string specifying the data source for data files. Options are \sQuote{ascii} or a gulf 
#'               package name (e.g. \sQuote{gulf.data}).
#' @param remove Character string vector specifying a list of key words, which if found in the data path or file name, 
#'               are removed from the search results.
#' 
#' @examples 
#' locate.nafo()          # Find all NAFO files.
#' locate.nafo(1965)      # Find specific year.
#' locate.nafo(1965:1970) # Find set of years.
#' 
#' @seealso \code{\link{read.nafo}}

#' @export locate.nafo
locate.nafo <- function(x, year, source = "ascii", ...){
   if (!missing(x)) if (is.numeric(x)) year <- x

   # Use 'gulf.data' as data source:
   if (source == "ascii"){
      path <- paste0(options()$gulf.path$groundfish$nafo)
      file <- locate(path = path, pattern = c("blah"))
      }

   # Year subset:
   if (!missing(year) & (length(file) > 0)){
      index <- rep(FALSE, length(file))
      for (i in 1:length(year)) index[grep(substring(year[i],3,4), file)] <- TRUE
      file <- file[index]
   }

   # Empty search:
   if (length(file ) == 0)  return(NULL) else return(file)
}
