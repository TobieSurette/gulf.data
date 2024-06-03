#' @title Fishery Landings
#' 
#' @description Generate a summary of fishery landings.
#' 
#' @param year Fishery year.
#' @param by Character vector namings variables by which to group the data.
#' @param date.start,date.end Start and end dates to bound the data summary.
#' 

#' @export 
landings <- function(year, by){
   # Load official landings table:
   res <- read.csv(file = locate(package = "gulf.data", keywords = c("fishery", "statistics")))
   if (!missing(year)) res <- res[res$year %in% sort(unique(year)), ]
   
   if (missing(by)){
      res <- aggregate(res["landings"], by = res["year"], sum)      
      return(res)
   }else{
      # Add 'year' to 'by' groupings"
      by <- tolower(unique(c("year", by)))
     
      # Grouping variables are all in statistics table:
      if (all (by %in% names(res))){
         res <- aggregate(res["landings"], by = res[by], sum)
         return(res)
      }
         
      # Read logbook data:
      x <- gulf.data::read.logbook(year)
         
      # Extract fishing year:
      x$year <- year(x$date.caught)
      
      res <- aggregate(list(landings = x$slip.prop.day), by = x[by], sum, na.rm = TRUE)

      return(res)   
   } 
}






