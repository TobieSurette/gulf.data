#' @title Fishery Landings
#' 
#' @description Generate a summary of fishery landings.
#' 
#' @param year Fishery year.
#' @param by Character vector namings variables by which to group the data.
#' @param date.start,date.end Start and end dates to bound the data summary.
#' 

#' @export 
landings <- function(year, by, date.start, date.end){
   
   # Check input arguments:
   if (missing(year)) stop("'year' must be specified.")
   
   # Read data:
   x <- gulf.data::read.sc.logbook(year)

   # Parse 'by' argument:
   if (missing(by)) by <- NULL
   vars <- c(by)
   var <- vars[vars %in% names(x)]
   
   if (length(vars) == 0){
      res <- sum(x$slip.prop.day)
   }else{
      if (!missing(date.start) | !missing(date.end)) vars <- unique(c(vars, "date.landed"))
      vars <- vars[vars %in% names(x)]

      res <- aggregate(list(landings = x[, "slip.prop.day"]), by = x[vars], sum, na.rm = TRUE)
      res$effort <- aggregate(list(effort = x[, "trap.day"]), by = x[vars], sum, na.rm = TRUE)$effort
      res$cpue <- res$landings / res$effort
   } 
   
   # Calculate grid statistics:
   

   
   # Aggregate by start and end dates:
   if (!missing(date.start)){
      date.start <- as.POSIXct(date.start)
      if (length(date.start) == 1){
         res <- res[which(as.POSIXct(res$date.landed) >= date.start), ]
      }else{
         stop("'date.start' must be a single value.")  
      }
   }
   if (!missing(date.end)){
      date.end <- as.POSIXct(date.end)
      if (length(date.end) == 1){
         res <- res[which(as.POSIXct(res$date.landed) <= date.end), ]
      }else{
         stop("'date.end' must be a single value.")  
      }
   }
   
   
   return(res)   
}






