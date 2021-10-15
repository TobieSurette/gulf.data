#' @title Find Event Time Files
#' 
#' @description Locate files containing event times for various surveys.
#' 
#' @param year Survey year.
#' @param project Project identifier.
#' 

#' @export locate.event
locate.event <- function(x, year, project = "scs", ...){
   # Parse project identifier.
   project <- gulf.metadata::project(project)
   
   # Parse year:
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Locate all files.
   v <- locate(package = "gulf.data", keywords = c(project, "events"))
   
   # Subset by year:
   if (!missing(year)){
      ix <- NULL
      year <- unique(year)
      for (i in 1:length(year)) ix <- unique(c(ix, grep(year[i], v)))
      v <- v[ix]
   }
   
   return(v)
}
