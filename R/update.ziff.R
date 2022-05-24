#' @title Update ZIFF Data
#' 
#' @description Update Zonal Interchange File Format (ZIFF) data, by reading ASCII data and saving an R data version.
#' 
#' @param year Year.
#' @param region Fishing zone identifier.
#' @param ... Not used.
#' 

#' @export update.ziff
update.ziff <- function(year, region = "g", ...){
   # Check 'year' argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be an integer.")
   
   # Parse 'region' argument:
   region <- substr(tolower(as.character(region)),1,1)
   region <- region[region %in% c("g", "n", "q", "s")]
   
   # Loop over years:
   for (i in 1:length(year)){
      # Find file:
      file <- locate.ziff(year[i], region = region, source = "ascii", ...)
      
      # Upload data:
      x <- read.ziff(file, ...)
         
      # Write data:
      save(x, file = paste0(options("gulf.path")[[1]]$groundfish$ziff, "R versions/", region, "_raw_", year[i], ".Rdata"))
   }
}
