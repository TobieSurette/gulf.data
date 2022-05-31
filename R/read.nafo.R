#' @title Read NAFO Landings Data
#'
#' @description Read a fixed-width formatted NAFO landings data file. 
#'
#' @param x Survey year or file name.
#' @param file File name(s). 
#' @param year Survey year(s).
#' @param ... Arguments passed onto \sQuote{locate.nafo}.
#' 
#' @return A data frame is returned.
#' 
#' @examples
#' # Read the 1960-1965 NAFO landings data files:
#' x <- read.nafo(year = 1960)
#'    
#' # Read the 1960-1965 NAFO landings data files:
#' x <- read.nafo(year = 1960:1965)

#' @export read.nafo
read.nafo <- function(x, file, ...){
   # Determine files to load:
   if (!missing(x) & missing(file)) if (is.character(x)) file = x 
   if (missing(file)) file <- locate.nafo(x, ...)
   if (length(file) == 0) return(NULL)
   
   # Read multiple files:
   if (length(file) > 1){
      v <- NULL
      for (i in 1:length(file)){
         print(paste0("Reading file: ", file[i]))
         # Append data:
         tmp <- read.nafo(file = file[i], ...)
         
         # Make previous and current data tables uniform: 
         vars <- union(names(tmp), names(v))
         tmp[setdiff(vars, names(tmp))] <- NA
         if (!is.null(v)) v[setdiff(vars, names(v))] <- NA
         
         # Append data tables:
         v <- rbind(v[vars], tmp[vars])  
         
         # Convert NA strings to empty strings:
         for (j in 1:ncol(v)) if (is.character(v[,j])) v[is.na(v[,j]), j] <- ""
      }
   }
   
   # Read single file:
   if (length(file) == 1){
      # Read fixed-width file:
      v <- utils::read.fortran(file = file, format = c("I2","I2","I2","I3","I2",
                                                       "I3", "I1","I5",
                                                       "I5","I5","I5","I5","I5","I5",
                                                       "I5","I5","I5","I5","I5","I5"))
      names(v) <- c("year", "country","gear","blank1","nafo.division.code",
                    "species","blank2","blank3",
                    "jan","feb","mar","apr","may","jun",
                    "jul","aug","sep","oct","nov","dec")
      
      v$year <- as.numeric(paste0("19", v$year))
      v$gear.code <- v$gear
      
      # Define months:
      months <- tolower(month.abb)
      
      # Linearize data frame:
      vars <- setdiff(names(v), months)
      for (i in 1:length(vars)){
         if (i == 1){
            temp <- base::data.frame(rep(v[, vars[i]], length(months)))
         }else{
            temp <- cbind(temp, base::data.frame(rep(v[, vars[i]], length(months))))
         }
      }
      names(temp) <- vars
      temp <- cbind(temp, base::data.frame(month = rep(1:length(months), each = nrow(v))))
      temp$round.weight <- as.vector(as.matrix(v[, months]))
      
      # Remove entries with no landings:
      temp <- temp[!is.na(temp$round.weight) & (temp$round.weight > 0), ]
      temp$round.weight <- temp$round.weight * 1000
      
      v <- temp
   }
   
   return(v)
   
}
