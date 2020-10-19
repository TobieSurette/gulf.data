#' Survey Identifiers
#'
#' @description Returns research survey codes and identifiers.
#'
#' @param x Object.
#' @param year Survey year(s).
#'
#' @param survey Character string specifying the research survey. The input is passed onto the
#'               \code{\link{project}} function. To see the complete list of survey IDs available,
#'               run \code{survey()}.
#'
#' @param verbose Logical value specifying whether to return the survey identifiers in long form.
#'
#' @examples
#' # Complete lists survey codes:
#' survey()                      # Complete survey table.
#' survey(project = "sept")      # September multispecies survey table.
#  survey("rv")                  # September multispecies survey table.
#' survey(project = "ns")        # Northumberland Strait survey ID.
#' survey(project = "northumb")  # Northumberland Strait survey ID.
#'
#' survey("^P")                  # Surveys whose IDs start with "P".
#' 
#' # Return the September research vessel survey code for 2005:
#' survey(year = 2005:2007, project = "rv")
#'
#' # Return the September research vessel survey codes for a vector of years:
#' survey(survey = "rv", year = 2000:2019)
#'
#' # Return the Nothumberland Strait survey codes for a vector of years:
#' survey(year = 2000:2009, survey = "ns")
#'
#' # Return the juvenile cod survey codes for a vector of years:
#' survey(year = 1990:1995, survey = "juv")
#' 
#' # Snow crab survey type:
#' x <- read.scsset(2019)
#' survey(x) # Separates regular fall and comparative survey sets.

#' @export
survey <- function(x, ...) UseMethod("survey")

#' @describeIn survey Fetch survey identifiers.
#' @export
survey.default <- function(x, year, project, ...){
   v <- read.csv(locate(package = "gulf.data", file = "survey.csv"), header = TRUE, stringsAsFactors = FALSE)
   
   # Subset by project:
   if (!missing(project)) v <- v[v$project %in% project(project), ]
   
   # Subset by year:
   if (!missing(year)) v <- v[v$year %in% year, ]
   
   if (!missing(x)){
      if (length(x) > 0){
         index <- NULL
         for (i in 1:length(x)) index <- c(index, grep(tolower(x[i]), tolower(v$project)))
         for (i in 1:length(x)) index <- c(index, grep(tolower(x[i]), tolower(v$id)))
         index <- sort(unique(index))
         v <- v[index, ]
      }
   }
   
   return(v)
}

#' @describeIn survey Determine type of survey sampling for snow crab survey tows.
#' @export
survey.scsset <- function(x, ...){
   month <- as.numeric(substr(x$date, 6, 7))
   v <- rep("", nrow(x))
   v[month %in% 4:5] <- "spring"
   v[month %in% 6] <- "summer"
   v[month %in% 7:10] <- "regular"
   
   index <- which((gulf.utils::year(x) == 2019) & (substr(x$tow.id,2,2) == "C"))
   v[index] <- paste(v[index], "comparative")
   
   return(v)
}

#' @describeIn survey Determine type of survey sampling for snow crab survey biological data.
#' @export
survey.scsbio <- function(x, ...){
   month <- as.numeric(substr(x$date, 6, 7))
   v <- rep("", nrow(x))
   v[month %in% 4:5] <- "spring"
   v[month %in% 6] <- "summer"
   v[month %in% 7:10] <- "regular"
   
   index <- which((gulf.utils::year(x) == 2019) & (substr(x$tow.id,2,2) == "C"))
   v[index] <- paste(v[index], "comparative")
   
   return(v)
}

