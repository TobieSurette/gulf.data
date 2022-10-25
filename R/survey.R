#' @title Survey Identifiers
#'
#' @description Returns research survey codes and identifiers.
#'
#' @param x Object.
#' @param year Survey year(s).
#' @param project Data project.
#' @param survey Character string specifying the research survey. The input is passed onto the
#'               \code{\link{project}} function. To see the complete list of survey IDs available,
#'               run \code{survey()}.
#'
#' @param verbose Logical value specifying whether to return the survey identifiers in long form.
#' @param output Character string specifying the column(s) of the survey table to be returned.
#' @param ...
#'
#' @examples
#' # Complete lists survey codes:
#' survey()                      # Complete survey table.
#' survey(project = "sept")      # September multispecies survey table.
#' survey("rv")                  # September multispecies survey table.
#' survey(project = "ns")        # Northumberland Strait survey ID.
#' survey(project = "northumb")  # Northumberland Strait survey ID.
#'
#' survey("^P")                  # Surveys whose IDs start with "P".
#' 
#' # Return the September research vessel survey code for 2005-2007:
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
survey.default <- function(x, year, project, output, ...){
   v <- read.csv(locate(package = "gulf.data", file = "survey.csv"), header = TRUE, stringsAsFactors = FALSE)
   
   # Subset by project:
   if (!missing(project)) v <- v[v$project %in% project(project), ]
   
   # Subset by year:
   if (!missing(year)) v <- v[v$year %in% year, ]
   
   if (!missing(x)){
      if (length(x) > 0){
         ix <- NULL
         for (i in 1:length(x)) ix <- c(ix, grep(tolower(x[i]), tolower(v$project)))
         for (i in 1:length(x)) ix <- c(ix, grep(tolower(x[i]), tolower(v$id)))
         ix <- sort(unique(ix))
         v <- v[ix, ]
      }
   }
   
   if (!missing(output)){
      output <- output[output %in% names(v)]
      if (length(output) > 0) v <- v[, output]
   }
      
   return(v)
}

scs.survey <- function(x, output = "type", ...){
   if (output == "type"){
      month <- as.numeric(substr(x$date, 6, 7))
      v <- rep("", nrow(x))
      v[month %in% 4:5]  <- "spring"
      v[month %in% 6]    <- "summer"
      v[month %in% 7:10] <- "regular"
      
      # 1988 cross study:
      v[which((year(x) == 1988) & gulf.utils::date(x) >= gulf.utils::date("1988-10-31"))] <- "study"
      
      # 2002 catchability study:
      v[which(substr(x$date,1,10) %in% c("2002-09-24", "2002-09-25"))] <- "catchability"
      
      # 2004 catchability study:
      v[which(substr(x$date,1,10) %in% c("2004-10-18", "2004-10-24", "2004-10-25"))] <- "catchability"
      
      # 2005 special studies:
      v[which(substr(x$date,1,10) %in% c("2005-10-04", "2005-10-05", "2005-10-06"))] <- "catchability"
      v[which(substr(x$date,1,10) %in% "2005-10-12")] <- "selectivity"
      
      # 2019 comparative study:
      v[which((gulf.utils::year(x) == 2019) & (substr(x$tow.id,2,2) == "C"))] <-"comparative"
      
      # 2021 trawl experiment:
      v[which((gulf.utils::year(x) == 2021) & (substr(x$tow.id,1,2) == "XP"))] <-"trawl experiment"
   }
   
   return(v)
}

#' @describeIn survey Determine type of survey sampling for snow crab survey tows.
#' @export
survey.scsset <- function(x, ...) return(scs.survey(x))

#' @export
survey.scscat <- function(x, ...) return(scs.survey(x))

#' @export
survey.scsbio <- function(x, ...) return(scs.survey(x))

#' @export
survey.scslen <- function(x, ...) return(scs.survey(x))

#' @describeIn survey Fetch survey identifiers for 'gulf.set' objects.
#' @rawNamespace S3method(survey,gulf.set)
survey.gulf.set <- function(x, output, ...){
   # Initialize result variable:
   r <- NULL
   
   if (!missing(output)){
      # Look up column in survey information table:
      tab <- survey()
      output <- output[output %in% names(tab)]
      if (length(output) > 0){
         id <- paste0(x$vessel.code, gsub(" ", "0", formatC(x$cruise.number, width = 3)))
         ix <- match(id, tab$id)
         r <- tab[ix, output]
      } 
      return(r)
   }else{
      id <- paste0(x$vessel.code, gsub(" ", "0", formatC(x$cruise.number, width = 3)))
      return(id)
   }
}

#' @rawNamespace S3method(survey,gulf.cat)
survey.gulf.cat <- function(x, ...) return(survey.gulf.set(x, ...))

#' @rawNamespace S3method(survey,gulf.bio)
survey.gulf.bio <- function(x, ...) return(survey.gulf.set(x, ...))

#' @rawNamespace S3method(survey,gulf.len)
survey.gulf.len <- function(x, ...) return(survey.gulf.set(x, ...))

