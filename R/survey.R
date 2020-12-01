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

scs.survey <- function(x, ...){
   month <- as.numeric(substr(x$date, 6, 7))
   v <- rep("", nrow(x))
   v[month %in% 4:5] <- "spring"
   v[month %in% 6] <- "summer"
   v[month %in% 7:10] <- "regular"
   
   # 2002 catchability study:
   index <- which(substr(x$date,1,10) %in% c("2002-09-24", "2002-09-25"))
   v[index] <- "catchability"
   
   # 2004 catchability study:
   index <- which(substr(x$date,1,10) %in% c("2004-10-18", "2004-10-24", "2004-10-25"))
   v[index] <- "catchability"
   
   # 2005 special studies:
   index <- which(substr(x$date,1,10) %in% c("2005-10-04", "2005-10-05", "2005-10-06"))
   v[index] <- "catchability"
   index <- which(substr(x$date,1,10) %in% "2005-10-12")
   v[index] <- "selectivity"

   # 2019 comparative study:
   index <- which((gulf.utils::year(x) == 2019) & (substr(x$tow.id,2,2) == "C"))
   v[index] <- paste(v[index], "comparative")
   
   return(v)
}

#' @describeIn survey Determine type of survey sampling for snow crab survey tows.
#' @export
survey.scsset <- function(x, ...) return(scs.survey(x))

#' @export
survey.scscat <- function(x, ...) return(scs.survey(x))

#' @export
survey.scsbio <- function(x, ...) return(scs.survey(x))

nss.survey <- function(x){
   # Define regular survey cruises:
   regular <- c("O901", "O024", "O139", "O241", "O341", "O434", "O536", "O637", "O030", "O022", "O029", "O103", "O129",
                "O026", "P126", "P021", "P521", "P018", "P024", "P041", "P140", "P150")
   fall <- c("O160", "O263", "O365", "O456", "P329", "P829")
   tagging <- "P953"
   spring <- c("O218", "O320")
   test <- "O104"

   # Do assignment:
   v <- rep("", nrow(x))
   v[x$cruise %in% regular] <- "regular"
   v[x$cruise %in% fall]    <- "fall"
   v[x$cruise %in% tagging] <- "tagging"
   v[x$cruise %in% spring]  <- "spring"
   v[x$cruise %in% test]    <- "trawl test"
   
   return(v)
}

#' @describeIn survey Determine type of survey sampling for Northumberland Strait survey tows.
#' @export
survey.nssset <- function(x, ...) return(nss.survey(x))

#' @export
survey.nsscat <- function(x, ...) return(nss.survey(x))

#' @export
survey.nssbio <- function(x, ...) return(nss.survey(x))

#' @export
survey.nsslen <- function(x, ...) return(nss.survey(x))

