#' Survey Identifiers
#'
#' @description Returns research survey codes and identifiers.
#'
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
#' survey()               # List all survey IDs.
#' survey(verbose = TRUE) # List survey full names.
#  survey("rv")           # September multispecies survey ID.
#  survey("sept")         # September multispecies survey ID.
#  survey("september")    # September multispecies survey ID.
#' survey("ns")           # Northumberland Strait survey ID.
#' survey("northumb")     # Northumberland Strait survey ID.
#' survey("sen")          # Sentinel survey ID.
#'
#' # Return the September research vessel survey code for 2005:
#' survey(year = 2005:2007, survey = "rv")
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
#' @section Functions:
#' \describe{
#'   \item{\code{survey}}{Generic \code{survey} method.}
#'   \item{\code{survey.default}}{Fetch survey identifiers.}
#'   \item{\code{survey.character}}{Fetch survey identifiers and information.}
#' } 
#' 
 
#' @export
survey <- function(x, ...) UseMethod("survey")

#' @export
survey.default <- function(x, survey, year, ...){
   if (missing(survey) & missing(year)) return(survey.character(...))

   # Get project ID:
   project <- survey(survey)

   file <- locate(package = "gulf.data", pattern = c("survey.csv"))
   tab <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   tab <- tab[tab$project == project, ]
   
   # Return the whole list if year is unspecified:
   if (missing(year)) v <- tab else v <- tab[as.character(year)]

   # Replace NULL values with NAs:
   v[unlist(lapply(v, is.null))] <- list(NA)

   # Convert to named vector:
   v <- unlist(v)
   names(v) <- substr(names(v), 1, 4)

   return(v)
}

#' @export
survey.character <- function(x, survey, year, verbose = FALSE, ...){
   if (missing(x) & !missing(survey) & !missing(year))
       return(survey.default(survey = survey, year = year, verbose = verbose, ...))
   if (missing(x) & !missing(survey)) x <- survey

   # Load project information:
   p <- project()
   p <- p[grep("survey", p$keyword), ]
   if (missing(x)) if (!verbose) return(p$name) else return(p$name.long)

   # Look up survey names:
   x <- tolower(x)
   v <- match(x, gsub("s$", "", p$name))
   v[is.na(v)] <- pmatch(x[is.na(v)], tolower(p$name))
   v[is.na(v)] <- pmatch(x[is.na(v)], tolower(p$name.long))

   if (!verbose) v <- p$name[v] else v <- p$name.long[v]

   return(v)
}
