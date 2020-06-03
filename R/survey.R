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
#' @export survey
#' @export survey.default
#' @export survey.character

#' @rdname survey
survey <- function(x, ...) UseMethod("survey")

#' @rdname survey
survey.default <- function(x, survey, year, ...){
   if (missing(survey) & missing(year)) return(survey.character(...))

   # Get project ID:
   project <- survey(survey)

   # September groundfish survey list:
   if (project == "rvs"){
       tab <- list("1970" = "P079",
                   "1971" = "P091",
                   "1972" = "P106",
                   "1973" = "P122",
                   "1974" = "P143",
                   "1975" = "P157",
                   "1976" = "P172",
                   "1977" = "P188",
                   "1978" = "P204",
                   "1979" = "P229",
                   "1980" = "P244",
                   "1981" = "P260",
                   "1982" = "P278",
                   "1983" = "P296",
                   "1984" = "P312",
                   "1985" = c("P327", "H141"),
                   "1986" = "H159",
                   "1987" = "H179",
                   "1988" = "H192",
                   "1989" = "H204",
                   "1990" = "H219",
                   "1991" = "H232",
                   "1992" = c("N176", "N178", "H245"),
                   "1993" = "N192",
                   "1994" = "N210",
                   "1995" = "N230",
                   "1996" = "N249",
                   "1997" = "N746",
                   "1998" = "N846",
                   "1999" = "N941",
                   "2000" = "N045",
                   "2001" = "N150",
                   "2002" = "N251",
                   "2003" = "T352",
                   "2004" = c("T434", "N446"),
                   "2005" = c("T507", "N542"),
                   "2006" = "T678",
                   "2007" = "T749",
                   "2008" = "T815",
                   "2009" = "T992",
                   "2010" = "T074",
                   "2011" = "T194",
                   "2012" = "T205",
                   "2013" = "T318",
                   "2014" = "T433",
                   "2015" = "T533",
                   "2016" = "T661",
                   "2017" = "T777",
                   "2018" = "T896",
                   "2019" = "T901")
     }

   # Northumberland Strait summer survey list:
   if (project == "nss"){
      tab <- list("1999" = "O901",
                  "2000" = "O024",
                  "2001" = c("O139","O160") ,
                  "2002" = c("O241", "O218", "O263"),
                  "2003" = c("O341", "O320", "O365"),
                  "2004" = c("O434", "O456"),
                  "2005" = "O536",
                  "2006" = "O637",
                  "2007" = "O030",
                  "2008" = "O022",
                  "2009" = "O029",
                  "2010" = c("O103", "O104"),
                  "2011" = "O129",
                  "2012" = c("O026", "O048"),
                  "2013" = "P126",
                  "2014" = "P021",
                  "2015" = "P521",
                  "2016" = "P018",
                  "2017" = c("P024", "P329"),
                  "2018" = c("P041", "P829"),
                  "2019" = c("P140", "P953"))
   }

   # Sentinel survey list:
   if (project == "sens"){
      tab <- list("2003" = "S001",
                  "2004" = "S002",
                  "2005" = "S003",
                  "2006" = "S004",
                  "2007" = "S005",
                  "2008" = "S006",
                  "2009" = "S007",
                  "2010" = "S008",
                  "2011" = "S009",
                  "2012" = "S010",
                  "2013" = "S011",
                  "2014" = "S012",
                  "2015" = "S013",
                  "2016" = "S014",
                  "2017" = "S015",
                  "2018" = "S016",
                  "2019" = "S017")
   }

   # Inshore survey list:
   if (project == "ins"){
      tab <- list("1985" = c("A001", "A002", "A003", "A004", "A005", "N012"),
                  "1986" = c("A010", "A011", "A012", "A013", "N014"),
                  "1987" = "N017",
                  "1988" = c("N028", "N038"),
                  "1994" = "A021",
                  "1996" = "C043",
                  "1997" = "C038",
                  "1999" = "Y001",
                  "2000" = "Y002",
                  "2001" = c("A031", "A032", "A033", "Y003"))
   }

   # January surveys:
   if (project == "jans"){
      tab <- list("1994" = "N197",
                  "1995" = "N214",
                  "1996" = "T182",
                  "1997" = "T201",
                  "2003" = "N001")
   }

   # Juvenile survey list:
   if (project == "juvs"){
      tab <- list("1988" = "J046",
                  "1989" = "N015",
                  "1990" = "J088",
                  "1991" = c("JC91", "JH01"),
                  "1992" = "P431",
                  "1993" = "P446",
                  "1994" = "J094",
                  "1995" = "J095")
     }

   # Seasonal survey list:
   if (project == "seas"){
      tab <- list("1986" = c("H059", "H166"),
                  "1987" = c("H079", "H172", "H174", "N073"),
                  "1989" = "H209",
                  "1990" = c("H215", "H223", "V036"),
                  "1991" = c("N151", "V137"),
                  "1992" = "NV35")
   }

   # Scallop survey list:
   if (project == "scas"){
   tab <- list("2012" = "O021",
               "2013" = "P019",
               "2014" = "P047",
               "2015" = "P026",
               "2016" = "P621",
               "2018" = "P841",
               "2019" = c("P201","P242"))
   }

   # Return the whole list if year is unspecified:
   if (missing(year)) v <- tab else v <- tab[as.character(year)]

   # Replace NULL values with NAs:
   v[unlist(lapply(v, is.null))] <- list(NA)

   # Convert to named vector:
   v <- unlist(v)
   names(v) <- substr(names(v), 1, 4)

   return(v)
}

#' @rdname survey
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
