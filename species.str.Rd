\name{species.str}

\alias{species.str}

\title{
   Species Names.
}

\description{
   Returns a species name for a given species code.
}

\usage{
   species.str(code, language = "english", coding = "res", source = "oracle",  as.data.frame = FALSE)
}

\arguments{
   \item{code}{Numerical vector of species codes.}

   \item{language}{A character string specifying the language in which species
                   names are to be returned. Either english (\code{language = "english"}), 
                   french (\code{language = "french"} or \code{language = "français"})
                   or latin (\code{language = "latin"}). Only used for research 
                   vessel species coding (\code{e.g. coding = "res"}).
                  }
                   
   \item{coding}{A character string specifying the type of species coding being 
                 used. It may refer to standard research survey coding (\code{coding = "rv"}, 
                 \code{coding = "res"} or \code{coding = "research"}), to Statistical 
                 Coordinating Committee for the Atlantic Coast (STACAC) coding 
                 (\code{= "STACAC"}), to North American Fisheries Organization 
                 (NAFO) coding (\code{= "NAFO"}), observer file name coding 
                 (\code{= "obs" or "observer"}) or commerical file name coding
                 (\code{= "com" or "commericial"})
                }
   \item{source}{A character string indicating the source of the data (either \dQuote{Oracle} or \code{NULL}).}
   
   \item{as.data.frame}{A logical value specifying whether the data to return should include all languages and codes 
               or simply a character vector.}
}

\details{
   This function returns a species name for a given species code. The observer 
   and commercial coding option do not refer to the type of coding contained 
   within the data files, but rather the partial species names used in the
   data file names themselves. These are usually three-letter codes (e.g. 
   Amercican plaice is \code{"pla"}). The species codes used within observer and
   commercial files generally follow the standard research vessel coding scheme.
   
   Unknown codes and NA values are returned as NA values. 
   
   This function is a member of a suite of \code{\link[gulf]{Data Code Description Methods}} 
   which translate variable codes into textual descriptions strings. 
}

\value{
   Returns a vector of character strings containing the names for a specified 
   vector of species codes.
   
   If the function argument \code{as.data.frame} is set to \code{TRUE} the function will return a data frame containing the 
   species names (in the selected language) as well as the associated codes.

}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}
        
\seealso{
   \code{\link[gulf]{species.code}},
   \code{\link[gulf]{species.names}},
   \code{\link[gulf]{is.invertebrate}},
   \code{\link[gulf]{is.fish}},
   \code{\link[gulf]{is.skate}},
   \code{\link[gulf]{is.shrimp}}
}

\examples{
   # Return the name for species code 10, which is "Atlantic cod":
   species.str(10) 

   # Return the name for species code 10, which is "Atlantic cod" as well as the code in a data frame format :
   species.str(10, as.data.frame=TRUE) 
   
   # Return the latin names for Atlantic cod and American plaice:
   species.str(c(10, 40), language = "latin")

   # Return the latin names for Atlantic cod and American plaice as well as the code in a data frame format :
   species.str(c(10, 40), language = "latin", as.data.frame=TRUE)
   
   # Return the name for NAFO species code 101:
   species.str(101, coding = "nafo")  
   
   # Return the name for STACAC species code 145:
   species.str(145, coding = "stacac") 
   
   # Return the commercial file prefix for Witch flounder:
   species.str(41, coding = "com") 
}
