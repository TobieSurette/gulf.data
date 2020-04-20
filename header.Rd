\name{header}

\alias{header}
\alias{header.minilog}
\alias{header.star.oddi}
\alias{header.netmind}
\alias{header.esonar}

\title{Extract Header Data}

\description{
   Returns the header data associated with an object, such as acoustic monitoring
   and depth-temperature probes. Data files often have header information which 
   are stored in an object's attributes.
}

\usage{
   # Generic 'header' method:
   header(x, ...)
   
   # 'minilog' 'header' method:
   header(x)
   
   # 'netmind' 'header' method:
   header(x)
   
   # 'esonar' 'header' method:
   header(x)
}

\arguments{
   \item{x}{An object.}
}

\value{
   A named list is returned containing the header information
   associated with an object.
}

\seealso{
   \code{\link[gulf]{minilog class}}, 
   \code{\link[gulf]{netmind class}}, 
   \code{\link[gulf]{esonar class}}, 
   \code{\link[gulf]{star.oddi class}}, 
}

\keyword{IO}

\examples{
    # Read the first minilog file for the 2010 September survey and extract header info:
    file <- minilog.file.str(year = 2010)
    x <- read.minilog(file[1])
    y <- header(x)
     
    # Read the first esonar file for the 2013 snow crab survey and extract header info:
    file <- esonar.file.str(year = 2013)
    x <- read.esonar(file[1])
    y <- header(x)
    
    # Read the first netmind file for the 2010 snow crab survey and extract header info:
    file <- netmind.file.str(year = 2010)
    x <- read.netmind(file[1])
    y <- header(x)        
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}

