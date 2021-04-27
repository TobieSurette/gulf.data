#' Species Names or Code
#' 
#' @name species
#' 
#' @description Converts species codes to corresponding species names and vice versa.
#' 
#' @param x Numerical species code(s) or species name(s).
#' @param language Character string specifying the language in which species names are to be returned. Either 
#'                 \code{\sQuote{english}}, \code{\sQuote{french}} or \code{\sQuote{latin}}. Note that not all 
#'                 language and coding combinations are available.
#' @param coding,input,output Character string specifying the type of species coding being used. Options are the standard 
#'               research survey coding \code{\sQuote{rv}} (or equivalently \code{\sQuote{research}}), the Statistical
#'               Coordinating Committee for the Atlantic Coast (STACAC) coding \code{\sQuote{stacac}}, the North 
#'               American Fisheries Organization (NAFO) coding \code{\sQuote{nafo}}, World Repository of Marine Species (WoRMS) 
#'               coding \code{\sQuote{WoRMS}} or \code{\sQuote{aphiaID}},  and the observer and commercial 
#'               file name prefix coding \code{\sQuote{observer}} and \code{\sQuote{commercial}}.
#' 
#' @return Vector of character strings containing species names. For multiple species name searches, a list of
#'         search results are returned.
#'
#' @section Functions:
#' \describe{
#'   \item{\code{species}}{Generic \code{species} method.}
#'   \item{\code{species.foreign}}{Load foreign species code table.}
#'   \item{\code{species.default}}{Default \code{species} method.}
#'   \item{\code{species.list}}{Apply \code{species} function to list elements.}
#'   \item{\code{species.numeric}}{Convert numeric species code(s) to corresponding species name(s).}
#'   \item{\code{species.character}}{Find numeric species code(s).}
#'   \item{\code{species.str}}{Alias of \code{species.numeric}.}
#'   \item{\code{species.code}}{Alias of \code{species.character}.}
#' }
#' 
#' @examples
#' # Miscellaneous queries:
#' species(10)  # Code for "Atlantic cod".
#' species(c(10, 12, 40:43), language = "latin") # Latin names for common species.
#' species(101, coding = "nafo")     # NAFO species codes.
#' species(145, coding = "stacac")   # STACAC species codes.
#' species(41, coding = "com")       # Commercial file prefix for Witch flounder.
#' species(126436, coding = "WoRMS") # WoRMS species codes.
#' 
#' # Converting between species codes:
#' species(10, output = "WoRMS") # DFO to WoRMS coding.
#' species(126436, coding = "worms", output = "code")
#' species(10, output = "stacac")
#' species(10, output = "stacac")
#' species(10, output = "nafo")
#' species(101, coding = "nafo", output = "stacac")
#'
#' # Search for two key words:
#' species("cod atl")
#' 
#' # Search for two key words:
#' v <- species(c("cod atl", "witch", "wint fl"))
#' species(v) # Extract names.
#' 
#' # Species entries with the word "Atlantic" in their name:
#' species(species("atlantic"))
#' 
#' # Extract entire species tables:
#' species()
#' species.foreign()
#' 
#' @seealso \code{\link{taxon}}, \code{\link{data}}
#' 
 
#' @export 
species <- function(x, ...) UseMethod("species") # Generic function.

#' @rdname species
#' @export species.foreign
species.foreign <- function(x, ...){
   file <- system.file("extdata", "species.foreign.tab", package = "gulf.data")
   x <- read.table(file = file, sep = "\t", header = TRUE, fileEncoding = "utf-8", stringsAsFactors = FALSE)
   return(x)
}

#' @rdname species
#' @export
species.default <- function(x, ...){
   if (missing(x)){
      file <- system.file("extdata", "species.tab", package = "gulf.data")
      x <- read.table(file = file, sep = "\t", header = TRUE, fileEncoding = "utf-8", stringsAsFactors = FALSE)
      return(x)
   }
   if (all(is.na(x))) return(x)
   if (is.factor(x)) return(species(as.character(x)))
}

#' @rdname species
#' @export
species.list <- function(x, ...) return(lapply(x, species))

#' @rdname species
#' @export
species.numeric <- function(x, language = "english", coding = "code", output, ...){
   # Parse input arguments:
   language <- language(language)
   
   # Parse input coding: 
   coding <- gulf.utils::deblank(gsub("[._]", "", tolower(coding)))
   coding <- match.arg(coding, c("code", "rv", "research", "nafo", "stacac", "aphiaid", "worms", "commercial", "observer"))
   if (coding %in% c("rv", "research")) coding <- "code"
   if (coding %in% c("worms"))          coding <- "aphiaid"
   if (coding == "aphiaid")             coding <- "aphia.id"
   
   # Initialize result variable:
   result <- NULL
   
   # Load species code tables:
   if (coding %in% c("code", "aphia.id")) tab <- species()          # Gulf research survey codes.
   if (coding %in% c("stacac", "nafo"))   tab <- species.foreign()  # STACAC and NAFO species codes:
   
   # Parse output coding: 
   if (!missing(output)){
      output <- gulf.utils::deblank(gsub("[._]", "", tolower(output)))
      output <- match.arg(output, c("code", "rv", "research", "nafo", "stacac", "aphiaid", "worms"))
      if (output %in% c("rv", "research")) output <- "code"
      if (output %in% c("worms"))          output <- "aphiaid"
      if (output == "aphiaid")             output <- "aphia.id"
      
      if (output %in% c("stacac", "nafo")) tab <- species.foreign()  # STACAC and NAFO species codes:
   }else{
      output <- language
   }
   
   # Return species file strings for observer and commercial data species codes:
   if (coding %in% c("commercial", "observer")){
      tab <- data.frame(code = c(10,11, 12, 16, 23, 30, 31, 40, 41, 42, 43, 60, 64, 70, 220, 720, 2211),
                        name = c("cod","had", "wh", "pol", "red", "hal", "turb", "pla", "wit", "yel", "wf", "her", "cap", "mack","dog", "sau", "shri"),
                        stringsAsFactors = FALSE)
      coding <- "code"
      output <- "name"
   }   

   # Look-up species codes:
   result <- tab[match(x, tab[, coding]), output] 
   
   return(result)
}

#' @rdname species
#' @export
species.character <- function(x, language = "english", coding = "code", drop = TRUE, ...){
   # Languages:
   language <- language(language)
   if (language == "any") language <- ""
   
   # Input coding:
   coding <- gulf.utils::deblank(gsub("[._]", "", tolower(coding)))
   coding <- match.arg(coding, c("code", "rv", "research", "nafo", "stacac", "aphiaid", "worms"))
   if (coding %in% c("rv", "research")) coding <- "code"
   if (coding %in% c("worms"))          coding <- "aphiaid"
   if (coding == "aphiaid")             coding <- "aphia.id"
   
   # Character species string match:
   ux <- unique(x[!is.na(x) & (x != "")])  
      
   # Species table:
   if (coding %in% c("code", "aphia.id")) tab <- species() else tab <- species.foreign()

   # Loop over key words:
   v <- rep(list(NULL), length(ux))
   for (i in 1:length(ux)){
      words <- tolower(strsplit(ux[i], "[ ,;]")[[1]])
      for (j in 1:length(language)){
         r <- 1:nrow(tab)
         for (k in 1:length(words)) r <- intersect(r, grep(words[k], tolower(tab[, language[j]])))
         ix <- grep("eggs", tolower(tab[r, language[j]]))
         ix <- c(ix, grep("larva", tolower(tab[r, language[j]])))
         if (length(ix) > 0){
            ix <- sort(ix)
            r <- c(r[-ix], r[ix])
         }
      }
      v[[i]] <- tab[r, coding]
      v[[i]] <- v[[i]][!is.na(v[[i]])]
   }
   v <- v[match(x, ux)]
      
   # Drop list:
   if (drop & (length(v) == 1)) v <- v[[1]]
      
   return(v)
}

#' @export species.str
species.str <- species.numeric

#' @export species.code
species.code <- species.character

