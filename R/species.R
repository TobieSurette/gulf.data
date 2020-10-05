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
#'               American Fisheries Organization (NAFO) coding \code{\sQuote{nafo}}, and the observer and commercial 
#'               file name prefix coding \code{\sQuote{observer}} and \code{\sQuote{commercial}}.
#' @param oracle Logical value specifying whether to use the Oracle database as a source. Default is \code{FALSE}.
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
#' species(101, coding = "nafo") 
#' species(145, coding = "stacac") 
#' species(41, coding = "com") # Commercial file prefix for Witch flounder:
#' 
#' # Search for two key words:
#' species("cod atl")
#' 
#' # Search for two key words:
#' v <- species(c("cod atl", "witch", "wint fl"))
#' species(v) # Extract names.
#' 
#' # Species with the word "Atlantic" in their name:
#' species(species("atlantic"))
#' 
#' # Extract entire species tables:
#' species()
#' species.foreign()
#' 
#' @seealso \code{\link{data}}
#' 
 
#' @export 
species <- function(x, ...) UseMethod("species") # Generic function.

#' @rdname species
#' @export species.foreign
species.foreign <- function(x, ...){
   file <- system.file("extdata", "species.foreign.csv", package = "gulf.data")
   x <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   return(x)
}

#' @rdname species
#' @export
species.default <- function(x, ...){
   if (missing(x)){
      file <- system.file("extdata", "species.csv", package = "gulf.data")
      x <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
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
species.numeric <- function(x, language = "english", coding = "rv", source, ...){
   # Parse input arguments:
   language <- match.arg(tolower(language), c("english", "french", "latin" , "any"))
   coding <-  match.arg(tolower(coding), c("rv", "research", "commercial", "observer", "stacac", "nafo"))
   if (coding == "research") coding <- "rv"

   # Define language table:
   language.table <- data.frame(name   = c("english", "french", "latin"),
                                oracle = c("COMM", "COMM_FR", "SPEC"),
                                prefix = c("en", "fr", "latin"), 
                                stringsAsFactors = FALSE)
   index <- match(language, language.table$name) 
   if (!is.na(index)) language.table <- language.table[index, ]
   
   # Initialize result variable:
   result <- NULL
   
   # Gulf research survey codes:
   if (coding == "rv"){    
      if (missing(source)){
         tab <- species()
         result <- tab[match(x, tab$code), paste0("name_", language.table$prefix)]
      }else{ 
         if (source == "oracle"){ 
            fieldtoreturn <- paste0("l.list CODE, ", paste0(language.table$oracle, collapse = ", "))

            channel = oracle.open()
            
            # This query will return the species names in the same order as the code vector (unsorted)       
            ux <- unique(x[!is.na(code)])
            if (length(ux) > 500){
               query = paste0(" SELECT * FROM GSSPECIES")      
               result = oracle.query(channel, query)
               result <- result[match(ux, result$CODE), oracle.lang]
            }else{
               # Translate code vector into character string for Oracle query
               query = paste("SELECT ",fieldtoreturn," 
                             FROM GSSPECIES s
                             ,(select rownum r, list from (SELECT trim(REGEXP_SUBSTR('", paste0(ux, collapse = ","), "', '[^,]+', 1, LEVEL)) AS list FROM dual CONNECT BY INSTR('",code.str,"', ',', 1, LEVEL-1) > 0)) l
                             WHERE s.CODE(+) = l.list
                             ORDER BY l.r", sep = "")
               
               result <- oracle.query(channel, query)
               result <- result[match(x, ux), ]
            }
            oracle.close()
         }  
      }
   }
  
   # Define species names and STACAC and NAFO codes:
   if (coding %in% c("stacac", "nafo")){
       tab <- species.foreign()
       result <- tab[match(x, tab[, coding]), paste0("name_", language.table$prefix)]
   }

   # Return species file strings for observer data species codes:
   if (coding %in% c("commercial", "observer")){
     tab <- data.frame(code = c(10,11, 12, 16, 23, 30, 31, 40, 41, 42, 43, 60, 64, 70, 220, 720, 2211),
                       name = c("cod","had", "wh", "pol", "red", "hal", "turb", "pla", "wit", "yel", "wf", "her", "cap", "mack","dog", "sau", "shri"),
                       stringsAsFactors = FALSE)
     result <- tab$name[match(x, tab$code)]
   }   

   return(result)
}

#' @rdname species
#' @export
species.character <- function(x, language = "english", input = "stacac", output = "rv", ...){
   # Languages:
   language <- match.arg(tolower(language), c("", "any", "english", "french", "latin"))
   if (language == "any") language <- ""
   
   # Input coding:
   input <- match.arg(tolower(input), c("rv", "research", "nafo", "stacac"))
   if (input == "research") input <- "rv"
   
   # Output coding:
   output <- match.arg(tolower(output), c("rv", "research", "nafo", "stacac"))
   if (output == "research") output <- "rv"
   
   # Numeric species code match:
   if (is.numeric(x)){
      # Read species table:
      data <- species.foreign()                       
      return(data[match(x, data[, input]), output])
   }                                                                                                              
  
   # Character species string match:
   if (is.character(x)){
      ux <- unique(x[!is.na(x) & (x != "")])  
      
      # Species table:
      if (output != "rv"){
         tab <- species.foreign() 
      }else{
         tab <- species()
         output <- "code"        
      } 
      
      # Define data fields to search:
      if (language == "english") var <- "name_en"
      if (language == "french")  var <- "name_fr"
      if (language == "latin")   var <- "name_latin"
      if (language == "")        var <- names(x)[grep("^name", names(x))]
      
      # Loop over key words:
      v <- rep(list(NULL), length(x))
      for (i in 1:length(ux)){
         words <- tolower(strsplit(ux[i], "[ ,;]")[[1]])
         for (j in 1:length(var)){
            r <- 1:nrow(tab)
            for (k in 1:length(words)) r <- intersect(r, grep(words[k], tolower(tab[, var[j]])))
         }
         v[[i]] <- tab[r, output]
      }
      
      # Drop list:
      if (length(v) == 1) v <- v[[1]]
      
      return(v)
   }
}

#' @export species.str
species.str <- species.numeric

#' @export species.code
species.code <- species.character

