#' Biological and Species Categories
#' 
#' @name category
#' 
#' @description Convenience functions to quickly specify and identify biological categories and species groups. 
#'              These are defined by sex, size, maturity or other biological characteristics.
#'
#' @param x Character string(s) specifying a biological categories or an\sQuote{scsbio} object. 
#'          The full syntax of biological strings are presented in the \code{Details} section. =
#' @param sex A scalar specifying the sex category of the default values to be returned if 
#'            \code{code} is numeric. Some accepted values are \code{1}, \sQuote{m} or 
#'            \sQuote{male} for males, or \code{2}, \sQuote{f} or \sQuote{female} for females.
#' @param language A character string specifying the language in which category code descriptions 
#'                 are to be returned. Either english (\code{language = "english"}), 
#'                 french (\code{language = "french"} or \code{language = "français"}).
#' @param parse Logical value specifying whether to parse a category string onto a list containing
#'              the corresponding variable definitions, suitable for direct indexing of biological 
#'              fields. 
#' @param species Numerical species code(s).
#' @param drop Logical values specifying whether to convert index to a vector from a data frame 
#'             when only a single category is specified in \code{is.category.scsbio}.
#' @param ... Further argument (e.g. \code{probability}) passed onto the \code{\link{is.mature.scbio}} function.
#'
#' @return Returns a vector of character strings containing the descriptions for a specified vector 
#'         of category strings or codes.
#'       
#' @details 
#' String are not case sensitive. Spaces or other separators may be included in the string for clarity, 
#' but have no effect on the output. String syntax is as follows:
#'
#' \describe{ 
#'    \item{\code{Sex}}{The first character specifies the sex \sQuote{M} for males and \sQuote{F} for females.}
#'
#'    \item{\code{Maturity}}{The second character(s) is/are optional and specifies the maturity. It may
#'                           be either \sQuote{M} for matures, \sQuote{I} for immatures, \sQuote{P} 
#'                           for primiparous, \sQuote{MULT} for multiparous or \sQuote{SENILE} for seniles.}
#'
#'    \item{\code{Shell condition}}{Shell condition may be specified using the form \sQuote{SCX}, 
#'                                  where \sQuote{X} is a string of shell condition codes.}
#'
#'    \item{\code{Size}}{Carapace width intervals may be specified using the forms \sQuote{GX} 
#'                       (greater than or equal to X) and/or \sQuote{LX} (less than X). In addition,
#'                       forms such as \sQuote{BTXTOY}, where \sQuote{X} and \sQuote{Y} specify 
#'                       lower and upper bounds, respectively. The form \sQuote{FROMXTOY} is also
#'                       acceptable.}
#'   \item{\code{Gonad colour}}{Gonad colour may be specified using the form \sQuote{GNX} where
#'                              \sQuote{X} is a valid colour string. Acceptable strings are \sQuote{W}
#'                              (White), \sQuote{B} (Beige) and \sQuote{O} (Orange).}
#'                              
#'                              
#'   \item{\code{Egg colour}}{Egg colour may be specified using the form \sQuote{EX} where
#'                            \sQuote{X} is a valid colour string. Acceptable strings are 
#'                            \sQuote{LO} (Light Orange), \sQuote{DO} (Dark Orange) and \sQuote{B} 
#'                            (Black).}
#'
#'   \item{\code{Eggs remaining}}{Eggs remaining may be specified using the form \sQuote{ELXXP} 
#'                                or \sQuote{EGXXP}, which correspond to the less-than or 
#'                                greater-or-equal-to forms, respectively. The string \sQuote{X} 
#'                                is a two-digit percentage of eggs remaining. So crab with eggs 
#'                                remaining less than 50\% would be specified using the string 
#'                                \sQuote{EL50P}.}
#'
#'   \item{\code{Missing legs}}{Crabs with missing legs may be targetted by including a \sQuote{ML} 
#'                              string.}
#' }
#' 
#' @examples
#' # Default biological category strings:
#' category()  # All.
#' category(sex = "female") # Female category strings.
#' category(1:10, sex = "male") # First 10 elements of male category strings:
#'
#' # Get long form description for mature males with carapace width larger than 95mm:
#' category("MMG95")
#'
#' # Mature males, large than 95mm with shell condition 3,4 or 5 (spaces are ignored):
#' category("M M G95 SC345")
#'
#' # Multiparous females with orange gonads:
#' category("FMULTGNO")
#'
#' # With multiple entries:
#' category(c("FM", "FMULTGNO", "MG65ML"))
#'    
#' # Parse category strings:
#' category("MM", parse = TRUE)         # Mature males.
#' category("MIG95", parse = TRUE)      # Immature males larger than 95mm.
#' category("FPBT35TO60", parse = TRUE) # Primiparous females between 35 and 60mm.
#'        
#' # Snow crab biological data example:
#' x <- read.scsbio(year = 2012)
#' index <- is.category(x, "TMMG95")  # Mature males greater than 95mm: 
#' 
#' # Get number of observations per category:
#' index <- is.category(x, c("T", "TM", "TF", "TMIL95SC12", "TMMG95", "TFMULT"))
#' apply(index, 2, sum, na.rm = TRUE)
#' 
#' @section Functions:
#' \describe{
#'    \item{\code{category}}{Generic \code{category} method.}
#'    \item{\code{category.default}}{Default \code{category} method.}
#'    \item{\code{category.numeric}}{Return sets of pre-defined biological category strings.}
#'    \item{\code{category.character}}{Return formal and informal description or formal specification of a biological category string.}
#'    \item{\code{is.category}}{Generic \code{is.category} function.}
#'    \item{\code{is.category.scsbio}}{Returns whether an observation belongs to a specified snow crab biological category.}
#'    \item{\code{is.fish}}{Determine whether a species is an vertebrate fish species.}
#'    \item{\code{is.invertebrate}}{Determine whether a species is an invertebrate.}
#'    \item{\code{is.skate}}{Determine whether a fish species is a skate.}
#'    \item{\code{is.shrimp}}{Determine whether a species was categorized as a shrimp historically.}
#' }   
#'    
 
#' @rdname category
#' @export category
category <- function(x, ...) UseMethod("category")

#' @rdname category
#' @export
category.default <- function(x, ...) category.numeric(...) 

#' @rdname category
#' @export
category.numeric <- function(x, sex, group, ...){
   # Parse 'sex' argument:
   if (!missing(sex)){
      sex <- sort(unique(sex))
      if (is.numeric(sex)) sex <- c("male", "female")[sex]
      sex <- match.arg(tolower(sex), c("male", "female"))
   }
   
   # Parse 'group' object:
   if (!missing(group)) group <- match.arg(tolower(as.character(group)), c("other", "all")) 
   
   # Define short male category codes:
   male.str <- c("M", "MM", "MI", "MGE95", "MMGE95",
                 "MMNEW", "MMMEDIUM", "MMOLD",
                 "MMGE95SC12345", "MMGE95SC1", "MMGE95SC2", "MMGE95SC3", "MMGE95SC4", "MMGE95SC5",
                 "MMGE95ML", "MMGE95SC12", "MMGE95SC345",
                 "COMLT102", "COMLT102SC1", "COMLT102SC2", "COMLT102SC3", "COMLT102SC4", "COMLT102SC5",
                 "COMLT102ML", "MMGE102",
                 "MMGE102SC1", "MMGE102SC2", "MMGE102SC3", "MMGE102SC4", "MMGE102SC5", "MMGE102ML",
                 "MIGE95SC12", "MIGE95SC345", "MIGE95",
                 "ML95", "MML95SC12", "MML95SC345", "MML95",
                 "ML35", "MGE35L95", "MIGE56", "MIGE76",
                 "MIL95SC12", "MIL95SC345", "MIL95")

   # Define short male category codes:
   female.str <- c("F", "FM", "FI", # Total females, mature females, immature females.
                   "FIGNW", "TFIGNB", "TFIGNO", # Gonad colour: white, beige, orange
                   "FP",         # Mature, sc12
                   "FPELO",      # Mature, sc12, eggs Light Orange
                   "FPEDO",      # Mature, sc12, eggs Dark Orange
                   "FPEB",       # Mature, sc12, eggs Brown
                   "FPEL50P",    # Mature, sc12, Eggs Remaining < 50%
                   "FPEG50P",    # Mature, sc12, Eggs Remaining > 50%
                   "FMULT",      # Mature, sc34
                   "FMULTELO",   # Mature, sc34, eggs light orange.
                   "FMULTEDO",   # Mature, sc34, eggs dark orange.
                   "FMULTEB",    # Mature, sc34, black eggs.
                   "FMULTEL50P", # Mature, sc34, less than 50% eggs.
                   "FMULTEG50P", # Mature, sc34, more than 50% eggs.
                   "FSENILE")    # Mature, sc5

   # Define other categories:
   other.male.str <- c('MIGE34L45SC1','MIGE34L45SC2','MIGE34L45SC3','MIGE34L45SC4',
                       'MIGE34L45SC5','MIGE34L45','MIGE45L56SC1','MIGE45L56SC2',
                       'MIGE45L56SC3','MIGE45L56SC4','MIGE45L56SC5','MIGE45L56',
                       'MIGE56L69SC1','MIGE56L69SC2','MIGE56L69SC3','MIGE56L69SC4',
                       'MIGE56L69SC5','MIGE56L69','MIGE69L83SC1','MIGE69L83SC2',
                       'MIGE69L83SC3','MIGE69L83SC4','MIGE69L83SC5','MIGE69L83',
                       'MIGE83L98SC1','MIGE83L98SC2','MIGE83L98SC3','MIGE83L98SC4',
                       'MIGE83L98SC5','MIGE83L98','MIGE98L115SC1','MIGE98L115SC2',
                       'MIGE98L115SC3','MIGE98L115SC4','MIGE98L115SC5','MIGE98L115',
                       'MMGE41L53SC1','MMGE41L53SC2','MMGE41L53SC3','MMGE41L53SC4',
                       'MMGE53L65SC1','MMGE53L65SC2','MMGE53L65SC3','MMGE53L65SC4',
                       'MMGE53L65SC5','MMGE53L65','MMGE65L79SC1','MMGE65L79SC2',
                       'MMGE65L79SC3','MMGE65L79SC4','MMGE65L79SC5','MMGE65L79',
                       'MMGE79L95SC1','MMGE79L95SC2','MMGE79L95SC3','MMGE79L95SC4',
                       'MMGE79L95SC5','MMGE79L95','MMGE95L110SC1','MMGE95L110SC2',
                       'MMGE95L110SC3','MMGE95L110SC4','MMGE95L110SC5','MMGE95L110',
                       'MMGE110L128SC1','MMGE110L128SC2','MMGE110L128SC3','MMGE110L128SC4',
                       'MMGE110L128SC5','MMGE110L128')

   other.female.str <- paste0("F", substr(other.male.str, 2, nchar(other.male.str)))

   # Define numeric code values:
   str <- NULL
   if (missing(group)){
      if (missing(sex)){
         str <- c(male.str, female.str)
      }else{
         if ("male" %in% sex) str <- c(str,  male.str)
         if ("female" %in% sex) str <- c(str,  female.str)         
      }
   }else{
      if (missing(sex)){
         if (group == "other") str <- c(other.male.str, other.female.str) 
      }else{
         if (("male" %in% sex) & (group == "other")) str <- c(str,  male.str)
         if (("male" %in% sex) & (group == "all")) str <- c(str,  male.str, other.male.str)
         if (("female" %in% sex) & (group == "other")) str <- c(str,  other.female.str)
         if (("female" %in% sex) & (group == "all")) str <- c(str,  female.str, other.female.str)         
      }
   }

   if (missing(x)) return(str) else return(str[x])
}

# Internal biological string parsing function:
parse.category <- function(x){
   # Initialize category variables:
   sex <- NULL      # Sex.
   maturity <- NULL # Maturity.
   cw <- c(NA, NA)  # Carapace width.
   cwf <- c(FALSE, FALSE) # Carapace width equality flag.
   sc <- NULL       # Shell condition.
   ml <- NULL       # Missing legs.
   gc <- NULL       # Gonad colour.                 
   er <- c(NA, NA)  # Eggs remaining interval.
   ec <- NULL       # Egg colour.
   fun <- NULL      # Function category.
   
   # Remove separators:
   x <- gsub("[ ,;-_]", "", tolower(x))
   
   # Remove leading "T":
   x <- gsub("^t*", "", x)
   
   # Check for literal shell condition specification:
   if (length(grep("new", x)) > 0){
      fun <- c(fun, "is.new.shell")
      x <- gsub("new", "", x)
   }
   x <- gsub("med", "medium", x)
   x <- gsub("medium", "sc34", x)
   x <- gsub("old", "sc5", x)

   x <- gsub("mult", "multiparous", x)
   if (length(grep("multiparous", x)) > 0){
      fun <- c(fun, "is.multiparous")
      x <- gsub("multiparous", "", x)
   }
   if (length(grep("senile", x)) == 0) x <- gsub("sen", "senile", x)
   if (length(grep("senile", x)) > 0){
      fun <- c(fun, "is.senile")
      x <- gsub("senile", "", x)
   }
   if (length(grep("soft", x)) > 0){
      fun <- c(fun, "is.soft.shell")
      x <- gsub("soft", "", x)
   }
   if (length(grep("hard", x)) > 0){
      fun <- c(fun, "is.hard.shell")
      x <- gsub("hard", "", x)
   }
   
   # Commercial crab.      
   if (length(grep("com", x)) > 0){
      x <- gsub("com", "", x) 
      x <- paste0("mmge95", x)
   }
   
   # Substitute 'bt' for 'g':
   x <- gsub("bt", "ge", x)
   
   # Substitute 'from' for 'ge':
   x <- gsub("from", "ge", x)
   
   # Substitute 'to' for 'l':
   x <- gsub("to", "l", x)
   
   # Check leading 'sex' character:
   if (x == "") x <- "a"
   if (!(substr(x, 1, 1) %in% c("m", "f", "a"))) x <- paste0("a", x)
   
   # Determine sex category:
   x <- gsub("^a*", "", x)
   if (substr(x, 1, 1) == "a"){ x <- substr(x, 2, nchar(x))}
   if (substr(x, 1, 1) == "m"){ sex <- 1; x <- substr(x, 2, nchar(x))}
   if (substr(x, 1, 1) == "f"){ sex <- 2; x <- substr(x, 2, nchar(x))}
   
   # Determine maturity:
   x <- gsub("^a*", "", x)
   
   if (substr(x, 1, 1) == "m"){ maturity <- 1; x <- substr(x, 2, nchar(x)) }
   if (substr(x, 1, 1) == "i"){ maturity <- 2; x <- substr(x, 2, nchar(x)) }
   if (substr(x, 1, 1) == "p"){
      fun <- "is.primiparous"
      x <- substr(x, 2, nchar(x))
   }
   
   # Parse gonad colour:
   index <- regexpr("gn", x)
   if (index > 0){
      gc <- substr(x, index+2, index+2)
      if (!(gc %in% c("w", "b", "o")))
         stop("Gonad colour character must be either 'w' (white), 'b' (beige) or 'o' (orange).")
      if (gc == "w") gc <- 1
      if (gc == "b") gc <- 2
      if (gc == "o") gc <- 3
      x <- gsub(substr(x, index, index+2), "", x)
   }
   
   # Parse egg colour:
   index <- regexpr("e(lo|do|b)", x)
   if (index > 0){
      len <- attr(index, "match.length")
      ec <- substr(x, index+1, index+len-1)
      if (ec == "lo") ec <- 1
      if (ec == "do") ec <- 2
      if (ec == "b")  ec <- 3
      x <- gsub(substr(x, index, index+len-1), "", x)
   }
   
   # Parse eggs remaining:
   index <- regexpr("e(l|g)\\d\\dp", x)  # "ELXXP" or "EGXXP".
   if (index > 0){
      len <- attr(index, "match.length")
      str <- substr(x, index+1, index+1)
      if (str == "g"){
         er[1] <- as.numeric(substr(x, index+2, index+3))
         if (er[1] >= 100) temp <- 4
         if ((er[1] >= 75) & (er[1] <= 99)) temp <- 3
         if ((er[1] >= 50) & (er[1] <= 74)) temp <- 2
         if ((er[1] >= 1) & (er[1] <= 49)) temp <- 1
         if (er[1] == 0) temp <- 0
         er <- temp:4
      }
      if (str == "l"){
         er[2] <- as.numeric(substr(x, index+2, index+3))
         if (er[2] >= 100) temp <- 3
         if ((er[2] > 75) & (er[2] <= 99)) temp <- 3
         if ((er[2] > 50) & (er[2] <= 75)) temp <- 2
         if ((er[2] > 1) & (er[2] <= 50)) temp <- 1
         if (er[2] <= 1) temp <- 0
         er <- sort(temp:0)
      }
      x <- gsub(substr(x, index, index+len-1), "", x)
   }
   
   # Parse shell condition:
   index <- regexpr("sc1?2?3?4?5?", x)
   if (index > 0){
      len <- attr(index, "match.length")
      str <- substr(x, index, index+len-1)
      for (i in 1:(len-2)) sc <- c(sc, as.numeric(substr(str, i+2, i+2)))
      x <- gsub(str, "", x)
   }
   
   # Substitute trailing 'ml' (missing legs) for 'x':
   if ((substr(x, nchar(x)-1, nchar(x))) == "ml"){
      x <- paste(substr(x, 1, nchar(x)-2), "x", sep = "")
   }
   
   # Parse missing legs argument:
   if (length(grep("x", x)) > 0){
      x <- gsub("x", "", x)
      ml <- TRUE
   }
   
   # Determine lower carapace width bound:
   i <- regexpr("g[te]*[0-9]+", x)
   if (i > 0){
      str <- substr(x, i, i + attr(i, "match.length") - 1)
      val <- as.numeric(gsub("[a-z]+", "", str))
      op <- gsub("[0-9]+", "", str) 
      if (substr(op, 2, 2) == "e") cwf[1] <- TRUE
      cw[1] <- val
      x <- gsub(str, "", x)        
   }
   
   # Determine upper carapace width bound:
   i <- regexpr("l[te]*[0-9]+", x)
   if (i > 0){
      str <- substr(x, i, i + attr(i, "match.length") - 1)
      val <- as.numeric(gsub("[a-z]+", "", str))
      op <- gsub("[0-9]+", "", str) 
      if (substr(op, 2, 2) == "e") cwf[2] <- TRUE
      cw[2] <- val
      x <- gsub(str, "", x)        
   }
   
   # Set intervals to NULL if undefined:
   if (all(is.na(er))) er <- NULL
   if (all(is.na(cw))) cw <- NULL
   
   # Define result structure:
   result <- list(sex = sex,
                  maturity = maturity,
                  carapace.width = cw,
                  carapace.width.flag = cwf,
                  shell.condition = sc,
                  missing.legs = ml,
                  gonad.colour = gc,
                  eggs.remaining = er,
                  egg.colour = ec,
                  fun = fun)
   
   return(result)
}

# Internal string category description function:
describe.category <- function(x, language = "english", symbols = TRUE, simplify = TRUE, ...){
   # Parse 'language' argument:
   language <- match.arg(tolower(language), c("english", "french", "français"))
   if (language == "français") language <- "french"

   # Initilize result string:
   str <- NULL
      
   # Parse character string 'x' argument:
   if (length(x) == 0) return(str)
   if (length(x) > 1){
      for (i in 1:length(x)) str <- c(str, describe.category(x[i], language = language, symbols = symbols, ...))
   }else{
      # Extract formal specifications:
      r <- parse.category(x)

      # Check for special cases:
      if (simplify){
         # Commercial crab:
         index <- (r$maturity == 1) & (r$sex == 1) & (r$carapace.width[1] == 95) & (r$carapace.width.flag[1])
         if (length(index) == 1){
            if (index){
               if (language == "english") str <- paste(str, "commercial crab ")
               if (language == "french") str <- paste(str, "crabes commerciaux ")
               r$sex <- NULL
               r$maturity <- NULL
               r$carapace.width[1] <- NA
               r$carapace.width.flag[1] <- NA
            }
         }
         
         # Skip-moulters crab:
         index <- (r$maturity == 2) & all(r$shell.condition %in% 3:5)
         if (length(index) == 1){
            if (index){
               if (language == "english") str <- paste(str, "skip-moulter ")
               if (language == "french") str <- paste(str, "saute-mue ")
               r$maturity <- NULL
               r$shell.condition <- NULL
            }
         }
         
         # Sub-legals:
         index <- (r$sex == 1) & (r$carapace.width[2] == 95)
         index <- index[!is.na(index)]
         if (length(index) == 1){
            if (index){
               if (language == "english") str <- paste(str, "sub-legal ")
               if (language == "french") str <- paste(str, "sous-legaux ")
               r$carapace.width[2] <- NA
            }
         } 
         
         # Legal-sized:
         index <- (r$sex == 1) & (r$carapace.width[1] == 95) & (r$carapace.width.flag[1])
         index <- index[!is.na(index)]
         if (length(index) == 1){
            if (index){
               if (language == "english") str <- paste(str, "legal-sized ")
               if (language == "french") str <- paste(str, "taille légal")
               r$carapace.width[1] <- NA
            }
         }           
      }

      # Maturity:
      if (!is.null(r$maturity)){
         if (r$maturity == 1) str <- paste0(str, "mature")
         if (r$maturity == 2) str <- paste0(str, "immature")
      }

      # Extract function string:
      if (!is.null(r$fun)){
         tmp <- gsub("^is[.]", "", r$fun)
         tmp <- gsub("[.]", " ", tmp)
         str <- paste(str, tmp)
      } 

      # Sex:
      if (!is.null(r$sex)){
         if (r$sex == 1){
            if (language == "english") str <- paste(str, "males")
            if (language == "french")  str <- paste("mâles", str)
         }
         if (r$sex == 2){
            if (language == "english") str <- paste(str, "females")
            if (language == "french")  str <- paste("femelles", str)
         }
      }
      str <- paste(str, ",", sep = "")

      # Carapace width:
      if (language == "english") cw.str <- "cw"
      if (language == "french") cw.str <- "lc"
      if (symbols){
         signs <- list(c(">=", ">"), c("<=", "<"))
      }else{
         if (language == "english") signs <- list(c("larger or equal to", "larger than"), c("smaller or equal to", "smaller than"))
         if (language == "french") signs <- list(c("plus grand ou egal à", "plus grand que"), c("plus petit ou egal à", "plus grand que"))
      } 
      if (!is.null(r$carapace.width)){
         for (i in 1:2){
            if (!is.na(r$carapace.width[i])){
               str <- paste(str, cw.str, signs[[i]][(2-r$carapace.width.flag[i])], r$carapace.width[i])
               str <- paste0(str, "mm,")
            }
         }
      }

      # Shell condition:
      if (!is.null(r$shell.condition)){
         if (language == "english") str <- paste(str, " shell condition ", paste(r$shell.condition, collapse = ","), ",", sep = "")
         if (language == "french")  str <- paste(str, " condition de carapace ", paste(r$shell.condition, collapse = ","), ",", sep = "")
      }

      # Missing legs:
      if (!is.null(r$missing.legs)){
         if (r$missing.legs){
            if (language == "english") str <- paste(str, "with missing legs,")
            if (language == "french")  str <- paste(str, "avec pattes manquantes,")
         }
      }

      # Gonad colour:
      if (!is.null(r$gonad.colour)){
         if (language == "english"){
            if (r$gonad.colour == 1) str <- paste(str, "white gonads,")
            if (r$gonad.colour == 2) str <- paste(str, "beige gonads,")
            if (r$gonad.colour == 3) str <- paste(str, "orange gonads,")
         }
         if (language == "french"){
            if (r$gonad.colour == 1) str <- paste(str, "gonade blanche,")
            if (r$gonad.colour == 2) str <- paste(str, "gonade beige,")
            if (r$gonad.colour == 3) str <- paste(str, "gonade orange,")
         }
      }

      # Eggs remaining:
      if (!is.null(r$eggs.remaining)){
         if (language == "english") str <- paste(str, " eggs remaining = ", paste(r$eggs.remaining, collapse = ","), ",", sep = "")
         if (language == "french")  str <- paste(str, " oeufs restants = ", paste(r$eggs.remaining, collapse = ","), ",", sep = "")
      }

      # Egg colour:
      if (!is.null(r$egg.colour)){
         if (language == "english"){
            if (r$egg.colour == 1) str <- paste(str, " light orange eggs,", sep = "")
            if (r$egg.colour == 2) str <- paste(str, " dark orange eggs,", sep = "")
            if (r$egg.colour == 3)  str <- paste(str, " brown eggs,", sep = "")
         }
         if (language == "french"){
            if (r$egg.colour == 1) str <- paste(str, " oeufs orange clairs,", sep = "")
            if (r$egg.colour == 2) str <- paste(str, " oeufs orange foncés,", sep = "")
            if (r$egg.colour == 3)  str <- paste(str, " oeufs bruns,", sep = "")
         }
      }

      if (language == "french"){
         str <- gsub("multiparous", "multipares", str)
         str <- gsub("primiparous", "primipares", str)
         str <- gsub("immatures*", "immatures", str)
         str <- gsub("matures*", "matures", str)
         str <- gsub("seniles*", "seniles", str)
      }
      
      # Remove leading space & trailing comma:
      str <- gsub(",$", "", deblank(str))
      str <- gsub(" +", " ", str)
      str <- gsub(" ,", ",", str)
      
      # Capitalize first letter:
      str <- paste(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)), sep = "")
   }
   
   return(str)
}

#' @rdname category
#' @export
category.character <- function(x, parse = FALSE, ...){
   args <- list(...)
   if (any(c("sex", "group") %in% names(args))) return(category.numeric(...))
   if (parse) return(parse.category(x))
   return(describe.category(x, ...))
}

#' @rdname category
#' @export is.category
is.category <- function(x, ...) UseMethod("is.category")

#' @rdname category
#' @export
is.category.scsbio <- function(x, category, drop = TRUE, ...){
   # Check whether crab belongs to a specified category:

   if (length(category) == 1){
      # Initialize result vector:
      index <- rep(TRUE, dim(x)[1])
   
      # Parse category string:
      t <- parse.category(category)
   
      # Build logical vector:
      if (!is.null(t$sex)) index <- index & (x$sex == t$sex)
      if (!is.null(t$carapace.width)){
         if (!is.na(t$carapace.width[1])){
            if (t$carapace.width.flag[1]){
               index <- index & (x$carapace.width >= t$carapace.width[1])
            }else{
               index <- index & (x$carapace.width > t$carapace.width[1])
            }
         }
         if (!is.na(t$carapace.width[2])){
            if (t$carapace.width.flag[2]){ 
               index <- index & (x$carapace.width <= t$carapace.width[2])
            }else{
               index <- index & (x$carapace.width < t$carapace.width[2])
            }
         }
      }

      if (!is.null(t$shell.condition)) index <- index & (x$shell.condition %in% t$shell.condition)
      if (!is.null(t$missing.legs)) if (t$missing.legs) index <- index & is.missing.legs(x)
      if (!is.null(t$gonad.colour)) index <- index & (x$gonad.colour == t$gonad.colour)
      if (!is.null(t$eggs.remaining)){
         if (!any(is.na(t$eggs.remaining))) index <- index & (x$eggs.remaining %in% t$eggs.remaining)
      }
      if (!is.null(t$egg.colour)) index <- index & (x$egg.colour == t$egg.colour)
      ii <- index
      if (!is.null(t$maturity)) index <- index * ifelse(t$maturity == rep(1, dim(x)[1]), is.mature(x, ...), 1-is.mature(x, ...))
      if (!is.null(t$fun)) for (i in 1:length(t$fun)) index <- index * eval(parse(text = paste0(t$fun[i], "(x, ...)")))
      index[!ii] <- 0
   }else{
      index <- NULL
      for (i in 1:length(category)){
         index <- cbind(index, is.category(x, category[i], ...))
      }
      dimnames(index) <- list(NULL, category)
   }
   
   # Convert to logical if there are no fractions:
   if (all((index[!is.na(index)] %% 1) == 0)) index <- (index == 1)
  
   if (!drop & is.vector(index)){
      index <- data.frame(index)
      names(index) <- category
   }

   return(index)
}

#' @rdname category
#' @export is.fish
is.fish <- function(species) return(species %in% c(1:899, 940, 950, 960, 965))

#' @rdname category
#' @export is.invertebrate
is.invertebrate <- function(species) return((species >= 1700) & (species < 9000))

#' @rdname category
#' @export is.skate
is.skate <- function(species) return(species %in% c(200:212, 217, 219))

#' @rdname category
#' @export is.shrimp
is.shrimp <- function(species){
   v <- species %in% c(2211:2213, 2220:2222, 2230, 2312, 2313, 2315, 2316, 2319, 
                       2331:2333, 2411, 2414, 2415, 2417, 2420, 2421)
   return(v)
}
