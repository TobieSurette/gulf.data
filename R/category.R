#' Biological Categories
#' 
#' @description This function checks whether a sampled specimen belongs to a specified biological category.
#' 
#' @param x An \sQuote{scbio} object.
#' @param category Character string(s) specifying bioligical categories. The syntax of these 
#'                 strings is presented in \code{\link[gulf]{category.str}}. 
#'                 
#' @param ... Further argument (e.g. \code{probability}) passed onto the
#' \code{\link[gulf]{is.mature.scbio}} function.
#' @details 
#' 
#' #'
#' String syntax is as follows:
#'
#' \itemize{ \item A leading "T" (i.e. \sQuote{Total}) may be included, though
#' not necessary as it serves no function.
#'
#' \item The first character specifies the sex \sQuote{M} for males and
#' \sQuote{F} for females.
#'
#' \item The second character(s) is/are optional and specifies the maturity.
#' It may be either \sQuote{M} for matures, \sQuote{I} for immatures,
#' \sQuote{P} for primiparous, \sQuote{MULT} for multiparous or \sQuote{SENILE}
#' for seniles.
#'
#' \item Shell condition may be specified using the form \sQuote{SCX}, where
#' \sQuote{X} is a string of shell condition codes.
#'
#' \item Carapace width intervals may be specified using the forms \sQuote{GX}
#' (greater than or equal to X) and/or \sQuote{LX} (less than X). In addition
#' forms such as \sQuote{BTXTOY}, where \sQuote{X} and \sQuote{Y} specify lower
#' and upper bounds, respectively. The form \sQuote{FROMXTOY} is also
#' acceptable.
#'
#' \item Gonad colour may be specified using the form \sQuote{GNX} where
#' \sQuote{X} is a valid colour string. Acceptable strings are \sQuote{W}
#' (White), \sQuote{B} (Beige) and \sQuote{O} (Orange).
#'
#' \item Egg colour may be specified using the form \sQuote{EX} where
#' \sQuote{X} is a valid colour string. Acceptable strings are \sQuote{LO}
#' (Light Orange), \sQuote{DO} (Dark Orange) and \sQuote{B} (Black).
#'
#' \item Eggs remaining may be specified using the form \sQuote{ELXXP} or
#' \sQuote{EGXXP}, which correspond to the less-than or greater-or-equal-to
#' forms, respectively. The string \sQuote{X} is a two-digit percentage of eggs
#' remaining. So crab with eggs remaining less than 50\% would be specified
#' using the string \sQuote{EL50P}.
#'
#' \item Crabs with missing legs may be targetted by including a \sQuote{ML}
#' string.
#'
#' \item Strings may be expressed in lowercase or uppercase letters. This has
#' no effect on the output.
#'
#' \item Spaces or other separators may be included in the string for clarity, but these are ignored.}
#'
#' @examples
#'
#'    # Get the vector of all default category strings:
#'    category.str()
#'
#'    # Get the vector of all default female category strings:
#'    category.str(sex = "female")
#'
#'    # Get the first 10 elements of male category strings:
#'    category.str(1:10, sex = "male")
#'
#'    # Get the first 10 elements of male category strings, long form:
#'    category.str(1:10, sex = "male", verbose = TRUE)
#'
#'    # Get long form description for mature males with carapace width larger than 95mm:
#'    category.str("MMG95")
#'
#'    #Mature males, large than 95mm with shell condition 3,4 or 5 (spaces are ignored):
#'    category.str("M M G95 SC345")
#'
#'    # Multiparous females with orange gonads:
#'    category.str("FMULTGNO")
#'
#'    # With multiple entries:
#'    category.str(c("FM", "FMULTGNO", "MG65ML"))

#'    # Mature males:
#'    parse.category.str("MM")
#' 
#'    # Immature males larger than 95mm:
#'    parse.category.str("MIG95")
#' 
#'    # Primiparous females larger between 35 and 60mm:
#'    parse.category.str("FPBT35TO60")
#'        
#' x <- read.scsbio(year = 2012)
#' index <- is.category(x, "TMMG95")
#' 
#' index <- is.category(x, "TMMG95")  # Mature males greater than 95mm: 
#' 
#' # Get number of observations per category:
#' index <- is.category(x, c("T", "TM", "TF", "TMIL95SC12", "TMMG95", "TFMULT"))
#' apply(index, 2, sum, na.rm = TRUE)
#' 
is.category <- function(x, ...) UseMethod("is.category")

is.category.scbio <- function(x, category, ...){
   # Check whether crab belongs to a specified category:

   if (length(category) == 1){
      # Initialize result vector:
      index <- rep(TRUE, dim(x)[1])
   
      # Parse category string:
      t <- parse.category.str(category)
   
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
  
   return(index)
}

#' @param code Either a numerical vector of snow crab category codes or a
#' vector of category character strings.
#' @param sex A scalar specifying the sex category of the default values to be
#' returned if \code{code} is numeric. Some accepted values are \code{1},
#' \sQuote{m} or \sQuote{male} for males, or \code{2}, \sQuote{f} or
#' \sQuote{female} for females.
#' @param language A character string specifying the language in which category
#' code descriptions are to be returned. Either english (\code{language =
#' "english"}), french (\code{language = "french"} or \code{language =
#' "français"}).
#' @return Returns a vector of character strings containing the descriptions
#' for a specified vector of category strings or codes.
#' @author Tobie Surette \email{Tobie.Surette@@dfo-mpo.gc.ca} \cr Pablo Vergara
#' \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @seealso \code{\link[gulf]{Snow Crab Methods}},
#' \code{\link[gulf]{parse.category.str}},
#' \code{\link[gulf]{is.category.scbio}}
#' @examples

#'
#'

category.str <- function(code = NULL, sex = NULL, group, verbose = FALSE, language = "english"){
   # Returns a short snow crab category string or description.

   # Parse 'group' object:
   if (!missing(group)) group <- match.arg(tolower(as.character(group)), c("other", "all")) else group <- NULL
   if (is.null(group)) group <- ""

   # Parse 'language' argument:
   language <- tolower(language)
   language <- match.arg(language, c("english", "french", "français"))
   if (language == "français") language <- "french"

   # Parse character string 'code' argument:
   if (is.character(code)){
      if (length(code) == 1){
         r <- parse.category.str(code)
         str <- NULL

         # Maturity:
         if (!is.null(r$maturity)){
            if (r$maturity == 1) str <- paste(str, "mature", sep = "")
            if (r$maturity == 2) str <- paste(str, "immature", sep = "")
         }

         # Extract maturity string:
         if (!is.null(r$fun)){
            str <- paste(str, gsub("is.", "", r$fun))
         }

         # Sex:
         if (!is.null(r$sex)){
            if (r$sex == 1){
               if (language == "english") str <- paste(str, "males")
               if (language == "french")  str <- paste(str, "mâles")
            }
            if (r$sex == 2){
               if (language == "english") str <- paste(str, "females")
               if (language == "french")  str <- paste(str, "femelles")
            }
         }
         str <- paste(str, ",", sep = "")

         # Carapace width:
         if (!is.null(r$carapace.width)){
            if (!is.na(r$carapace.width[1])){
               if (language == "english"){
                  if (r$carapace.width.flag[1]){
                     str <- paste(str, " cw >= ", r$carapace.width[1], "mm,", sep = "")
                  }else{
                     str <- paste(str, " cw > ", r$carapace.width[1], "mm,", sep = "")
                  }
               }
               if (language == "french"){
                  if (r$carapace.width.flag[1]){
                     str <- paste(str, " lc >= ", r$carapace.width[1], "mm,", sep = "")
                  }else{
                     str <- paste(str, " lc > ", r$carapace.width[1], "mm,", sep = "")
                  }
               }
            }
            if (!is.na(r$carapace.width[2])){
               if (language == "english"){
                  if (r$carapace.width.flag[2]){
                     str <- paste(str, " cw <= ", r$carapace.width[2], "mm,", sep = "")
                  }else{
                     str <- paste(str, " cw < ", r$carapace.width[2], "mm,", sep = "")
                  }
               }
               if (language == "french"){
                  if (r$carapace.width.flag[2]){
                     str <- paste(str, " lc <= ", r$carapace.width[2], "mm,", sep = "")
                  }else{
                     str <- paste(str, " lc < ", r$carapace.width[2], "mm,", sep = "")
                  }
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
               if (language == "english") str <- paste(str, " with missing legs,", sep = "")
               if (language == "french") str <- paste(str, " avec pattes manquantes,", sep = "")
            }
         }

         # Gonad colour:
         if (!is.null(r$gonad.colour)){
            if (language == "english"){
               if (r$gonad.colour == 1) str <- paste(str, " white gonads,", sep = "")
               if (r$gonad.colour == 2) str <- paste(str, " beige gonads,", sep = "")
               if (r$gonad.colour == 3) str <- paste(str, " orange gonads,", sep = "")
            }
            if (language == "french"){
               if (r$gonad.colour == 1) str <- paste(str, " gonade blanche,", sep = "")
               if (r$gonad.colour == 2) str <- paste(str, " gonade beige,", sep = "")
               if (r$gonad.colour == 3) str <- paste(str, " gonade orange,", sep = "")
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

         # Remove leading space:
         if (substr(str, 1, 1) == " ") str <- substr(str, 2, nchar(str))

         # Remove trailing comma:
         if (substr(str, nchar(str), nchar(str)) == ",") str <- substr(str, 1, nchar(str)-1)

         # Capitalize first letter:
         str <- paste(toupper(substr(str, 1, 1)), substr(str, 2, nchar(str)), sep = "")
         return(str)
      }else{
         str <- NULL
         for (i in 1:length(code)){
            temp <- category.str(code = code[i])
            str <- c(str, temp)
         }
         return(str)
      }
   }

   # Define short male category codes:
   short.male.str <- c("TM", "TMM", "TMI", "TMGE95", "TMMGE95",
                       "TMMNEW", "TMMMEDIUM", "TMMOLD",
                       "TMMGE95SC12345", "TMMGE95SC1", "TMMGE95SC2", "TMMGE95SC3", "TMMGE95SC4", "TMMGE95SC5",
                       "TMMGE95ML", "TMMGE95SC12", "TMMGE95SC345",
                       "COMLT102", "COMLT102SC1", "COMLT102SC2", "COMLT102SC3", "COMLT102SC4", "COMLT102SC5",
                       "COMLT102ML", "TMMGE102",
                       "TMMGE102SC1", "TMMGE102SC2", "TMMGE102SC3", "TMMGE102SC4", "TMMGE102SC5",
                       "TMMGE102ML",
                       "TMIGE95SC12", "TMIGE95SC345", "TMIGE95",
                       "TML95", "TMML95SC12", "TMML95SC345", "TMML95",
                       "TML35", "TMGE35L95", "TMIGE56", "TMIGE76",
                       "TMIL95SC12", "TMIL95SC345", "TMIL95")

   # Define short male category codes:
   short.female.str <- c("TF", "TFM", "TFI", # Total females, mature females, immature females.
                         "TFIGNW", "TFIGNB", "TFIGNO", # Gonad colour: white, beige, orange
                         "TFP",         # Mature, sc12
                         "TFPELO",      # Mature, sc12, eggs Light Orange
                         "TFPEDO",      # Mature, sc12, eggs Dark Orange
                         "TFPEB",       # Mature, sc12, eggs Brown
                         "TFPEL50P",    # Mature, sc12, Eggs Remaining < 50%
                         "TFPEG50P",    # Mature, sc12, Eggs Remaining > 50%
                         "TFMULT",      # Mature, sc34
                         "TFMULTELO",   # Mature, sc34, eggs light orange.
                         "TFMULTEDO",   # Mature, sc34, eggs dark orange.
                         "TFMULTEB",    # Mature, sc34, black eggs.
                         "TFMULTEL50P", # Mature, sc34, less than 50% eggs.
                         "TFMULTEG50P", # Mature, sc34, more than 50% eggs.
                         "TFSENILE")    # Mature, sc5

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

   # Parse 'sex' argument:
   if (!is.null(sex)){
      sex <- sort(unique(sex))
      if (is.numeric(sex)) sex <- c("male", "female")[sex]
      sex <- match.arg(tolower(sex), c("male", "female"))
   }

   # Define numeric code values:
   short.str <- NULL
   if (is.null(sex) & (group == "")) short.str <- c(short.male.str, short.female.str, other.male.str, other.female.str)
   if (("male" %in% sex) & (group == ""))        short.str <- c(short.str,  short.male.str)
   if (("male" %in% sex) & (group == "other"))   short.str <- c(short.str,  other.male.str)
   if (("male" %in% sex) & (group == "all"))     short.str <- c(short.str,  short.male.str, other.male.str)
   if (("female" %in% sex) & (group == ""))      short.str <- c(short.str,  short.female.str)
   if (("female" %in% sex) & (group == "other")) short.str <- c(short.str,  other.female.str)
   if (("female" %in% sex) & (group == "all"))   short.str <- c(short.str,  short.female.str, other.female.str)
   if (is.null(sex) & (group == "other"))        short.str <- c(other.male.str, other.female.str)

   values <- c(1:length(short.str))

   # Convert to longer versions if specified:
   if (verbose) short.str <- category.str(short.str, language = language)

   # Look up codes in table:
   result <- lookup(code, values, short.str)

   return(result)
}

#' Parse a Snow Crab Category String
#' 
#' Parses a snow crab category string onto a list containing appropriate
#' variable definitions. In effect, this function translates the crab category
#' specification into a form which is directly applicable for indexing fields
#' from snow crab data tables. A description of the category string's syntax is
#' presented in \code{\link[gulf]{category.str}}.
#' 
#' The output of this function may be used for subsetting or indexing parts of
#' snow crab data.
#' 
#' @param x A character string specifying a snow crab category.
#' @return Returns a list whose names are variable names to be found within
#' \sQuote{scbio} objects. The contents of each defined field is a translation
#' of that specified in the string.
#' @author Tobie Surette \email{Tobie.Surette@@dfo-mpo.gc.ca} \cr Pablo Vergara
#' \email{Pablo.Vergara@@dfo-mpo.gc.ca}
#' @seealso \code{\link[gulf]{Snow Crab Methods}},
#' \code{\link[gulf]{category.str}}, \code{\link[gulf]{is.category.scbio}}
#' @examples
#' 

#' 
#' 
# PARSE.CATEGORY.STR - Returns a list characterizing a snow crab category.
parse.category.str <- function(x){

   
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
   
   # Check 'category' argument:
   if (!is.character(x)) stop("Category specification must be a character string.")
   
   # Convert to lower case:
   x <- tolower(x)
   
   # Remove spaces:
   x <- gsub(" ", "", x)
   
   # Remove leading "T":
   x <- gsub("^t*", "", x)
   
   # Check for literal shell condition specification:
   if (length(grep("new", x)) > 0){
      fun <- c(fun, "is.new")
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
      fun <- c(fun, "is.soft")
      x <- gsub("soft", "", x)
   }
   if (length(grep("hard", x)) > 0){
      fun <- c(fun, "is.hard")
      x <- gsub("hard", "", x)
   }
         
   # Check for commercial identifier:
   x <- gsub("com", "mmge95", x)
   
   # Substitute 'bt' for 'g':
   x <- gsub("bt", "ge", x)
   
   # Substitute 'from' for 'g':
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
