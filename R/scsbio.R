#' Snow Crab Biological Data Class
#'
#' @description 
#' 
#' The \sQuote{scsbio} class is a biological database containing information about individual 
#' organisms sampled on the snow crab annual survey. This function is used for creating
#' \sQuote{scbio} objects, which contain snow crab biological data.
#' 
#' @param x Object.
#' 
#' @export scsbio
#' @export scsbio.default
#' @export scsbio.scsset
#' 
scsbio <- function(x, ...) UseMethod("scsbio")

#' @describeIn scsbio Create a \code{scsbio} object.
scsbio.default <- function(x, format = fmt.scbio(), ...){
   if ("scsbio" %in% class(x)) return(x)
   
   key(x) <- c("year", "month", "day", "tow.number", "crab.number")
   class(x) <- unique(c("scsbio", class(x))) 
   return(x)
}

#' @describeIn scsbio Summary biological data for a set of snow crab data.
scsbio.scsset <- function(x, ...){
   v <- read.scsbio(x, ...)
   v <- summary(v, by = key(x), ...)
   index <- match(x[key(x)], v[key(x)])
   x <- cbind(x, v[index, setdiff(names(x), key(x))])
}

#' @describeIn scsbio ASCII format definition for reading and writing snow crab biological data.
fmt.scsbio <- function(x = NULL){
   # If argument x is specified:
   if (!is.null(x)) return(fmt.ascii(x))

   #           variable name                   format  fill.char  description
   fmt.str = c("blank1",                        "A1",     " ",    "Blank.",
               "day",                           "A2",     "0",    "Day.",
               "month",                         "A2",     "0",    "Month.",
               "year",                          "A4",     " ",    "Year.",
               "blank2",                        "A1",     " ",    "Blank.",
               "zone",                          "A2",     " ",    "Fishing zone.",
               "subzone",                       "A1",     " ",    "Fishing sub-zone.",
               "blank3",                        "A3",     " ",    "Blank.",
               "data.type",                     "A1",     " ",    "Trawl = 3.",
               "blank4",                        "A1",     " ",    "Blank.",
               "tow.number",                    "A3",     "0",    "Tow number.",
               "crab.number",                   "A4",     "0",    "Crab ID number.",
               "blank5",                        "A1",     " ",    "Blank.",
               "sex",                           "A1",     "*",    "Sex.",
               "carapace.width",                "A6",     "*",    "Carapace width(mm).",
               "abdomen.width",                 "A5",     "*",    "Abdomen width(mm).",
               "blank6",                        "A1",     " ",    "Blank.",
               "chela.height",                  "A5",     "*",    "Chela height.",
               "maturity",                      "A1",     "*",    "Maturity.",
               "blank7",                        "A1",     " ",    "Blank.",
               "shell.condition",               "A1",     "*",    "Shell condition.",
               "shell.condition.mossy",         "A1",     " ",    "Mossy shell condition.",
               "gonad.colour",                  "A2",     "*",    "Gonad colour code.",
               "blank8",                        "A1",     " ",    "Blank.",
               "egg.colour",                    "A1",     "*",    "Egg colour code.",
               "eggs.remaining",                "A1",     "*",    "Eggs remaining code.",
               "tag.number",                    "A8",     "0",    "Crab tag number.",
               "blank9",                        "A1",     " ",    "Blank.",
               "missing.legs",                  "A10",    " ",    "Missing leg codification.",
               "blank10",                       "A1",     " ",    "Blank.",
               "position.type",                 "A2",     " ",    "Position type.",
               "blank11",                       "A1",     " ",    "Blank.",
               "latitude.start",                "A8",     " ",    "Start latitude of tow.",
               "blank12",                       "A1",     " ",    "Blank.",
               "longitude.start",               "A8",     " ",    "Start longitude of tow.",
               "blank13",                       "A1",     " ",    "Blank.",
               "depth",                         "A3",     " ",    "Depth(fathoms).",
               "blank14",                       "A1",     " ",    "Blank.",
               "soak.days",                     "A1",     " ",    "Soak days for traps.",
               "durometer",                     "A3",     "*",    "Durometer measurement.",
               "blank15",                       "A1",     " ",    "Blank.",
               "trap.code",                     "A4",     " ",    "Trap code.",
               "blank16",                       "A7",     " ",    "Blank.",
               "samplers",                      "A21",    " ",    "Sampler names.",
               "weight",                        "A6",     " ",    "Crab weight.",
               "blank17",                       "A1",     " ",    "Blank.",
               "comments",                      "A25",    " ",    "Comments.",
               "tow.id",                        "A8",     " ",    "Tow ID string.")

   n <- length(fmt.str) # Total number of fields.
   k <- 4 # Number of columns to be parsed.

   # Recast file.info as a 'fmt' (format) object:
   x <- fmt(variable = fmt.str[seq(1,n,k)], format = fmt.str[seq(2,n,k)],
            fill.char = fmt.str[seq(3,n,k)], description = fmt.str[seq(4,n,k)])

   return(x)
}
