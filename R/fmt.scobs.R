#' @title Snow crab Observer Text File Formats
#' 
#' @description
#' File format definitions to be used in reading snow crab observer data text files.
#' 
#' @param x Year.
#' @param year Year.
#' 

#' @export fmt.scobs
fmt.scobs <- function(x, year, ...){
   if (!missing(x)) if (is.numeric(x)) year <- x
   
   # Define file format:
   #           variable name                   format  fill.char  description
   fmt.str = c("blank1",                        "A1",     " ",    "Blank.",
               "day",                           "A2",     " ",    "Day.",
               "month",                         "A2",     " ",    "Month.",
               "year",                          "A4",     " ",    "Year.",
               "blank2",                        "A1",     " ",    "Blank.",
               "trap.type",                     "A2",     " ",    "Trap type.",
               "blank3",                        "A1",     " ",    "Blank.",
               "trip.number",                   "A9",     " ",    "Trip.number",
               "crab.number",                   "A3",     " ",    "Crab number",
               "blank4",                        "A1",     " ",    "Blank.",
               "sex",                           "A1",     "*",    "Sex.",
               "blank5",                        "A1",     " ",    "Blank.",
               "carapace.width",                "A3",     " ",    "Carapace width(mm).",
               "blank6",                        "A1",     " ",    "Blank.",
               "soak.time",                     "A4",     " ",    "Soak time for traps(days).",
               "trap.number",                   "A2",     " ",    "Trap number.",
               "chela.height.right",            "A4",     " ",    "Chela height right(mm).",
               "chela.height.left",             "A4",     " ",    "Chela height left(mm).",
               "shell.condition",               "A1",     "*",    "Shell condition.",
               "shell.condition.mossy",         "A1",     " ",    "Mossy shell condition.",
               "durometer",                     "A2",     "*",    "Durometer measurement.",
               "blank7",                        "A1",     " ",    "Blank.",
               "cfvn",                          "A6",     " ",    "Canadian fishing vessel number.",
               "blank8",                        "A1",     " ",    "Blank.",
               "comments",                      "A14",    " ",    "Comments.",
               "blank9",                        "A1",     " ",    "Blank.",
               "position.type",                 "A2",     " ",    "Position type.",
               "blank10",                       "A3",     " ",    "Blank.",
               "latitude",                      "A6",     " ",    "Latitude coordinate.",
               "blank11",                       "A3",     " ",    "Blank.",
               "longitude",                     "A6",     " ",    "Longitude coordinate.",
               "depth",                         "A4",     " ",    "Depth(fathoms).",
               "chela.length",                  "A2",     " ",    "Chela length(mm).",
               "weight",                        "A6",     " ",    "Total weight(kg).",
               "blank12",                       "A1",     " ",    "Blank.",
               "zone",                          "A2",     " ",    "Fishing zone.",
               "blank13",                       "A1",     " ",    "Blank.",
               "data.type",                     "A1",     " ",    "Port sample(=1), sea sample(=2) or trawl(=3).",
               "blank14",                       "A1",     " ",    "Blank.",
               "missing.legs",                  "A10",    " ",    "Missing leg codification.",
               "blank15",                       "A5",     " ",    "Blank.",
               "observer",                      "A23",    " ",    "Sampler names.",
               "blank16",                       "A8",     " ",    "Blank.",
               "vessel",                        "A18",    " ",    "Vessel name.",
               "blank17",                       "A1",     " ",    "Blank.",
               "egg.maturity",                  "A1",     "*",    "Egg maturity code.",
               "blank18",                       "A1",     " ",    "Blank.",
               "egg.development",               "A1",     "*",    "Egg maturity code.",
               "blank19",                       "A1",     " ",    "Blank.",
               "egg.clutch",                    "A1",     "*",    "Egg maturity code.",
               "blank20",                       "A8",     " ",    "Blank.",
               "male.total",                    "A3",     " ",    "Total number of males in trap.",
               "blank21",                       "A1",     " ",    "Blank.",
               "fishing.grid",                  "A4",     " ",    "Snow crab fishing grid.",
               "blank22",                       "A1",     " ",    "Blank.",
               "female.total",                  "A3",     " ",    "Total number of females in trap.",
               "blank23",                       "A1",     " ",    "Blank.",
               "logbook",                       "A7",     " ",    "Logbook ID number.",
               "blank24",                       "A1",     " ",    "Blank.",
               "abdomen.width",                 "A4",     " ",    "Abdomen width(mm).",
               "blank25",                       "A1",     " ",    "Blank.",
               "maillageC",                     "A3",     " ",    "Net measure C.",
               "blank26",                       "A1",     " ",    "Blank.",
               "maillage1",                     "A3",     " ",    "Net measure 1.",
               "blank27",                       "A1",     " ",    "Blank.",
               "maillage2",                     "A3",     " ",    "Net measure 2.",
               "blank28",                       "A1",     " ",    "Blank.",
               "maillage3",                     "A3",     " ",    "Net measure 3.",
               "blank29",                       "A1",     " ",    "Blank.",
               "maillageC1",                    "A2",     " ",    "Net measure C1.",
               "blank30",                       "A1",     " ",    "Blank.",
               "maillageC2",                    "A2",     " ",    "Net measure C2.",
               "blank31",                       "A1",     " ",    "Blank.",
               "maillageC3",                    "A2",     " ",    "Net measure C3.")
   
   # Recast file.info as a 'fmt' (format) object:
   n <- length(fmt.str) # Total number of fields.
   k <- 4 # Number of columns to be parsed.
   fmt <- data.frame(name = fmt.str[seq(1,n,k)], format = fmt.str[seq(2,n,k)],
                   fill.char = fmt.str[seq(3,n,k)], description = fmt.str[seq(4,n,k)])
   fmt$start <- cumsum(as.numeric(gsub("A", "", fmt$format)))
   fmt$stop  <- fmt$start + as.numeric(gsub("A", "", fmt$format)) - 1
   fmt$type  <- substr(fmt$format, 1, 1)
   fmt$width  <- as.numeric(substr(fmt$format, 2, 10))

   # Format for earlier years:
   if (!missing(year)){
      if (year < 1999){
         fmt <- c("day",             "I",    2,     3, 
                  "month",           "I",    5,     6, 
                  "year",            "I",    8,     9,
                  "trap.number",     "A",    11,    12,   
                  "trip.number",     "A",    15,    22,   
                  "crab.number",     "A",    23,    25,
                  "sex",             "A",    27,    27, 
                  "carapace.width",  "I",    29,    31,   
                  "soak.time",       "I",    33,    36,
                  "chela.height",    "I",    39,    40,
                  "shell.condition", "I",    47,    47,
                  "durometer",       "I",    49,    50,
                  "cfvn",            "I",    52,    57, 
                  "comments",        "A",    59,    72,
                  "position.type",   "A",    74,    75,
                  "latitude",        "I",    77,    82,
                  "longitude",       "I",    86,    91,
                  "depth",           "I",    95,    97,
                  "weight",          "I",    100,   105,
                  "zone",            "A",    107,   108,
                  "data.type",       "I",    110,   110,
                  "missing.legs",    "A",    112,   121,
                  "observer",        "A",    127,   149,
                  "vessel",          "A",    158,   173)  
         dim(fmt) <- c(4, length(fmt) / 4)
         fmt <- as.data.frame(t(fmt))
         names(fmt) <- c("variable", "type", "start", "stop") 
         fmt$start <- as.numeric(fmt$start)
         fmt$stop  <- as.numeric(fmt$stop) 
         
         # Fill-in blanks:
         v <- rep(1, max(fmt$stop))
         for (i in 1:nrow(fmt)) v[fmt$start[i]:fmt$stop[i]] <- 0
         k <- 1
         lab <- rep(NA, length(v))
         for (i in 1:length(v)){
            if (v[i] == 1){
               lab[i] <- k
            } 
            if (i > 1){
               if ((v[i] == 0) & (v[i-1] == 1)) k <- k + 1
            }
         }
         k <- max(lab, na.rm = TRUE)
         tmp <- data.frame(variable = paste0("blank", 1:k), type = "A", start = NA, stop = NA)
         for (i in 1:k){
            tmp$start[i] <- min(which(lab == i))
            tmp$stop[i]  <- max(which(lab == i))
         }
         fmt <- rbind(fmt, tmp)
         fmt <- fmt[order(fmt$start), ]
         
         # Define field width:
         fmt$width <- fmt$stop - fmt$start + 1
      }
   }

   return(fmt)
}
