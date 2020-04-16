minilog.file <- function(x, year, set.number, tow.id, set.card, path = TRUE, survey = "sc", project, sort = TRUE, ...){
   # MINILOG.FILE - Return a list of available minilog file names.

   
   # git cat-file -p [ID/SHA-1 of directory]
   
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         index <- file.exists(x)
         if (any(index)) return(x[index])
         if ((length(grep(".", x, fixed = TRUE)) > 0) | (length(grep("/", x, fixed = TRUE)) > 0)) return(x)
         tow.id <- x 
      }
      if ("gulf.set" %in% class(x)) set.card <- x
   }
  
   # If 'file' is a set card, define proper parameters:
   if (!missing(set.card)){
      tow.id <- set.card$tow.id
      tow.id <- tow.id[!is.na(tow.id)]
      year   <- unique(set.card$year)   
   }  
   
   # Set 'survey' to snow crab survey if tow ID is specified:
   if (!missing(tow.id)) survey <- "sc"
   
   # Get minilog path directories:
   if (!missing(year)){
      path.str <- minilog.path.str(survey = survey, year = year, ...)
   }else{
      path.str <- minilog.path.str(survey = survey, ...)
   }
 
   # Parse 'survey' argument:
   survey <- tolower(survey)
   survey <- gsub(" ", "", survey, fixed = TRUE)
   survey <- gsub(".", "", survey, fixed = TRUE)
   survey <- match.arg(survey, c("rv", "september", "ns", "northumberlandstrait", "sc", "snowcrab"))

   # Define file string:
   if (survey %in% c("ns", "northumberlandstrait")){
      str <- shell(paste('dir/b "', path.str, '"\\*.txt', sep = ""), intern = TRUE)
   }
   if (survey %in% c("sc", "snowcrab", "rv", "september")){
      command <- paste('dir/b "', path.str, '"\\Asc-*', sep = "")
      str <- NULL
      for (i in 1:length(path.str)){
         str <- c(str, list.files(path = path.str[i], pattern = "*.csv", recursive = TRUE, full.names = path, ignore.case = TRUE))
         str <- c(str, list.files(path = path.str[i], pattern = "Asc-*", recursive = TRUE, full.names = path, ignore.case = TRUE))
      }   
   }
               
   # Remove invalid results:
   str <- str[setdiff(1:length(str), grep("File Not Found", str))]
   str <- str[setdiff(1:length(str), grep("deleted", tolower(str)))]
   
   # Extract subset using tow ID:
   if (!missing(tow.id)){
      if (year <= 2016){
         index <- rep(FALSE, length(str))
         for (i in 1:length(str)){
            tmp <- readLines(str[i], n = 10)
            tmp <- tmp[grep("GP[0-9][0-9][0-9]", tmp)]
            tmp <- strsplit(tmp, "GP")[[1]]
            tmp <- tmp[length(tmp)]
            tmp <- paste0("GP", tmp)
            tmp <- toupper(gsub(" +$", "", tmp))
            if (tmp %in% tow.id) index[i] <- TRUE
         }
         if (!any(index)) str <- NULL else str <- str[index] 
      }else{
         temp <- unlist(lapply(strsplit(str, "_", fixed = TRUE), function(x) x[length(x)]))
         temp <- toupper(gsub(".csv", "", tolower(temp)))
         index <- which(temp %in% tow.id)
         if (length(index) == 0) str <- NULL else str <- str[index]
      }
   }
   
   # Remove path:
   if (!path) str <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
   
   return(str)
}
