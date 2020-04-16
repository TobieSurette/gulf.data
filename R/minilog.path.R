minilog.path <- function(year, project, survey, ...){
   # MINILOG.PATH - Return the path of a minilog directory.
   
   # Check input arguments:
   if (missing(project) & missing(survey)) stop("Specify 'project' or 'survey'.")
   if (!missing(project) & !missing(survey)) stop("Specify 'project' or 'survey', but not both.")
   
   # Parse 'survey' argument:
   if (!missing(survey)) project <- survey

   # Parse project string:
   project <- gsub("[. _-]", "", tolower(project), fixed = TRUE)
   project <- match.arg(survey, c("azmp", "collector", "alsi", "rvs", "september", "nss", "northumberlandstrait", "scs", "snowcrab"))
   project <- gsub("collector", "alsi", project)
   project <- gsub("september", "rvs", project)
   project <- gsub("snowcrabs", "scs", project)
   project <- gsub("northumberlandstrait", "nss", project)
   if (length(project) == 0) stop("'project' not defined.")

   
   # Turn off warnings:
   warning <- options("warn")[[1]]  
   options(warn = -1)
   
   # Atlantic Lobster Settlement Index project:
   if (project == "alsi"){
      
   }
   
   # September and NS surveys:
   if (project %in% c("rvs", "nss")){
      if (missing(year)){
         command <- paste('dir/ad/b "', str, '"\\????', sep = "")
         res <- shell(command, intern = TRUE)
         str <- paste(str, res, "/", sep = "")
      }else{
         str <- paste(str, year, "/",  sep = "")
      }
   }
   
   # Snow crab survey:
   if (project == "scs"){
      # Append year to path:
      if (missing(year)) str <- paste0(str, list.files(path = str, pattern = "^Fishing Year [0-9]+$")) else str <- paste(str, "Fishing Year ", year, "/", sep = "")   
      str <- paste0(str, "/Trawl Data/South Western Gulf/Minilog/ASCII")
      str <- str[file.exists(str)]
   
      # Remove irrelevant errors:
      index <- which(str == "File Not Found")
      index <- grep("File Not Found", str)
      if (length(index) > 0) str <- str[-index]
   }

   # Restore warnings:
   options(warn = warning)
   
   # Remove pure path entries:
   index <- which(substr(str, nchar(str), nchar(str)) != "/")
   if (length(index) > 0) str[index] <- paste0(str[index], "/")
     
   return(str)
}
