species.code <- function(x, language = "english", as.data.frame = FALSE, input = "stacac", output = "rv", exact.matching = FALSE){
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
      data <- read.csv(system.file("species table foreign.csv"), header = TRUE, stringsAsFactors = FALSE)                           
      return(data[match(x, data[, input]), output])
   }                                                                                                              
  
   # Numeric species code match:
   if (is.character(x)){
      ux <- unique(x)  
      
      # Species table:
      if (output == "rv"){
         data <- read.csv(system.file("species table foreign.csv"), header = TRUE, stringsAsFactors = FALSE) 
      }else{
         data <- read.csv(system.file("species table.csv"), header = TRUE, stringsAsFactors = FALSE)
      } 
      
      # Define data fields to search:
      if (language == "english") var <- "name_en"
      if (language == "french")  var <- "name_fr"
      if (language == "latin")   var <- "name_latin"
      if (language == "")        var <- names(x)[grep("^name", names(x))]
      
      # Loop over key words:
      v <- list()
      for (i in 1:length(ux)){
         words <- strplit(ux[i], " ")[[1]]
         for (j in 1:length(var)){
            r <- 1:nrow(x)
            for (k in 1:length(words)) r <- intersect(r, grep(words[k], data[, var[j]]))
         }
         v[[i]] <- x[, output][r]
      }
      
      
      # Drop list:
      if (length(v) == 1) v <- v[[1]]
      
      
      return(v)
   }
}
