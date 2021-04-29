read.flowmeter <- function(x, year, tow.id){
   # READ.FLOWMETER - Read fixed-point Aquadopp 300 m data files.

  # path <- paste0(.gulf.path$snow.crab, "Fishing Year ", year, "/Trawl Data/South Western Gulf/Flowmeter")
   
   files <- list.files(path = path, pattern = "*.dat", full.names = TRUE)
   tows <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
   tows <- toupper(unlist(lapply(strsplit(tows, "[.]"), function(x) x[1])))
   format <- gsub(".dat", ".hdr", files)
      
   if (!missing(tow.id)){
      index <- which(tows %in% tow.id)
      files <- files[index]
      tows <- tows[index]
      format <- format[index]
   }   
   
   # Read files:
   if (length(files) > 0){
      # Parse file formats:
      data <- list()
      for (i in 1:length(files)){
         cat(paste0("Reading : ", files[i], "\n"))
         f <- readLines(format[i])
         fields <- f[grep("^[ 0-9][0-9]", f)]     
         fields <- gsub(" +", " ", fields)
         fields <- gsub("^ ", "", fields)
         fields <- unlist(lapply(lapply(strsplit(fields, " "), function(x) x[2:length(x)]), function(x) paste(x, collapse = ".")))

         data[[i]] <- readLines(files[i])
         data[[i]] <- gsub(" +", " ", data[[i]])
         data[[i]] <- strsplit(data[[i]], " ")
         tmp <- as.data.frame(matrix(NA, nrow = length(data[[i]]), ncol = length(fields)))
         for (j in 1:length(fields)) tmp[, j] <- unlist(lapply(data[[i]], function(x) x[j]))
         names(tmp) <- fields
         data[[i]] <- tmp
   
         data[[i]]$tow.id <- tows[i]
      }
   }else{
      return(NULL)
   }
   
   # Collapse data:
   if (length(data) == 1) data <- data[[1]]
   if ((length(data) > 1) & !is.data.frame(data) & (length(unique(unlist(lapply(lapply(data, names), length)))) == 1)){
      tmp <- data[[1]]
      for (i in 2:length(data)) tmp <- rbind(tmp, data[[i]])
      data <- tmp
   }
   
   # Fix variable names:
   str <- names(data)
   str[str == "Month.(1-12)"] <- "month"
   str[str == "Day.(1-31)"] <- "day"
   str[str == "Year"] <- "year"
   str[str == "Hour.(0-23)"] <- "hour"                                               
   str[str == "Minute.(0-59)"] <- "minute"
   str[str == "Second.(0-59)"] <- "second"
   str[str == "Velocity.(Beam1|X|East).(m/s)"] <- "velocity.east"                
   str[str == "Velocity.(Beam2|Y|North).(m/s)"] <- "velocity.north"         
   str[str == "Velocity.(Beam3|Z|Up).(m/s)"] <- "velocity.up"
   str[str == "Heading.(degrees)"] <- "heading"
   str[str == "Pitch.(degrees)"] <- "pitch"
   str[str == "Roll.(degrees)"] <- "roll"
   str[str == "Pressure.(dbar)"] <- "pressure"
   str[str == "Temperature.(degrees.C)"] <- "temperature"
   names(data) <- str 
   
   # Convert to numeric format:
   for (i in 1:ncol(data)) if (all(gsub("[ -.0-9]", "", data[,i]) == "")) data[[i]] <- as.numeric(data[,i])
   
   return(data)
}
