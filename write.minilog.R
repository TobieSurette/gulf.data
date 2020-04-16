write.minilog <- function(x, file = NULL){
   # WRITE.MINILOG - Write a 'minilog' object to file.
   
   # Write header information to file:
   info <- header(x)
   
   for (i in 1:length(info)){
      str <- paste("* ", gsub(".", " ", names(info[i]), fixed = TRUE), "=", info[[i]], sep = "")
      write.table(str, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
   }
   fields <- names(x)
   fields <- fields[7:length(fields)]
   fields <- c("Time(hh:mm:ss)", fields)
   fields <- c("Date(yyyy-mm-dd)", fields)
   
   # Add 'Celsius' column:
   col <- grep("celsius", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), " (ºC)", sep = "")
   }
   
   # Add 'Temp' column:
   col <- grep("temp", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), "(AtoD)", sep = "")
   }    
    
   # Add 'Meters' column:
   col <- grep("meters", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), " (m)", sep = "")
   }    
   
   # Add 'Depth' column:
   col <- grep("depth", fields)
   if (!is.null(col)){
      fields[col] <- paste(toupper(substr(fields[col], 1, 1)), substr(fields[col], 2, nchar(fields[col])), "(AtoD)", sep = "")
   }    
   
   # Write header line for file:
   fields <- do.call(paste, c(as.list(fields), sep = ","))
   fields <- paste("*", fields) 
   write.table(fields, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
   
   # Create data frame to be written to file:
   temp <- data.frame(date = as.character(date(x)), 
                      time = substr(as.character(time(x)), 12, 20))
   temp <- cbind(temp, x[, 7:dim(x)[2]])
   
   # Write minilog data to file:
   write.table(temp, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
}
