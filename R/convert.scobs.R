convert.scobs <- function(x){
   # CONVERT.SCOBS - Reformat an 'scobs' object for use or printing.

   if (!attr(x, "converted")){
      temp <- attributes(x) # Get catch attributes.
      x <- as.data.frame(x) # Convert 'x' to data frame.

      # Define variables which are to be converted to numeric format:
      vars <- c("day", "month", "year", "crab.number", "sex",
                "carapace.width", "soak.time", "chela.height.right",
                "chela.height.left",  "shell.condition", "durometer", "cfvn",
                "depth",  "chela.length", "weight", "data.type",
                "male.total", "female.total", "abdomen.width", "maillageC",
                "maillage1", "maillage2", "maillage3", "maillageC1", "maillageC2",
                "maillageC3")

      # Remove "*" characters and convert to numeric:
      for (i in 1:length(vars)){
         x[, vars[i]] <- gsub("*", " ", x[, vars[i]], fixed = TRUE)
         x[, vars[i]] <- as.numeric(x[, vars[i]], fixed = TRUE)
      }
      type(temp$format[vars, ]) <- "i"
      temp$format["chela.height.right", "format"]   <- "F4.0"
      temp$format["chela.height.left", "format"]    <- "F4.0"
      
      # Fix for coordinate offset problem in earlier years:
      x$latitude <- gsub(" ", "", paste0(x$blank10, x$latitude))
      x$longitude <- gsub(" ", "", paste0(x$blank11, x$longitude))
      
      # Remove blank columns:
      variables <- names(x)
      index <- grep("blank", variables)
      x <- x[, setdiff(variables, variables[index])]
      temp$format <- temp$format[setdiff(variables, variables[index]), ]
       
      # Convert coordinates:
      x$latitude <- as.numeric(x$latitude)
      x$latitude[x$latitude == 9999] <- NA
      x$longitude <- as.numeric(x$longitude)
      x$longitude[x$longitude == 9999] <- NA
      x$latitude  <- dmm2deg(x$latitude/100)
      x$longitude <- dmm2deg(x$longitude/100)
      x$longitude <- -abs(x$longitude)
      #temp$format[vars, "format"] <- "F9.7"

      # Clean-up 'zone' variable:
      x$zone <- gsub(" ", "", x$zone)
      x$zone[x$zone %in% c("2F", "12F")] <- "F"
      x$zone[x$zone %in% c("2E", "12E")] <- "E"
      
      # Clean-up missing leg coding:
      x$missing.legs <- toupper(x$missing.legs)
      x$missing.legs <- gsub("M", "1", x$missing.legs)
      x$missing.legs <- gsub("R", "2", x$missing.legs)
      
      # Remove leading and trailing spaces:
      x$observer <- observer.scobs(x)
      x$vessel <- gsub("(^[ ]+)|([ ]+$)", "", x$vessel)
      x$trap.number <- as.numeric(x$trap.number)
      x$trip.number <- toupper(x$trip.number)
      
      # Restore format attribute:
      temp$names <- rownames(temp$format)
      attributes(x) <- temp

      # Remove blank records:
      index <- is.na(x$sex) & is.na(x$carapace.width) & is.na(x$abdomen.width) &
                is.na(x$chela.height.right) & is.na(x$chela.height.left) & is.na(x$shell.condition)
      x <- x[!index, ]
      
      # Add species code:
      x$species <- 2526
      
      # Update 'converted' attribute:
      attr(x, "converted") <- TRUE
   }else{
      temp <- attributes(x) # Get biological attributes.
      format <- fmt(eval(call(class(x)[1]))) # Get catch format.
      x <- as.data.frame(x)

      # Recreate columns which were deleted or absent:
      if (!is.empty(x)){
         x[, setdiff(row.names(format), names(x))] <- NA
      }else{
         x <- cbind(x, data.frame(names = setdiff(row.names(format), names(x))))
      }

      # Re-order columns of x to familiar order and delete standard format columns:
      x <- x[, row.names(format)]

      # Import information from old format if variables are present in new format:
      index <- intersect(row.names(format), row.names(temp$format))
      format[index, ] <- temp$format[index, ]

      # Convert coordinates:
      vars <- c("longitude", "latitude")
      x$longitude <- abs(x$longitude)
      for (i in 1:length(vars)) x[, vars[i]] <- round(deg2dmm(x[, vars[i]]) * 100)
      format[vars, "format"] <- "A6"

      # Restore format attribute:
      temp$names <- rownames(format)
      temp$format <- format
      attributes(x) <- temp

      # Update 'converted' attribute:
      attr(x, "converted") <- FALSE
   }

   return(x)
}
