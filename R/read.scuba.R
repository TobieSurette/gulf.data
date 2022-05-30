# "http://dmapps/en/scuba/reports/dive.log.xlsx", to = "dive log.xlsx")
# "http://dmapps/en/scuba/reports/section/?year=2021") # Read in transect section table:
# "http://dmapps/en/scuba/reports/dive/?year=2021") # Read dives table:
# "http://dmapps/en/scuba/reports/outing/?year=2021") # Read outings table:
# "http://dmapps/en/scuba/reports/observations/?year=2018") # Read biological data:
# "http://dmapps/en/scuba/reports/scuba_transect/?year=2021") # Scuba transect table:
read.scuba <- function(year, table, compress = TRUE, source = "dmapps"){
   # Define Scuba data path:
   path <- "http://dmapps/en/scuba/reports/"
   
   # Read outings table:
   if (!missing(year)){
      outings <- NULL
      for (i in 1:length(year)) outings <- rbind(outings, read.csv(paste0(path, "outing/?year=", year[i])))
   }else{
      outings <- read.csv(path, "outing/?year")
   }
   if (compress){
      # Remove empty columns:
      outings <- gulf.utils::compress(outings)
      
      # Remove empty rows:
      outings <- outings[which(outings$transect != ""), ]
   } 
   if (nrow(outings) > 0){
      outings$date <- as.character(date(outings$datetime))
      outings$time <- substr(outings$datetime, 12, 19)
      outings <- outings[, setdiff(names(outings), "datetime")]      
   }

   # Read dive table:
   if (!missing(year)){
      print(paste0(path, "dive/?year=", year[i]))
      dives <- NULL
      for (i in 1:length(year)) dives <- rbind(dives, read.csv(paste0(path, "dive/?year=", year[i])))
   }else{
      dives <- read.csv(path, "dive/?year")
   }
   if (compress){
      # Remove empty columns:
      dives <- gulf.utils::compress(dives)
      
      # Remove empty rows:
      #dives <- dives[which(dives$transect != ""), ]
   } 
   if (nrow(dives) > 0){
      #outings$date <- as.character(date(outings$datetime))
      #outings$time <- substr(outings$datetime, 12, 19)
      #outings <- outings[, setdiff(names(outings), "datetime")]      
   }

      
   return(dives)
}





   

