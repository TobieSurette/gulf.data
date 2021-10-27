summary.scobs <- function(x, by, type){
   # SUMMARY.SCOBS - Generate a snow crab observer data summary.

   # Define observer company:
   x$company <- c("Biorex Gulf", "Biorex Quebec", "Javitech")[match(toupper(substr(x$trip.number,1,1)), c("G", "Q", "J"))]
   
   # Initialize result variable:
   res <- list()
   
   # Summary table by year:
   vars <- c("year", "data.type", "company")
   res$year <- aggregate(list(n.trip = x$trip.number), by = x[vars], function(x) return(length(unique(x))))
   tmp <- aggregate(list(n.trap = x$trap.number), by = x[c(vars, "trip.number")], function(x) length(unique(x)))
   res$year$n.trap <- aggregate(list(n.trap = tmp$n.trap), by = tmp[vars], sum)$n.trap
   res$year$n.crab <- aggregate(list(n.crab = x$trip.number), by = x[vars], length)$n.crab
   res$year$n.vessel <- aggregate(list(n.vessel = x$cfvn), by = x[vars], function(x) return(length(unique(x))))$n.vessel  
   res$year$n.observer <- aggregate(list(n.observer = x$observer), by = x[vars], function(x) return(length(unique(x))))$n.observer
   
   # Summary observer table:
   vars <- c("year", "data.type", "observer", "company")
   res$observer <- aggregate(list(n.trip = x$trip.number), by = x[vars], function(x) length(unique(x)))
   tmp <- aggregate(list(n.trap = x$trap.number), by = x[c(vars, "trip.number")], function(x) length(unique(x)))
   res$observer <- cbind(res$observer, aggregate(list(n.trap = tmp$n.trap), by = tmp[vars], sum)["n.trap"])
   res$observer <- cbind(res$observer, aggregate(list(n.crab = x$trip.number), by = x[vars], length)["n.crab"])
   res$observer <- cbind(res$observer, aggregate(list(n.vessel = x$cfvn), by = x[vars], function(x) return(length(unique(x))))["n.vessel"]) 
                
   # Summary trap table:
   x$soft <- !is.hard.scobs(x)
   x$individual.weight <- exp(-8.230136 + 3.098 * log(x$carapace.width)) / 1000 #weight(x, units = "kg")
   vars <- c("year", "data.type", "observer", "trip.number", "trap.number", "zone", "weight")
   res$trap <- aggregate(x[c("longitude", "latitude")], by = x[vars], mean, na.rm = TRUE)  
   res$trap$number.sampled <- aggregate(list(x = x$trip.number), by = x[vars], function(x) length(x))$x  
   res$trap$weight.sampled <- aggregate(list(x = x$individual.weight), by = x[vars], function(x) mean(x, na.rm = TRUE) * length(x))$x 
   #res$trap$weight.sampled <- aggregate(list(x = weight(x, units = "kg")), by = x[vars], function(x) mean(x, na.rm = TRUE) * length(x))$x 
   res$trap$number.caught <- aggregate(list(x = x$male.total), by = x[vars], function(x) unique(x)[1])$x
   ratio <- res$trap$number.caught / res$trap$number.sampled
   res$trap$weight.caught <- res$trap$weight.sampled * ratio 
   res$trap$number.sampled.soft <- aggregate(list(x = x$soft), by = x[vars], sum, na.rm = TRUE)$x


   res$trap$number.caught.soft <- res$trap$number.sampled.soft * ratio
   res$trap$number.caught.hardg95 <- aggregate(list(x = (!x$soft | is.na(x$soft)) & x$carapace.width >=95), by = x[vars], sum, na.rm = TRUE)$x * ratio
   res$trap$number.caught.hardl95 <- aggregate(list(x = (!x$soft | is.na(x$soft)) & x$carapace.width < 95), by = x[vars], sum, na.rm = TRUE)$x * ratio
   
   res$trap$number.sampled.hardl95 <- aggregate(list(x = (!x$soft | is.na(x$soft)) & x$carapace.width < 95), by = x[vars], sum, na.rm = TRUE)$x 
   
   res$trap$weight.sampled.soft <- aggregate(list(x = x$soft * x$individual.weight), by = x[vars], sum, na.rm = TRUE)$x
   res$trap$weight.sampled.hardg95 <- aggregate(list(x = ((!x$soft | is.na(x$soft)) & (x$carapace.width >= 95)) * x$individual.weight), by = x[vars], sum, na.rm = TRUE)$x
   res$trap$weight.sampled.hardl95 <- aggregate(list(x = ((!x$soft | is.na(x$soft)) & (x$carapace.width < 95)) * x$individual.weight), by = x[vars], sum, na.rm = TRUE)$x
 
   #res$trap$number.caught.soft <- res$trap$number.sampled.soft * ratio
   res$trap$weight.caught.soft <- res$trap$weight.caught * res$trap$number.sampled.soft / res$trap$number.sampled
   
   res$trap$number.sampled.commercial <- aggregate(list(x = is.category(x, "COMSC345")), by = x[vars], sum, na.rm = TRUE)$x  
   res$trap$weight.sampled.commercial <- aggregate(list(x = is.category(x, "COMSC345") * x$individual.weight), by = x[vars], sum, na.rm = TRUE)$x   
   res$trap$number.caught.commercial <- res$trap$number.sampled.commercial * ratio                                                                
   res$trap$weight.caught.commercial <- res$trap$weight.sampled.commercial * ratio 
   fun <-  function(x) return(sum(unlist(strsplit(as.character(x), "")) == "1") / length(x))
   res$trap$missing.legs <- aggregate(list(x = x$missing.legs), by = x[vars], fun )$x
   res$trap <- sort(res$trap, by = c("trip.number", "trap.number"))
 
   # Trip summary table:
   vars <- c("year", "data.type", "observer", "trip.number", "zone", "weight")
   res$trip <- aggregate(list(n.trap = res$trap$trip.number), by = res$trap[vars], length)
   res$trip$number.caught <- aggregate(list(x = res$trap$number.caught), by = res$trap[vars], sum, na.rm = TRUE)$x
   res$trip$weight.caught <- aggregate(list(x = res$trap$weight.caught), by = res$trap[vars], sum, na.rm = TRUE)$x
   res$trip$weight.caught[res$trip$weight.caught == 0] <- NA
   res$trip$number.caught <- res$trip$number.caught * res$trip$weight / res$trip$weight.caught
   res$trip$number.sampled <- aggregate(list(x = res$trap$number.sampled), by = res$trap[vars], sum)$x   
 
   res$trip$number.sampled.soft <- aggregate(list(x = res$trap$number.sampled.soft), by = res$trap[vars], sum)$x   
   res$trip$number.caught.soft <- res$trip$number.caught * (res$trip$number.sampled.soft / res$trip$number.sampled)
   res$trip$number.caught.soft <- res$trip$number.caught * (res$trip$number.sampled.soft / res$trip$number.sampled)
 
   res$trip$number.sampled.hardl95 <- aggregate(list(x = res$trap$number.sampled.hardl95), by = res$trap[vars], sum)$x 
   
   res$trip$weight.sampled.soft <- aggregate(list(x = res$trap$weight.sampled.soft), by = res$trap[vars], sum, na.rm = TRUE)$x
   res$trip$weight.sampled.hardg95 <- aggregate(list(x = res$trap$weight.sampled.hardg95), by = res$trap[vars], sum, na.rm = TRUE)$x
   res$trip$weight.sampled.hardl95 <- aggregate(list(x = res$trap$weight.sampled.hardl95), by = res$trap[vars], sum, na.rm = TRUE)$x
   
   # res$trip$number.caught.soft <- aggregate(list(x = tmp$number.caught.soft), by = tmp[vars], sum)$x  # Weigh by total male count.
   res$trip$proportion.soft <- res$trip$number.sampled.soft / res$trip$number.sampled
   
   res$trip$number.caught.hardl95 <- aggregate(list(x = res$trap$number.caught.hardl95), by = res$trap[vars], sum)$x * res$trip$weight / res$trip$weight.caught 
   res$trip$number.caught.hardg95 <- aggregate(list(x = res$trap$number.caught.hardg95), by = res$trap[vars], sum)$x * res$trip$weight / res$trip$weight.caught 
   
   # Deal with missing male counts for CPUEs: 
   res$trip$number.caught.commercial <- aggregate(list(x = res$trap$number.caught.commercial), by = res$trap[vars], sum, na.rm = TRUE)$x
   res$trip$weight.caught.commercial <- aggregate(list(x = res$trap$weight.caught.commercial), by = res$trap[vars], sum, na.rm = TRUE)$x                                                                
   res$trip$CPUE.number.commercial <- res$trip$number.caught.commercial / aggregate(list(x = res$trap$number.caught), by = res$trap[vars], function(x) length(x[!is.na(x)]))$x
   res$trip$CPUE.weight.commercial <- res$trip$weight.caught.commercial / aggregate(list(x = res$trap$weight.caught), by = res$trap[vars], function(x) length(x[!is.na(x)]))$x
   res$trip <- res$trip[order(res$trip$trip.number), ]

   # Summary by zone:
   vars <- c("year", "zone")
   tmp <- aggregate(res$trip$weight, by = res$trip[vars], sum)
   res$zone <- aggregate(res$trip["proportion.soft"] * res$trip$weight / tmp$x[match(res$trip$zone, tmp$zone)] , by = res$trip[vars], sum)        
   res$zone$CPUE.number.commercial <- aggregate(list(x = res$trip$CPUE.number.commercial), by = res$trip[vars], mean, na.rm = TRUE)$x
   res$zone$CPUE.number.commercial.sd <- aggregate(list(x = res$trip$CPUE.number.commercial), by = res$trip[vars], sd, na.rm = TRUE)$x
   res$zone$CPUE.weight.commercial <- aggregate(list(x = res$trip$CPUE.weight.commercial), by = res$trip[vars], mean, na.rm = TRUE)$x
   res$zone$CPUE.weight.commercial.sd <- aggregate(list(x = res$trip$CPUE.weight.commercial), by = res$trip[vars], sd, na.rm = TRUE)$x

   res$zone$n.trip <- aggregate(list(x = res$trap$trip.number), by = res$trap[vars], function(x) length(unique(x)))$x
   res$zone$n.trap <- aggregate(list(x = res$trap$number.caught.commercial), by = res$trap[vars], function(x) length(x[!is.na(x)]))$x
   res$zone$n.crab <- aggregate(list(x = res$trap$number.sampled), by = res$trap[vars], sum, na.rm = TRUE)$x
   
   # Summary of errors:
   cat("\nIrregularities summary : \n")
   
   # Missing carapace width measurements:
   index <- which(is.na(x$carapace.width))
   if (length(index) > 0){
      tmp <- aggregate(x$trip.number[index], by = x[index, "trip.number", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip '", tmp[i,1], "' has ", tmp[i,2], " missing carapace width measurements.\n"))
      }
   }
   
   # Check month:
   index <- which(x$month < 4)
   if (length(index) > 0){
      tmp <- aggregate(x$trip.number[index], by = x[index, c("trip.number", "month")], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip '", tmp[i,1], "' has month = ", tmp[i,"month"], ".\n"))
      }
   }
     
   # Check for missing chelae cases with measurements which were present:
   index <- which((substr(x$missing.legs,1,1) %in% c("1", "2")) & (substr(x$missing.legs,6,6) %in% c("1", "2")) & (!is.na(x$chela.height.right) | !is.na(x$durometer)))
   if (length(index) > 0){
      tmp <- aggregate(x$observer[index], by = x[index, "observer", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Observer '", tmp[i,1], "' has ", tmp[i,2], " instances where there were chela or durometer measures, but no chelae present.\n"))
      }
   }
  
   # Check that missing legs contain the right codes:
   index <- which(!unlist(lapply(strsplit(x$missing.legs, ""), function(x) return(all(x %in% c(" ", "*", "1", "2", "7"))))))
   if (length(index) > 0){
      tmp <- aggregate(x$observer[index], by = x[index, "trip.number", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip number '", tmp[i,1], "' has ", tmp[i,2], " instances where there missing legs had odd codes.\n"))
      }
   }
   
   # Check zone codes:
   index <- which(!(x$zone %in% c("12", "19", "E", "F", "25", "18")))
   if (length(index) > 0){
      tmp <- aggregate(x$observer[index], by = x[index, "trip.number", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip number '", tmp[i,1], "' has ", tmp[i,2], " instances where the zone is non-standard.\n"))
      }
   }
      
   # Check that zone codes are consistent within trips:
   tmp <- aggregate(x$zone, by = x[c("trip.number", "data.type")], function(x) return(length(unique(x))))
   index <- which(tmp$x > 1)
   if (length(index) > 0){
      for (i in 1:length(index)){
         cat(paste0("   Trip number '", tmp[index[i],1], "' with data.type = ", tmp[index[i],2], ", has ", tmp[index[i],3], " different fishing zones.\n"))
      }
   }
   
   # Check that zone codes are identical between port and sea samples:
   if (any(x$data.type == 1) & any(x$data.type == 2)){
      trips <- intersect(unique(x$trip.number[x$data.type == 1]), unique(x$trip.number[x$data.type == 2]))
      if (length(trips) > 0){
         index <- x$trip.number %in% trips
         tmp <- aggregate(x$zone[index], by = x[index, "trip.number", drop = FALSE], function(x) return(length(unique(x))))
         index <- which(tmp$x > 1)
         if (length(index) > 0){
            for (i in 1:length(index)){
               cat(paste0("   Trip number '", tmp[index[i],1], "' has different fishing zones between port and sea samples.\n"))
            }
         }
      }
   }
   
   # Check landed weight field:
   index <- which(x$weight <= 0 | is.na(x$weight))
   if (length(index) > 0){
      tmp <- aggregate(x$observer[index], by = x[index, "trip.number", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip number '", tmp[i,1], "' has ", tmp[i,2], " instances where landed weight is empty."))
      }
   }
   
   # Check coordinates:
   index <- which((x$latitude < 40 | x$latitude > 50) | (x$longitude < -67 | x$longitude > -59))
   if (length(index) > 0){
      tmp <- aggregate(x$observer[index], by = x[index, "trip.number", drop = FALSE], length)
      for (i in 1:nrow(tmp)){
         cat(paste0("   Trip number '", tmp[i,1], "' has ", tmp[i,2], " instances where latitude-longitude coordinates lie outside the gulf.\n"))   
      }
   }
   
   # Check date ranges within trips:
   vars <- c("year", "month", "day", "trip.number", "data.type")
   tmp <- unique(x[vars])
   tmp$date <- date(tmp)
   a <- aggregate(list(start.date = tmp$date), by = tmp[c("trip.number", "data.type")], min)
   a$end.date <- aggregate(list(end.date = tmp$date), by = tmp[c("trip.number", "data.type")], max)$end.date
   d <- (a$end.date - a$start.date) / (24*60*60)
   index <- which(d > 5)
   if (length(index) > 0){
      for (i in 1:length(index)){
         cat(paste0("   Trip number '", a$trip.number[index[i]], "', with data type ", a$data.type[index[i]], " has a duration of ", d[index[i]], " days.\n"))   
      }
   }   
   
   # Compare dates from port and sea samples:
   if (any(x$data.type == 1) & any(x$data.type == 2)){
      trips <- intersect(unique(x$trip.number[x$data.type == 1]), unique(x$trip.number[x$data.type == 2]))
      if (length(trips) > 0){
         unique()
         index <- x$trip.number %in% trips
         tmp <- aggregate(x$zone[index], by = x[index, "trip.number", drop = FALSE], function(x) return(length(unique(x))))
         index <- which(tmp$x > 1)
         if (length(index) > 0){
            for (i in 1:length(index)){
               cat(paste0("   Trip number '", tmp[index[i],1], "' has different fishing zones between port and sea samples.\n"))
            }
         }
      }
   }
   
   return(res)
}
