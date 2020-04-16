plot.minilog <- function(x, tow.id, year, buffer = 2, ...){
   # PLOT.MINILOG - Graphically display a minilog object.

   # Define 'tow.id' argument:
   if (!missing(year) & !missing(tow.id) & missing(x)) x <- read.minilog(year = year, tow.id = tow.id)
   if (missing(tow.id)) tow.id <- sort(unique(x$tow.id))
   
   # Define data year:
   if (missing(year)) year <- sort(unique(x$year))
   
   # Parse 'buffer' argument:
   if (length(buffer) == 1) buffer  <- c(buffer, buffer)
   
   # Define default colours:
   cols <- c("blue", "chartreuse3")
   
   for (i in 1:length(tow.id)){
      windows()
      par(mar = c(5, 4, 4, 4))
      y <- read.scset(year = year)
      
      tt <- c(start.time(y[y$tow.id == tow.id[i], ]), end.time(y[y$tow.id == tow.id[i], ]))
      if (is.na(tt[1])) tt[1] <- min(time(x))
      if (is.na(tt[2])) tt[2] <- max(time(x))
      
      # Subset by tow ID:
      xx <- x[x$tow.id == tow.id[i], ]
      
      # Define relative time by minutes:
      xx$time <- time2min(time(xx), tt[1])
      
      # Start and end times relative to start time:
      tt <- time2min(tt, tt[1])
      
      # Plot depth:
      plot(xx$time, xx$depth, xlim = c(0-buffer[1], tt[2]+buffer[2]), ylim = c(0, 1.15 * max(xx$depth)), 
           lwd = 2, col = cols[1], yaxt = "n", ylab = "", xlab = "Time (min)", type = "l")
      axis(2, col = cols[1], col.axis = cols[1])
      mtext("Depth(m)", 2, 2, col = cols[1])
      
      # Plot temperature:
      if ("temperature" %in% names(x)){
         ii <- (xx$time >= (0-buffer[1])) & (xx$time <= (tt[2]+buffer[2]))
         r <- range(x$temperature[ii])
         r <- c(r[1] - 0.25, r[2] + 0.4)
         par(usr = c(par("usr")[1:2], r))
         lines(xx$time, xx$temperature, lwd = 2, col = cols[2])
         axis(4, col = cols[2], col.axis = cols[2])
         mtext("Temperature(C)", 4, 2, col = cols[2])
      }
      
      lines(c(tt[1], tt[1]), par("usr")[3:4], lwd = 2, lty = "dashed", col = "red")
      lines(c(tt[2], tt[2]), par("usr")[3:4], lwd = 2, lty = "dashed", col = "red")
      
      legend("bottomleft", legend = c("Depth", "Temperature", "Start and end time"), 
             lwd = 2, bg = "white", col = c(cols, "red"), lty = c("solid", "solid", "dashed")) 
             
      text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
           par("usr")[3] + 0.95 * diff(par("usr")[3:4]), 
           paste0("Tow ID: '", tow.id[i], "'"))
   }
}
