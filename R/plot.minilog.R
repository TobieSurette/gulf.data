#' Plot Minilog Data
#' 
#' @description Plots the data recorded by a Minilog probe, generally the temperature and water depth.
#' 
#' @param x Minilog object.
#' @param ... Line style arguments (see \code{\link[graphics]{lines}}).
#' 
#' @examples 
#' file <- system.file("extdata", "Minilog example.txt", package = "gulf.data")
#' x <- read.minilog(file)
#' plot(x)
#' 
#' @export plot.minilog
#' 
plot.minilog <- function(x, col, lwd = 1, ...){
   # Define default colours:
   if (missing(col)){
      cols <- "black"
      if (all(c("temperature", "depth") %in% names(x))) cols <- c("blue", "chartreuse3")
   }
   
   # Plot temperature:
   ylim <- c(0, 1.15 * max(x$temperature))
   plot(time(x), x$temperature, xaxs = "i", yaxs = "i",
        ylim = ylim, lwd = lwd, col = cols[1], yaxt = "n", xlab = "", ylab = "", type = "l", ...)
   
   axis(2, col = cols[1], col.axis = cols[1])
   mtext("Time", 1, 2, col = cols[1])
   mtext("Temperature(C)", 2, 2, col = cols[1])
      
   # Plot depth:
   if ("depth" %in% names(x)){
      r <- range(x$depth)
      r <- c(r[1] - 0.25, r[2] + 0.4)
      par(usr = c(par("usr")[1:2], r))
      lines(time(x), x$depth, lwd = lwd, col = cols[2], ...)
      axis(4, col = cols[2], col.axis = cols[2])
      mtext("Depth", 4, 2, col = cols[2])
   }
}
