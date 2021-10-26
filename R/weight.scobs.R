weight.scobs <- function(x, ...){
   # WEIGHT.SCOBS - Returns expected weight for a snow crab.

   # Buffer variables:
   if (!("chela.height" %in% names(x)))   x$chela.height <- x$chela.height.right
   if (!("gonad.colour" %in% names(x)))   x$gonad.colour <- NA
   if (!("egg.colour" %in% names(x)))     x$egg.colour <- NA
   if (!("eggs.remaining" %in% names(x))) x$eggs.remaining <- NA
   
   x$chela.height <- chela.height(x)
   
   w <- weight.scbio(x, ...)
   
   return(w)
}
