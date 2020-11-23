#' Gear Codes
#'
#' @description Gear code functions.
#'
#' @param x Numeric gear code(s).
#' 
#' @examples 
#' gear() # Return all codes.
#' 
#' gear(1)
#' gear(c(1, 1, 10, NA, 11, NA, 2, 2)
#' 

#' @export
gear <- function(x, ...){
   descriptions <- c("3/4 35 otter trawl",            # 1
                     "35 otter trawl",                # 2
                     "yankee 36 otter trawl",         # 3
                     "41.5 otter trawl",              # 4
                     "long line",                     # 5
                     "beam trawl",                    # 6 
                     "mid-water trawl",               # 7
                     "Engel highlift (bottom) trawl", # 8
                     "western IIA",                   # 9 
                     "IYGPT (International young gadoid pelagic trawl)", # 10
                     "shrimp trawl",                  # 11  Foreign trawl (30m in length)
                     "50' flounder drag",             # 12  Concord trawl
                     "rock hopper trawl",             # 13  Modified yankee #41
                     "nephrops trawl",                # 14
                     "300 Balloon Star otter trawl",  # 15
                     "No. 286 otter trawl with rockhopper",                # 16
                     "8 gang toothed scallop drag lined with 14 mm mesh",  # 17
                     "8 gang toothed scallop drag with 82.6mm rings",      # 18
                     "Northumberland trawl",                               # 19
                     "2 gang toothed scallop drag lined with 14 mm mesh",  # 20
                     "6 gang toothed scallop drag with 82.6mm rings",      # 21
                     "Newston net ",                                       # 96
                     "10ft isaacs-kidd net ",                              # 97
                     "6ft isaacs-kidd net ")                               # 98


   codes <- as.character(c(1:21, 96:98))
   names(codes) <- descriptions

   # Look up codes:
   if (!missing(x)) return(names(codes)[match(as.character(x), codes)])
   
   return(codes)
}