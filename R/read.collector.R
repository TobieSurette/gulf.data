read.collector <- function(year, site, species, type, ...){

   # Parse input arguments:
   if (!missing(type)){
      type <- pmatch(tolower(type), c("site", "collector", "biological"))
      type <- type[!is.na(type)]
   }else{
      type <- c("site", "collector", "biological")
   }

   # Set file loading flags:
   tab <- c(site = FALSE, , col = FALSE, biological = FALSE)
   if (length(type) > 0) tab[type] <- TRUE else return(NULL)

   # Upload files:
   v <- list()
   if (tab["site"])       v$site       <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/site.csv", stringsAsFactors = FALSE)
   if (tab["collector"])  v$collector  <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/collector.csv", stringsAsFactors = FALSE)
   if (tab["biological"]) v$biological <- read.csv("https://raw.github.com/TobieSurette/lobster-collectors/master/data/biological.csv", stringsAsFactors = FALSE)

   # Data subsetting:
   if (!missing(year))    v <- lapply(v, function(x) x[x$year %in% year, ])
   if (!missing(site))    v <- lapply(v, function(x) x[x$site %in% site, ])
   if (!missing(species)) v <- lapply(v, function(x) x[x$species %in% species, ])

   return(v)
}
