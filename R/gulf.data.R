#' Gulf Data Package
#' 
#' @name gulf.data
#' 
#' @description \strong{gulf.data} is an \code{R} package for accessing and manipulating southern Gulf of Saint Lawrence fisheries science data.
#' 
#' @examples 
#' # Snow crab survey data:
#' x <- read.scsset(2020) # Load snow crab survey set/tow data.
#' y <- read.scsbio(2020) # Load snow crab survey biological data.
#' catch(x) <- catch(y, category = c("MM", "COM", "MF") # Attach snow crab catches to tow data.
#' summary(x)  # Snow crab tow summary.
#' summary(y)  # Snow crab biological data summary.

.onLoad <- function(libname, pkgname){
   # Define default paths to be used and store them within 'options':
   path <- list()
   
   # Main drive paths:
   path$hd2 <- "//mon02uni01a/Science/Hd2/"
   path$hd3 <- "//mon02uni01a/Science/Hd3/"
   path$snow.crab <- "//mon02uni01a/AquaRes_Common$/Crab/"

   # Groundfish data directories:
   path$com  <- paste0(path$hd2, "commercial/")              # Commercial data directory.
   path$obs  <- paste0(path$hd2, "observer/")                # Observer data directory.
   path$rvs  <- paste0(path$hd2, "research/groundfish/")     # Research vessel survey data directory.
   path$sens <- paste0(path$hd2, "research/sentinel/")       # Sentinel survey data directory.
   path$nss  <- paste0(path$hd2, "research/northumberland/") # Northumberland Strait survey data directory..
   path$ins  <- paste0(path$hd2, "research/inshore/")        # Inshore survey data directory.
   path$jans <- paste0(path$hd2, "research/january/")        # January survey data directory.
   path$juvs <- paste0(path$hd2, "research/juvenile/")       # Juvenile survey data directory.
   path$seas <- paste0(path$hd2, "research/seasonal/")       # Seasonal survey data directory.
   path$scas <- paste0(path$hd2, "research/scallop/")        # Scallop survey data directory.
   path$ziff <- paste0(path$hd3, "landings/zif/raw/")        # ZIFF data directory.
   path$nafo <- paste0(path$hd3, "landings/nafo/")           # NAFO landings data directory.
   
   # Survey probe data:
   path$rv.minilog   <- paste0(path$rv, "minilog/") # Research vessel survey minilog data directory.
   path$ns.minilog   <- paste0(path$ns, "minilog/") # Northumberland Strait survey minilog data directory.
   path$rv.scanmar   <- paste0(path$rv, "scanmar/") # Research vessel survey scanmar data directory.
   path$ns.scanmar   <- paste0(path$ns, "scanmar/") # Northumberland Strait survey scanmar data directory.
   
   # Snow crab data paths:
   path$scs  <- paste0(path$snow.crab, "Offshore Crab Common/")
   
   # Herring data paths:
   path$herring.phone <- paste0(path$root, "herring/phone/")
   
   # Oracle databases:
   oracle <- list()
   oracle$generic.user <- "GLF_4R"
   oracle$generic.pass <- "GLF_4R"
   oracle$dev.bd       <- "dtran"
   oracle$prod.bd      <- "ptran"
   oracle$gap.bd       <- "gap"
   
   # Attach to options:
   options(gulf.path = path, gulf.oracle = oracle)

   invisible()
}

