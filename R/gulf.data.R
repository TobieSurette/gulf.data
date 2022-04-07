#' @title Gulf Data Package
#' 
#' @name gulf.data
#' 
#' @description \strong{gulf.data} is an \code{R} package for accessing and manipulating southern Gulf of Saint Lawrence fisheries data.
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
   
   # Snow crab data paths:
   path$snow.crab$root    <- "//ent.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/AquaRes_Common/Crab"
   path$snow.crab$logbook <- paste0(path$snow.crab$root, "/Databases/Fishery Logbooks/csv/")
   path$snow.crab$survey  <- paste0(path$snow.crab$root, "/Offshore Crab Common/")
   path$snow.crab$vms     <- paste0(path$snow.crab$root, "/Databases/vms/")
   
   # Groundfish data paths:
   path$groundfish$root  <- "//ENT.dfo-mpo.ca/dfo-mpo/GROUP/GLF/Regional_Shares/Science"
   path$groundfish$com  <- paste0(path$groundfish$root, "/Hd2/commercial/")              # Commercial data directory.
   path$groundfish$obs  <- paste0(path$groundfish$root, "/Hd2/observer/")                # Observer data directory.
   path$groundfish$rvs  <- paste0(path$groundfish$root, "/Hd2/research/groundfish/")     # Research vessel survey data directory.
   path$groundfish$sens <- paste0(path$groundfish$root, "/Hd2/research/sentinel/")       # Sentinel survey data directory.
   path$groundfish$nss  <- paste0(path$groundfish$root, "/Hd2/research/northumberland/") # Northumberland Strait survey data directory..
   path$groundfish$ins  <- paste0(path$groundfish$root, "/Hd2/research/inshore/")        # Inshore survey data directory.
   path$groundfish$jans <- paste0(path$groundfish$root, "/Hd2/research/january/")        # January survey data directory.
   path$groundfish$juvs <- paste0(path$groundfish$root, "/Hd2/research/juvenile/")       # Juvenile survey data directory.
   path$groundfish$seas <- paste0(path$groundfish$root, "/Hd2/research/seasonal/")       # Seasonal survey data directory.
   path$groundfish$scas <- paste0(path$groundfish$root, "/Hd2/research/scallop/")        # Scallop survey data directory.
   path$groundfish$ziff <- paste0(path$groundfish$root, "/Hd3/landings/zif/raw/")        # ZIFF data directory.
   path$groundfish$nafo <- paste0(path$groundfish$root, "/Hd3/landings/nafo/")           # NAFO landings data directory.
   
   # Oracle databases:
   p <- gulf.metadata::project()
   oracle <- list()
   oracle$rvs$dsn <- p[p$name == "rvs", "oracle.dsn"]
   oracle$rvs$uid <- p[p$name == "rvs", "oracle.uid"] 
   oracle$gap$dsn <- p[p$name == "sco", "oracle.dsn"] 
   oracle$gap$uid <- p[p$name == "sco", "oracle.uid"] 
   oracle$vms$dsn <- p[p$name == "vms", "oracle.dsn"] 
   oracle$vms$uid <- p[p$name == "vms", "oracle.uid"] 

   # Attach to options:
   options(gulf.path = path, gulf.oracle = oracle)

   invisible()
}

