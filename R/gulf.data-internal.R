.gulf.path <- list()

# Root directory:
.gulf.path$rvroot = "//DFNBE1CwpFSP002/Hd2/" 
.gulf.path$scroot <- "//mon02uni01a/AquaRes_Common$/Crab/Offshore Crab Common/"

# Define minilog data paths:
.gulf.path$minilog$collector <- "https://raw.github.com/TobieSurette/lobster-collectors/master/data/raw/Minilog_files/"

.gulf.path$minilog$azmp <- ""
.gulf.path$minilog$sc <- paste0(.gulf.path$scroot)                             # Snow crab survey.
.gulf.path$minilog$ns <- paste0(.gulf.path$rvroot, "research/northumberland/") # Northumberland Strait survey.  
.gulf.path$minilog$rv <- paste0(.gulf.path$rvroot, "research/groundfish/")     # September RV survey.


  