# gulf.data

Functions for accessing and manipulating southern Gulf of Saint Lawrence data

## Package Data Tables:
A number of data tables are included in the package.

  - North Atlantic species names and corresponding numeric codes (e.g. STACAC, NAFO, ...).  
  - Research project description table.
  - Research survey description table.
  - Survey vessel description table.
  - Snow crab survey tow/set data.
  - Snow crab survey biological data.
  
## Data Access:

Functions to access, read and load southern Gulf of Saint Lawrence research data are also provided.

### Survey data:
  - Snow crab annual survey. 
```
   x <- read.scsset(2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
   x <- read.scsbio(2020, category = "COM")              # Read commercial snow crab biological data for 2020.
   x <- read.scscat(2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
```
  - September multi-species annual survey.
  - Northumberland Strait annual survey.
```
   x <- read.nssset(2019, experiment = 1)                # Read valid regular survey tows for 2019.
   x <- read.nssbio(2019, species = "american lobster")  # Read biological data for American lobster in 2019.
   x <- read.nsscat(2019, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
```
  
### Probe data:
  - Trawl net acoustic monitoring data (Scanmar, Netmind, eSonar and Notus).
  - Depth-temperature, accelerometer, tilt and magnetometer (Minilog, Star Oddi, Seabird).
  - GPS track data.
  
### Fisheries data (private):
  - Groundfish observer data.
  - Snow crab observer data.
  - Vessel Monitoring System (VMS) data.
  - Logbook data.
  
### Special project data:
  - Lobster collector (Atlantic Lobster Settlement Index) data. 
  - Lobster escapement data.
  - Lobster SCUBA transect survey data.
  - Snow crab fecundity project.
  - Snow crab diet project.
  - Snow crab colorimeter data.
