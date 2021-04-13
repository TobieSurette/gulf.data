# `gulf.data` Package

Functions for accessing and manipulating southern Gulf of Saint Lawrence data

## Data Sets:

A number of data tables are included in the package.

### Science Survey Data:
  - North Atlantic species names and corresponding numeric codes (e.g. STACAC, NAFO, ...).  
  - Research project description table.
  - Research survey description table.
  - Survey vessel description table.
  - Snow crab survey tow/set, biological and by-catch data.
  - Northumberland Strait survey set, catch and biological data.
  
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

## Data Access:

The package contains functions to locate, access and manipulatte southern Gulf of Saint Lawrence research data.

### Reading Functions:

Function           | Description
------------------ | --------------------------------------------------
`read.scsset`      | Read snow crab survey set/tow data.
`read.scscat`      | Read snow crab survey by-catch data.
`read.scsbio`      | Read snow crab survey crab biological measurement data.
`read.scslen`      | Read snow crab survey fish length data.
`read.nssset`      | Read Northumberland Strait survey set/tow data.
`read.nsscat`      | Read Northumberland Strait survey catch data.
`read.nssbio`      | Read Northumberland Strait survey crab biological measurement data.
`read.nsslen`      | Read Northumberland Strait survey animal length data.
`read.minilog`     | Read Minilog probe data.
`read.star.oddi`   | Read Star Oddi probe data.
`read.gps`         | Read GPS track data.
`read.esonar`      | Read eSonar acoustic probe data.
`read.notus`       | Read Notus acoustic probe data.
`read.colorimeter` | Read colorimeter measurement data.
`read.alsi`        | Read Atlantic Lobster Settlement Index data.

### Data Field Functions:

Function           | Description
------------------ | --------------------------------------------------
`maturity`         | Read snow crab survey set/tow data.
`category`         | Read snow crab survey set/tow data.
`sex`              | Extract or format biological sex data. 
`weight`           | Extract or calculate catch or sample weight data. 
`sampler`          | Extract or format field sampler data. 
`trip`             | Extract field trip data. 
`gear`             | Extract sampling gear data. 
`grow`             | Predict growth.
`key`              | Extract index key for a data set.
`egg.condition`    | Extract egg condition data.
`vessel`           | Extract sampling or fishing vessel data.
`tow.id`           | Extract survey tow identification data.
`wingspread`       | Extract trawl wing spread data.

### Data Summary Functions:

Function           | Description
------------------ | --------------------------------------------------
`catch`            | Generate catch data summary. 
`freq`             | Generate size-frequency data summary.

### Moult stage Functions:

Function           | Description
------------------ | ---------------------------------------------------------------
`is.new.shell`     | Determine if crustacean has new shell (i.e. new moult). 
`is.soft.shell`    | Determine if crustacean has a soft shell. 
`is.hard.shell`    | Determine if crustacean has a hard shell (i.e. not soft moult). 
`is.skip.moulter`  | Determine if crustacean has skipped a moult. 

## Examples:

   ```
      x <- read.scsset(2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
      x <- read.scsbio(2020, category = "COM")              # Read commercial snow crab biological data for 2020.
      x <- read.scscat(2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
      x <- read.scslen(2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
   ```
  - September multi-species annual survey.
  - Northumberland Strait annual survey.
  ```
     x <- read.nssset(2019, experiment = 1)                # Read valid regular survey tows for 2019.
     x <- read.nssbio(2019, species = "american lobster")  # Read biological data for American lobster in 2019.
     x <- read.nsscat(2019, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
   ```
  

