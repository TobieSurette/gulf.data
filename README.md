# `gulf.data` Package

The `gulf.data` package contains functions for accessing and manipulating southern Gulf of Saint Lawrence data sets, which include data from:
  - Snow Crab Annual Science Survey
  - Northumberland Strait Annual Science Survey
  - **ALSI** Lobster and rock crab collector (Atlantic Lobster Settlement Index) data.
  - Lobster SCUBA transect survey data.

## Installation:

The `gulf.data` package can be installed directly from GitHub by running the following code from R:
```
library(devtools)
install.packages("sp", dependencies = TRUE)
install.packages("rgdal", dependencies = TRUE)
install.packages("akima", dependencies = TRUE)
install_github("TobieSurette/gulf.utils")
install_github("TobieSurette/gulf.metadata")
install_github("TobieSurette/gulf.data")
```

# Data Access:

## Reading Functions:

Function           | Description
------------------ | --------------------------------------------------
`read.scsset`      | Read snow crab survey set/tow data.
`read.scscat`      | Read snow crab survey by-catch data.
`read.scsbio`      | Read snow crab survey crab biological measurement data.
`read.scslen`      | Read snow crab survey fish length data.
`read.gulf.set`    | Read set/tow data from groundfish surveys.
`read.gulf.cat`    | Read catch summary data from groundfish surveys.
`read.gulf.bio`    | Read biological sampling data from groundfish surveys.
`read.gulf.len`    | Read length-frequency data from groundfish surveys.
`read.oracle`      | Read data from Oracle database.
`read.scobs`       | Read snow crab observer data.
`read.logbook`     | Read fishery logbook data.
`read.ziff`        | Read groundfish and pelagic fishery logbook data (Zonal Interchange File Format data files).
`read.vms`         | Read fishery Vessel Monitoring System data.
`read.minilog`     | Read Minilog probe data.
`read.star.oddi`   | Read Star Oddi probe data.
`read.scanmar`     | Read Scanmar acoustic trawl monitoring data.
`read.netmind`     | Read Netmind acoustic trawl monitoring data.
`read.esonar`      | Read eSonar acoustic trawl monitoring data.
`read.notus`       | Read Notus acoustic trawl monitoring data.
`read.gps`         | Read GPS track data.
`read.colorimeter` | Read colorimeter measurement data.
`read.alsi`        | Read Atlantic Lobster Settlement Index data.
`read.azmp`        | Read oceanographic data from the Atlantic Zonal Monitoring Program.

## Data Field Functions:

Function           | Description
------------------ | --------------------------------------------------
`category`         | Determine or extract animal category data.
`chela.height`     | Determine or extract crab chela height data.
`egg.condition`    | Extract egg condition data.
`gear`             | Extract sampling gear data. 
`grow`             | Predict growth.
`key`              | Extract index key for a data set.
`maturity`         | Determine or extract animal maturity data.
`missing.legs`     | Extract or format crustacean missing leg data.
`sampler`          | Extract or format field sampler data. 
`sex`              | Extract or format biological sex data. 
`species`          | Extract or convert species identification data. 
`tow.id`           | Extract survey tow identification data.
`tow.number`       | Extract survey trawl tow number data.
`trip`             | Extract field trip data. 
`vessel`           | Extract sampling or fishing vessel data.
`weight`           | Extract or calculate catch or sample weight data. 
`wingspread`       | Extract trawl wing spread data.

## Environmental Data Functions:

Function           | Description
------------------ | --------------------------------------------------
`depth`            | Extract elevation and bathymetry data. 
`temperature`      | Extract water temperature data.

## Data Summary Functions:

Function           | Description
------------------ | --------------------------------------------------
`catch`            | Generate catch data summary. 
`freq`             | Generate size-frequency data summary.
`summary`          | Generate data summary.

## Taxonomic, Maturity and Moult Stage Functions:

Function           | Description
------------------ | ------------------------------------------------------------------------------------
`species`          | Extract or convert species identification data. 
`taxon`            | Extract taxonomic identification data. 
`is.category`      | Detemine if snow crab belongs to a specified biological category.
`is.pubescent`     | Determine if animal is pubescent (i.e. immature with developping gonads).
`is.mature`        | Determine if animal is sexually mature.
`is.primiparous`   | Determine if female is primiparous, i.e. is carrying first clutch of eggs.
`is.multiparous`   | Determine if female is multiparous, i.e. is carrying second or more clutch of eggs.
`is.senile`        | Determine if crustacean is sexually senile.
`is.new.shell`     | Determine if crustacean has new shell (i.e. new moult). 
`is.soft.shell`    | Determine if crustacean has a soft shell. 
`is.hard.shell`    | Determine if crustacean has a hard shell (i.e. not soft moult). 
`is.skip.moulter`  | Determine if crustacean has skipped a moult. 
`is.fish`          | Determine if female is multiparous, i.e. is carrying second or more clutch of eggs.
`is.invertebrate`  | Determine if animal is invertbrate.       
`is.shrimp`        | Determine if animal species is a type of shrimp. 

## Metadata tables:

Function           | Description
------------------ | --------------------------------------------------
`glossary`         | Science and data terms glossary data table.
`probe`            | Data probe list and descriptions table.
`project`          | Data project definitions and descriptions table.
`region`           | Administrative regions definitions and descriptions table.
`species`          | Species codes data table.
`species.foreign`  | Foreign species codes data table.
`survey`           | Science surveys data table.

# Examples:

```
# Read snow crab survey data:
x <- read.scsset(year = 2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
x <- read.scsbio(year = 2020, category = "COM")              # Read commercial snow crab biological data for 2020.
x <- read.scscat(year = 2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice catch data for 2020.
x <- read.scslen(year = 2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice size data for 2020.

# Prepare commercial snow crab data:
b <- read.scsbio(year = 2020)                                # Read commercial snow crab biological data for 2020.
s <- read.scsset(year = 2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
import(s, fill = 0) <- catch(b, category = "COM")            # Merge commercial crab catch data to tow data table.

# Import male size-frequency data: 
b <- read.scsbio(2020, category = "M")                       # Read commercial snow crab biological data for 2020.
s <- read.scsset(year = 2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
import(s, fill = 0) <- freq(b, by = c("date", "tow.id"))     # Merge male crab size-frequency data to tow data table.
```
  

