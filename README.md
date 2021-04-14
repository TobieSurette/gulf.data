# `gulf.data` Package

The `gulf.data` package contains functions for accessing and manipulating southern Gulf of Saint Lawrence data sets, which include data from:
  - Snow Crab Annual Science Survey
  - Northumberland Strait Annual Science Survey
  - **ALSI** Lobster and rock crab collector (Atlantic Lobster Settlement Index) data.
  - Lobster SCUBA transect survey data.

# Data Access:

## Reading Functions:

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
`read.esonar`      | Read eSonar acoustic trawl probe data.
`read.notus`       | Read Notus acoustic trawl probe data.
`read.colorimeter` | Read colorimeter measurement data.
`read.alsi`        | Read Atlantic Lobster Settlement Index data.

## Data Field Functions:

Function           | Description
------------------ | --------------------------------------------------
`category`         | Determine or extract animal category data.
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
`trip`             | Extract field trip data. 
`vessel`           | Extract sampling or fishing vessel data.
`weight`           | Extract or calculate catch or sample weight data. 
`wingspread`       | Extract trawl wing spread data.

## Data Summary Functions:

Function           | Description
------------------ | --------------------------------------------------
`catch`            | Generate catch data summary. 
`freq`             | Generate size-frequency data summary.
`summary`          | Generate data summary.

## Moult Stage Functions:

Function           | Description
------------------ | ---------------------------------------------------------------
`is.new.shell`     | Determine if crustacean has new shell (i.e. new moult). 
`is.soft.shell`    | Determine if crustacean has a soft shell. 
`is.hard.shell`    | Determine if crustacean has a hard shell (i.e. not soft moult). 
`is.skip.moulter`  | Determine if crustacean has skipped a moult. 

## Metadata tables:

Function           | Description
------------------ | --------------------------------------------------
`glossary`         | Science and data terms glossary data table.
`project`          | Data project definitions and descriptions data table.
`species`          | Species codes data table.
`species.foreign`  | Foreign species codes data table.
`survey`           | Science surveys data table.

# Examples:

```
# Read snow crab survey data:
x <- read.scsset(2020, valid = 1, survey = "regular") # Read valid regular survey tows for 2020.
x <- read.scsbio(2020, category = "COM")              # Read commercial snow crab biological data for 2020.
x <- read.scscat(2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
x <- read.scslen(2020, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
  
# Read Northumberland Strait survey data:  
x <- read.nssset(2019, experiment = 1)                # Read valid regular survey tows for 2019.
x <- read.nssbio(2019, species = "american lobster")  # Read biological data for American lobster in 2019.
x <- read.nsscat(2019, species = c("cod", "plaice"))  # Read Atlantic cod and American plaice data for 2020.
```
  

