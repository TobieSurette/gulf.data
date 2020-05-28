# gulf.data

Functions for Accessing and Manipulating sGSL Data

There are different types of data sources:

* **Survey data**:
  - Snow crab sannual survey
  - September multi-species annual survey 
  - Northumberland Strait annual survey
  
* **Probe data**:
  - Trawl acoustic monitoring data (Scanmar, Netmind, eSonar and Notus)
  - Depth / temperature (Minilog, Star Oddi, Seabird)
  - GPS data
  
* **Observer at-sea-sampling**
  - Groundfish observer data
  - Snow crab observer data
  
* **Project data**:
  - Lobster collector data
  - Lobster escapement data
  - Lobster SCUBA transect survey data
  - Snow crab fecundity project
  - Snow crab diet project
  - Snow crab colorimeter data

### Metadata functions:

These are functions to define or retrieve various types of metadata associated with a data object.

Function      | Description
------------- | ------------------------------------------------------------------------
`metadata`    | Retrieve metadata.
`key`         | Set or retrieve an index key for a data table.
`description` | Set or retrieve a text description(s) for a data table or variable field.
`format`      | Set or retrieve the format defintion of a variable field.
`units`       | Set or retrieve the units of a variable field.
