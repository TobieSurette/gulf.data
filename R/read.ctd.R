#' @title Read CTD data
#'
#' @description 
#' 
#' Read Conductivity-Temperature-Depth (CTD) data directly from the Institut Maurice-Lamontagne (IML) SGDOP data base. 
#' In particular, the database contains the oceanographic data collected during science surveys in the southern Gulf
#' of St. Lawrence. Oracle client needs to be set up in ODBC sources.
#'
#' @aliases read.ctd
#' 
#' @param year      Survey or study year.
#' @param survey    Character string specifying which survey to target. Available codes are the Septmeber survey (\code{rvs}), the snow crab survey (\code{scs}), 
#'                  and the Northumberland Strait survey (\code{nss}). The default returns data from all available surveys.
#' @param table     Name of data table (\code{ctd}, \code{bottle} or \code{metadata}).
#' @param dsn       Oracle data source name (= \code{SGDOP}). 
#' @param username  Oracle user name. 
#' @param password  Oracle password. 
#' @param echo      Logical value specifying whether to return messages during data retrieval.

#' @export read.ctd
read.ctd <- function(year, survey = c("rvs", "scs", "nss"), table = "ctd", dsn = "SGDOP", username = "SGDO_lecture", password, echo = TRUE){
   # Start timer:
   start_time <- Sys.time()
   
   # Check that passwoird is specified:
   
   # Parse 'year' argument:
   if (missing(year)) stop("'year' must be specified.")
   if ((length(year) != 1) | !is.numeric(year)) stop("A single 'year' value must be specified.")
   
   # Parse 'table' argument:
   table <- match.arg(tolower(table), c("ctd", "bottle", "bouteille", "metadata"))
   if (table == "bouteille") table <- "bottle"

   # Parse 'survey' argument:
   for (i in 1:length(survey)){
      survey[i] <- match.arg(tolower(survey[i]), c("rvs", "scs", "nss", "september survey", "snow crab survey", "northumberland strait survey"))
   }
   survey[survey == "september survey"] <- "rvs"
   survey[survey == "snow crab survey"] <- "scs"   
   survey[survey == "northumberland strait survey"] <- "nss"  
   
   # Define September survey mission ID names:
   rvs.missions <- list("1998" = "NED1998046",
                        "1999" = "NED1999041",
                        "2000" = "NED2000045",
                        "2001" = "NED2001050",
                        "2002" = "NED2002051",
                        "2003"=  "NED2003052",
                        "2004" = "TEL2004534",
                        "2005" = "TEL2005607",
                        "2006" = "TEL2006678",
                        "2007" = "TEL2007749",
                        "2008" = "TEL2008815",
                        "2009" = "TEL2009892",
                        "2010" = "TEL2010974",
                        "2011" = "TEL2011094",
                        "2012" = "TEL2012105",
                        "2013" = "TEL2013118",
                        "2014" = "TEL2014133",
                        "2015" = "TEL2015133",
                        "2016" = "TEL2016161",
                        "2017" = "TEL2017177",
                        "2018" = "TEL2018196",
                        "2019" = "TEL2019201",
                        "2020" = "TEL2020211",
                        "2021" = "CJC2021222",
                        "2022" = "CAR2022025",
                        "2023" = "CAR2023300")
   
   # Define snow crab survey mission ID names:
   scs.missions <- list("2009" = "GLFSCTSD062009",
                        "2009" = "GLFSCTSD092009",
                        "2010" = "GLFSCTSD092010",
                        "2011" = "GLFSCTSD092011",
                        "2012" = "GLFSCTSD092012",
                        "2013" = "GLFSCTSD092013",
                        "2014" = "GLFSCTSC092014",
                        "2015" = "GLFSCTD2015",
                        "2016" = "GLFSCTD2016",
                        "2017" = "GLFSCTD2017",
                        "2018" = "GLFSCTD2018",
                        "2019" = "GLFSCTD2019",
                        "2020" = "GLFSCTD2020",
                        "2021" = "GLFSCTD2021",
                        "2022" = "GLFSCTD2022",
                        "2023" = "GLFSCTD2023")
   
   # Define Northumberland Strait survey mission ID names:
   nss.missions <- list("2009" = "CADDY2009",
                        "2010" = "CADDY2010",
                        "2011" = "OPL_2011_029",
                        "2012" = "OPL-2012-026",
                        "2012" = "OPL-2012-048",
                        "2013" = "PER2013126",
                        "2014" = "PER2014021",
                        "2015" = "PER2015021",
                        "2016" = "PER2016018",
                        "2017" = "PER2017024",
                        "2018" = "PER2018041",
                        "2019" = "PER2019140",
                        "2020" = "PER2020150",
                        "2021" = "PER2021151",
                        "2022" = "PER2022152",
                        "2023" = "PER2023303")
   
   # Return the whole list if year is unspecified:
   missions <- NULL
   if ("rvs" %in% survey) missions <- c(missions, unlist(rvs.missions[year == names(rvs.missions)]))
   if ("scs" %in% survey) missions <- c(missions, unlist(scs.missions[year == names(scs.missions)]))
   if ("nss" %in% survey) missions <- c(missions, unlist(nss.missions[year == names(nss.missions)]))
   names(missions) <- NULL
   missions <- unique(missions)
   
   # Check that mission was found:
   if (length(missions) == 0) stop("No valid survey missions found.")
   
   # need single quotes for Oracle query
   missions <- paste0("'", paste0(missions, collapse = "','"), "'")
   
   # Define query:
   query <- "select jd.*,jd.STAT_JD as SET_NO"
   if(table == "ctd")      query <- paste0(query, ", db.* from DONNEES_CTD db INNER JOIN JEU_DONNEES jd ON db.SEQ_JD = jd.SEQ_JD where jd.ACR_MISS in (",missions,");")
   if(table == "bottle")   query <- paste0(query, ", db.* from DONNEES_BOUTEILLES db INNER JOIN JEU_DONNEES jd ON db.SEQ_JD = jd.SEQ_JD where jd.ACR_MISS in (",missions,");")
   if(table == "metadata") query <- paste0(query, " from JEU_DONNEES jd where ACR_MISS in (",missions,");")

   # Perform query:
   res <- read.oracle(query, dsn = dsn, uid = username, password = password, believeNRows = FALSE)  
   if (echo) cat(paste0(nrow(res), " records were returned.\n"))
   
   # Rename and reformat variables:
   names(res) <- tolower(names(res))
   names(res) <- gsub("^bath$", "bathymetry", names(res))
   names(res) <- gsub("^set_no$", "set.number", names(res))   
   names(res) <- gsub("^deph$", "depth", names(res))    
   names(res) <- gsub("^lond$", "longitude", names(res))  
   names(res) <- gsub("^latd$", "latitude", names(res)) 
   names(res) <- gsub("^acr_miss$", "mission.id", names(res))  
   names(res) <- gsub("^no_ligne$", "record.number", names(res)) 
   names(res) <- gsub("^sytm$", "date", names(res))  
   names(res) <- gsub("^pht_$", "pH", names(res))  
   names(res) <- gsub("^ph$", "pH", names(res))  
   names(res) <- gsub("^trb_$", "turbidity", names(res))     
   names(res) <- gsub("^flor$", "fluorescence", names(res))     
   names(res) <- gsub("^doxy$", "oxygen", names(res))  
   names(res) <- gsub("^dens$", "density", names(res))    
   names(res) <- gsub("^psal$", "salinity", names(res)) 
   names(res) <- gsub("^te90$", "temperature", names(res)) 
   names(res) <- gsub("^cdom$", "fluorescence.organic", names(res)) 
   names(res) <- gsub("^desc_jd$", "description", names(res))  
   names(res) <- gsub("^typ_jd$", "data.type", names(res))  
   names(res) <- gsub("^acr_nom_int_prod$", "contact.name", names(res))  
   names(res) <- gsub("^amon$", "ammonia", names(res))   
   names(res) <- gsub("^atp_$", "adenosine.triphosphate", names(res))   
   names(res) <- gsub("^ntri$", "nitrite", names(res))  
   names(res) <- gsub("^ntra$", "nitrate", names(res)) 
   names(res) <- gsub("^ntrz$", "nitrite.nitrate", names(res)) 
   names(res) <- gsub("^phos$", "phosphate", names(res)) 
   names(res) <- gsub("^slca$", "silica", names(res)) 
   names(res) <- gsub("^pha_$", "pheopigment", names(res)) 
   names(res) <- gsub("^pim_$", "particulate.inorganic.matter", names(res)) 
   names(res) <- gsub("^poc_$", "particulate.organic.carbon", names(res)) 
   names(res) <- gsub("^pom_$", "particulate.organic", names(res)) 
   names(res) <- gsub("^pon_$", "particulate.organic.nitrogen", names(res))                                                                  
   names(res) <- gsub("^doc_$", "dissolved.organic.carbon", names(res)) 
   names(res) <- gsub("^ppr_$", "primary.production", names(res)) 
   names(res) <- gsub("^tsm_$", "total.suspended.matter", names(res)) 
   names(res) <- gsub("^ure_$", "urea", names(res)) 
   names(res) <- gsub("^pht_$", "pH.sea", names(res)) 
   names(res) <- gsub("^ticw$", "total.dissolved.inorganic.carbon", names(res)) 
   names(res) <- gsub("^n2o_$", "nitrous.oxide", names(res))
   names(res) <- gsub("^secc$", "secchi.depth", names(res))
   names(res) <- gsub("^alkw$", "alkalinity", names(res))
   names(res) <- gsub("^typ_jd$", "device", names(res))
   names(res)[grep("^lat_1", names(res))] <- "latitude.start"
   names(res)[grep("^lat_2", names(res))] <- "latitude.end"
   names(res)[grep("^lngt_1", names(res))] <- "longitude.start"
   names(res)[grep("^lngt_2", names(res))] <- "longitude.end"
   names(res)[grep("^nom_fich", names(res))] <- "file.name"
   if ("date" %in% names(res)){
      res$time <- unlist(lapply(strsplit(res$date, " "), function(x) x[2]))
      res$date <- unlist(lapply(strsplit(res$date, " "), function(x) x[1])) 
   }

   # Remove irrelavant data fields:
   remove <- names(res)[grep("_", names(res))]
   res <- res[setdiff(names(res), remove)]

   # Re-order fields:
   vars <- c("date", "time", "set.number")
   vars <- vars[vars %in% names(res)]
   res <- res[c(vars, setdiff(names(res), vars))]
   
   # Report time to execute query:
   if (echo){
      cat("Query took ", round(difftime(Sys.time(), start_time, units = "secs"), 1), " seconds.\n", sep = "")
   }
   
   return(res)
}