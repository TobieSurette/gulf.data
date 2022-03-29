#' @title Read Oracle Database
#' 
#' @description Functions to access Oracle data.
#' 
#' @param query SQL query.
#' @param dsn Registered data source.
#' @param uid User identification.
#' @param password User password.

#' @export read.oracle
read.oracle <- function(query, dsn, uid, password, believeNRows = FALSE, ...){
   # Open Oracle channel:
   if (missing(password)) stop("'password' must be specified.")
   channel <- RODBC::odbcConnect(dsn, uid, pwd = password, believeNRows = FALSE)
   
   # Execute query:
   z <- RODBC::sqlQuery(channel, query, stringsAsFactors = FALSE, as.is = TRUE)
   
   RODBC::odbcClose(channel) 
   
   return(z)
}
