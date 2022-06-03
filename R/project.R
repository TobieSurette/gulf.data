#' @title Project Identifier for Gulf Data
#'
#' @name project
#' 
#' @description Functions to retrieve project identifiers for \sQuote{gulf.set}, \sQuote{gulf.cat},
#'              \sQuote{gulf.bio} and \sQuote{gulf.len} objects.
#'              
#' @param x Data object.
#' @param ... Other arguments.
#'

#' @rawNamespace S3method(project,gulf.set)
project.gulf.set <- function(x, ...) return(survey(x, output = "project", ...))
   
#' @rawNamespace S3method(project,gulf.cat)
project.gulf.cat <- function(x, ...) return(survey(x, output = "project", ...))

#' @rawNamespace S3method(project,gulf.bio)
project.gulf.bio <- function(x, ...) return(survey(x, output = "project", ...))

#' @rawNamespace S3method(project,gulf.len)
project.gulf.len <- function(x, ...) return(survey(x, output = "project", ...))
