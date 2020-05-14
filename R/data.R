#' Reference and Example Data Sets
#' 
#' @description These are the reference and example data used in the \code{gulf.data} package.
#' 
#' @name gulf.data data
#' @aliases Minilog example
#' @aliases species.table species.table.foreign
#' 
#' @section \code{Minilog example.txt}: 
#' A sample Minilog text file with a header, field names, and temperature observations.
#' 
#' A Minilog text file with the following variables:
#' \describe{
#'   \item{date}{Date when the measurement was made.}
#'   \item{time}{Time when the measurement was made.}
#'   \item{temperature}{Temperature in degrees Celsius.}
#' }
#' 
#' @section \code{species.csv}: 
#' Species Names and Codes of Atlantic fish and invertebrate species used in DFO the southern Gulf and 
#' Nova Scotia DFO science surveys.
#' 
#' A \sQuote{csv} (comma-separated) file with the following variables:
#' \describe{
#'   \item{code}{Numeric species codes used in DFO's science reasearch surveys.}
#'   \item{name_en}{English species names.}
#'   \item{name_fr}{French species names.}
#'   \item{name_latin}{Latin species names.}
#' }
#'   
#' @section \code{species.foreign.csv}:
#' Species Names and Codes of Atlantic fish and invertebrate species used by other fishery groups, such 
#' as the North American Fisheries Organisation and the Statistical Coordinating Committee for the Atlantic 
#' Coast (STACAC).
#' 
#' A \sQuote{csv} (comma-separated) file with the following variables:
#' \describe{
#'   \item{stacac}{Statistical Coordinating Committee for the Atlantic Coast numeric species codes.}
#'   \item{nafo}{North American Fisheries Organization numeric species codes.}
#'   \item{rv}{DFO Science research survey numeric species codes.}
#'   \item{name_en}{English species names.}
#'   \item{name_fr}{French species names.}
#' }
#' 
#' @seealso \code{\link{gulf-data}}
#' @seealso \code{\link{species}}
NULL
