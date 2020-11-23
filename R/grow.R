#' Biological Growth Functions
#' 
#' @description Animal growth can be expressed in various ways. Fish species growth is 
#' generally modeled as length-at-age, while crustacean growth is normally modeled as
#' growth-at-moult size increment, i.e. the expected size increase. An common example 
#' of a fish growth model is the von Bertalanffy curve. For crustaceans, the discrete 
#' nature of growth and maturation frequently induce a stepwise behaviour. Thus, crustacean 
#' growth-at-moult increments are well modeled by a two-component piecewise linear model, 
#' with the first component describing immature growth, which is relatively fast, and the 
#' second component describing adolescent and mature growth, which has a lower relative 
#' growth rate.
#' 
#' @param x Specimen size, length-frequency or other object.
#' @param n Length-frequencies 
#' 
#' @details Crustacean growth-at-moult models have just three parameters, an initial slope
#' parameter, a transition window width (exp(log.w)) and a transition point xp. These 
#' functions generate the expected values for a given set of parameters. Also the assumed
#' error was assumed to be increasing with size.
#' 
#' @return 
#' 
#' If \code{x} is left unspecified, then a function is returned which can be used to 
#' evaluate growth if given inputs.

#' @export grow
grow <- function(x, ...) UseMethod("grow")

#' @export
grow.default <- function(x, ...) return(x + growth(x, ...))

#' @export
grow.scsbio <- function(x, ...) return(grow(x$carapace.width, species = "snow crab", ...))
   
#' @export
grow.nssbio <- function(x, ...){
   v <- grow(x$length, species = "lobster", ...)
   return(v)
}

#' @export
grow.nsslen <- function(x, ...){
   v <- grow(x$length, species = "lobster", ...)
   return(v)
}

