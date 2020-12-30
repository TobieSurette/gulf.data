#' Groundfish Sampling?
#' 
#' @description Returns whether a survey tow was length-sampled for groundfish.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2010:2020)
#' is.groundfish.sample(x)

#' @export is.groundfish.sample
is.groundfish.sample <- function(x, ...){
   UseMethod("is.groundfish.sample")
}
  
#' @rawNamespace S3method(is.groundfish.sample,scsset)
is.groundfish.sample.scsset <- function(x, ...){
   years <- sort(unique(year(x)))
   y <- read.scslen(years)
   y <- unique(y[key(x)])
   v <- rep(FALSE, nrow(x))
   ix <- gulf.utils::match(y[key(x)], x[key(x)])
   v[ix] <- TRUE
   
   # Special treatment for 2019 to remove groundfish sampled measured during comparative experiment: 
   ix <- year(x) == 2019
   if (any(ix)){
      tows <- c('GP002','GP007','GP008','GP010','GP013','GP016','GP022','GP027','GP037','GP041','GP044',
                'GP046','GP047','GP052','GP061','GP065','GP071','GP076','GP078','GP082','GP083','GP089',
                'GP095','GP096','GP099','GP100','GP102','GP103','GP106','GP107','GP110','GP114','GP117',
                'GP120','GP122','GP123','GP128','GP129','GP135','GP136','GP137','GP142','GP147','GP148',
                'GP150','GP158','GP160','GP161','GP163','GP169','GP173','GP175','GP179','GP181','GP188',
                'GP190','GP195','GP198','GP200','GP201','GP216','GP217','GP218','GP223','GP225','GP227',
                'GP230','GP233','GP240','GP241','GP246','GP247','GP258','GP260','GP261','GP272','GP282',
                'GP290','GP292','GP295','GP303','GP304','GP305','GP306','GP309','GP311','GP312','GP314',
                'GP317','GP319','GP321','GP322','GP323','GP331','GP333','GP339','GP340','GP346','GP350','GP353')
      
      v[ix] <- FALSE
      ix <- which(ix & (substr(x$tow.id,3,5) %in% substr(tows,3,5)) & (x$valid == 1) & (substr(x$tow.id,2,2) != "C"))
      v[ix] <- TRUE
   }
   
   return(v)
}
