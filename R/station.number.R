#' Return a Station Number
#' 
#' Partition a set of points into groups based on 
#' spatial and/or temporal proximity.
#' 
#' @description Functions to extract or determine trawl tow numbers.
#' 
#' @param longitude A numeric vector. A coordinate vector in decimal degrees.
#' @param latitude A numeric vector. A coordinate vector in decimal degrees.
#' @param time A numeric vector or a vector of a vector of \code{R} time
#' (\sQuote{POSIX}) objects.
#' @param distance.tolerance A numeric value specifying the distance (in
#' kilometers) below which a pair of points is considered to belong to the same
#' group or station.
#' @param time.tolerance A numeric value specifying the time-difference below
#' which a pair of points is considered to belong to the same group or station.
#' The units are in minutes if the \code{time} argument belongs to a
#' \sQuote{POSIX} class.
#'
#' @section Methods:
#' \describe{
#'    \item{\code{station.number}}{Generic \code{station.number} method.}
#'    \item{\code{station.number.default}}{Default \code{station.number} method.}
#' }
#' 

#' @export station.number
station.number <- function(x, ...) UseMethod("station.number")

#' @describeIn station.number Default 'station.number' method.
#' @rawNamespace S3method(station.number,default)
station.number.default <- function(longitude = NULL, latitude = NULL, time = NULL,
                                   distance.tolerance = NULL, ...){
   # Check length of coordinate vectors:
   if (length(longitude) != length(latitude))
      stop("'longitude' and 'latitude' vectors have inconsistent lengths.")
   
   # Check length of time vector:
   if (!is.null(time) & (length(time) != length(longitude)))
      stop("'time' and coordinate vectors must have the same lengths.")
   
   # Take absolute value of tolerance values:
   if (is.null(distance.tolerance)) stop("'distance.tolerance' must be specified.")
   distance.tolerance <- abs(distance.tolerance)
   if (!is.null(time.tolerance)) time.tolerance <- abs(time.tolerance)
   
   # Check whether there are NA values in the coordinates:
   if (any(is.na(longitude) | is.na(latitude))) stop("There are NA values in the input coordinates.")
   
   # Cluster by proximate distance:
   tree <- stats::hclust(stats::dist(gulf.spatial::deg2km(longitude, latitude)))
   if (any(tree$height < distance.tolerance)){
      k <- min(which(rev(tree$height) < distance.tolerance))
      k <- max(k, 2)
      V <- stats::cutree(tree, k = k)
   }else{
      V <- 1:length(longitude)
   }
   
   # Cluster by proximate time:
   if (!is.null(time.tolerance)){
      tree <- stats::hclust(stats::dist(as.numeric(time) / 60))
      if (any(tree$height < time.tolerance)){
         k <- min(which(rev(tree$height) < time.tolerance))
         k <- max(k, 2)
         V <- paste(V, "-", stats::cutree(tree, k = k))
         V <- match(V, unique(V))
      }else{
         V <- 1:length(longitude)
      }
   }
   
   return(V)
}

#' Assign a Station Number
#'
#' Assigns a station number for each set in the September Research Vessel
#' survey.  This function groups sets into unique sampling sites. This function
#' is used to treat various types of repeated samples (e.g. repeat sets,
#' comparative sets, etc...)
#'
#'
#' @param x A \sQuote{gulf.set} object.
#' @param method A character value specifying the method to used for fetching
#' or assigning station numbers. Accepted methods are either observed or
#' hard-coded values (\sQuote{observed}) or latitude-longitude coordinates
#' (\sQuote{calculated}, the default value).
#' @param distance.tolerance A positive numeric value stating the maximum
#' distance in kilometers beyond which proximate tows would not be considered
#' to be at a common sampling station. The default is \code{2}.
#' @param time.tolerance A positive numeric value stating the maximum time
#' interval in minutes beyond which proximate tows would not be considered to
#' be considered as comparative tows. The default is \code{10}.
#' @param list() Further arguments which are passed onto the
#' \code{\link[gulf.data]{station.number}} method.
#' @return A numeric vector of station numbers for each set in \sQuote{x}.
#' @examples
#'
#'    # Read September comparative survey data for 2005:
#'    x <- read.gulf.set(year = 2021, password="R_GulfPKG_19")
#'    x <- x[which(x$experiment != 3),]
#'
#'    # group the sampling stations using hard-coded fixed station assignment:
#'    x$set.number.observed <- station.number(x, method = "observed")
#'
#'    # group the sampling stations using spatio-temporal proximity
#'    x$set.number.calculated <- station.number(x, method = "calculated", distance.tolerance = 2, time.tolerance=10)
#'    
#' @rawNamespace S3method(station.number,gulf.set)
station.number.gulf.set <- function(x, method = "observed", distance.tolerance = 2, time.tolerance=10, ...){
   # Check 'method' argument:
   method <- match.arg(tolower(method), c("observed", "calculated"))
   
   # Define 'cruise' variable:
   cruise <- paste(x$vessel.code, x$cruise.number, sep = "")
   
   # Assign station numbers based on SAS program:
   V <- x$set.number
   if (method == "observed"){
      # 1984: 4 repeat stations (checked visually on a map)
      index <- (x$vessel.code == "P") & (x$cruise.number == "312")
      temp <- c(302, 202,
                402, 202,
                306, 206,
                309, 209,
                310, 210,
                410, 210)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to[from %in% x$set.number[index]]
      
      # Repeat sets for 1983, 1985, 1986 and 1987:
      index <- cruise %in% c("P296", "P327", "H141", "H159", "H179")
      V[index & (x$set.number %in% c(203))] <- 3
      V[index & (x$set.number %in% c(229))] <- 29
      V[index & (x$set.number %in% c(233, 333))] <- 33
      V[index & (x$set.number %in% c(235, 335))] <- 35
      V[index & (x$set.number %in% c(237))] <- 37
      V[index & (x$set.number %in% c(238))] <- 38
      V[index & (x$set.number %in% c(243))] <- 43
      V[index & (x$set.number %in% c(250))] <- 50
      V[index & (x$set.number %in% c(251))] <- 51
      V[index & (x$set.number %in% c(-54, seq(254, 954, by = 100)))] <- 54
      V[index & (x$set.number %in% c(255))] <- 55
      V[index & (x$set.number %in% seq(256, 956, by = 100))] <- 56
      V[index & (x$set.number %in% seq(257, 857, by = 100))] <- 57
      V[index & (x$set.number %in% c(258))] <- 58
      V[index & (x$set.number %in% c(259))] <- 59
      V[index & (x$set.number %in% c(260))] <- 60
      V[index & (x$set.number %in% c(261))] <- 61
      V[index & (x$set.number %in% c(269))] <- 69
      V[index & (x$set.number %in% c(275))] <- 75
      V[index & (x$set.number %in% c(281, 381))] <- 81
      V[index & (x$set.number %in% c(282, 382))] <- 82
      V[index & (x$set.number %in% c(285, 385))] <- 85
      V[index & (x$set.number %in% c(290))] <- 90
      V[index & (x$set.number %in% c(291, 391))] <- 91
      V[index & (x$set.number %in% c(294))] <- 94
      V[index & (x$set.number %in% c(211))] <- 111
      V[index & (x$set.number %in% c(212, 512, 612))] <- 112
      V[index & (x$set.number %in% c(513, 613))] <- 113
      V[index & (x$set.number %in% c(214))] <- 114
      V[index & (x$set.number %in% c(520))] <- 120
      V[index & (x$set.number %in% c(239))] <- 139
      V[index & (x$set.number %in% c(541))] <- 141
      V[index & (x$set.number %in% c(542))] <- 142
      
      # 1988: Repeat sets galore.
      index <- (x$vessel.code == "H") & (x$cruise.number == "192")
      temp <- c( 30,  27,
                 5,   7,
                 150, 146,
                 147, 143,
                 148, 144,
                 149, 145,
                 100,  97,
                 109, 103,
                 115, 114,
                 118, 110,
                 116, 112,
                 117, 111,
                 107, 101,
                 129, 122,
                 128, 121,
                 108, 102,
                 119, 120,
                 141, 134,
                 132, 126,
                 131, 125,
                 130, 124,
                 127, 123,
                 138, 137,
                 140, 135,
                 139, 136,
                 86,  81,
                 98,  94,
                 90,  85,
                 91,  92,
                 87,  82,
                 88,  83,
                 37,  34,
                 89,  84,
                 151, 154,
                 158, 157,
                 159, 156,
                 160, 155,
                 47,  44,
                 161, 163,
                 152, 153,
                 39,  41,
                 40,  42,
                 28,  25,
                 21,  24,
                 19,  23,
                 29,  26,
                 14,  17,
                 18,  22,
                 77,  76,
                 78,  75,
                 79,  74,
                 73,   1,
                 72,   8,
                 2,   9,
                 3,  10,
                 12,  16,
                 13,  69,
                 49,  52,
                 50,  51,
                 57,  54,
                 46,  53,
                 61,  65,
                 62,  66,
                 59,  56,
                 58,  55,
                 60,  64,
                 48,  45)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to[from %in% x$set.number[index]]
      
      # Comparative day-night surveys for 1993:
      index <- (x$vessel.code == "N") & (x$cruise.number == "192")
      temp <- c( 27,  30,
                 5,   7,
                 146, 150,
                 143, 147,
                 144, 148,
                 145, 149,
                 97, 100,
                 103, 109,
                 114, 115,
                 110, 118,
                 112, 116,
                 111, 117,
                 101, 107,
                 122, 129,
                 121, 128,
                 102, 108,
                 119, 120,
                 134, 141,
                 126, 132,
                 125, 131,
                 124, 130,
                 123, 127,
                 137, 138,
                 135, 140,
                 136, 139,
                 81,  86,
                 94,  98,
                 85,  90,
                 91,  92,
                 82,  87,
                 83,  88,
                 34,  37,
                 84,  89,
                 151, 154,
                 157, 158,
                 156, 159,
                 155, 160,
                 44,  47,
                 161, 163,
                 152, 153,
                 39,  41,
                 40,  42,
                 25,  28,
                 21,  24,
                 19,  23,
                 26,  29,
                 14,  17,
                 18,  22,
                 76,  77,
                 75,  78,
                 74,  79,
                 1,  73,
                 8,  72,
                 2,   9,
                 3,  10,
                 12,  16,
                 13,  69,
                 49,  52,
                 50,  51,
                 54,  57,
                 46,  53,
                 61,  65,
                 62,  66,
                 56,  59,
                 55,  58,
                 60,  64,
                 45,  48)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to[from %in% x$set.number[index]]
      
      # Comparative day-night surveys for 1998:
      index <- (x$vessel.code == "N") & (x$cruise.number == "846")
      V[index & (x$set.number == 16)]  <- 12
      V[index & (x$set.number == 17)]  <- 13
      V[index & (x$set.number == 18)]  <- 14
      V[index & (x$set.number == 23)]  <- 19
      V[index & (x$set.number == 25)]  <- 20
      V[index & (x$set.number == 26)]  <- 21
      V[index & (x$set.number == 46)]  <- 41
      V[index & (x$set.number == 47)]  <- 42
      V[index & (x$set.number == 48)]  <- 43
      V[index & (x$set.number == 184)] <- 178
      V[index & (x$set.number == 185)] <- 179
      V[index & (x$set.number == 186)] <- 180
      V[index & (x$set.number == 187)] <- 181
      V[index & (x$set.number == 188)] <- 182
      V[index & (x$set.number == 189)] <- 183
      V[index & (x$set.number == 199)] <- 196
      V[index & (x$set.number == 200)] <- 197
      V[index & (x$set.number == 201)] <- 198
      V[index & (x$set.number == 205)] <- 202
      V[index & (x$set.number == 206)] <- 203
      V[index & (x$set.number == 207)] <- 204
      V[index & (x$set.number == 211)] <- 208
      V[index & (x$set.number == 214)] <- 210
      V[index & (x$set.number == 215)] <- 209
      V[index & (x$set.number == 216)] <- 212
      V[index & (x$set.number == 217)] <- 213
      
      # Comparative day-night surveys for 1999:
      index <- (x$vessel.code == "N") & (x$cruise.number == "941")
      temp <- c(  2,   5,
                  1,   6,
                  3,   7,
                  4,   9,
                  20,  25,
                  21,  26,
                  23,  27,
                  24,  28,
                  37,  42,
                  35,  43,
                  174, 177,
                  175, 178,
                  176, 179,
                  184, 188,
                  185, 189,
                  186, 190,
                  193, 198,
                  196, 199,
                  197, 200,
                  194, 201,
                  195, 202)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to
      
      # Comparative day-night surveys for 2000:
      index <- (x$vessel.code == "N") & (x$cruise.number == "045")
      temp <- c(199, 204,
                200, 205,
                201, 206,
                11,  14,
                10,  12,
                197, 202,
                4,   8,
                2,   6,
                195, 196,
                3,   7,
                5,   9,
                29,  34,
                33,  38,
                32,  37,
                30,  35,
                58,  59,
                49,  50,
                57,  60,
                56,  61,
                31,  36,
                198, 203)
      to <- temp[seq(1, length(temp), by = 2)]
      from <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to
      
      # Comparative survey in 2004:
      index <- (x$vessel.code == "T") & (x$cruise.number == "434")
      temp <- c(7,   2,
                6,   3,
                16,  7,
                18,  9,
                139, 99,
                141, 98,
                136, 104,
                147, 96,
                137, 103,
                146, 97,
                134, 105,
                124, 106,
                126, 113,
                132, 108,
                127, 110,
                129, 111,
                117, 21,
                154, 95,
                113, 19,
                181, 15,
                182, 17,
                175, 20,
                179, 18,
                10,  4,
                5,   5,
                19,  10,
                20,  12,
                12,  6,
                22,  13,
                23,  14)
      from <- temp[seq(1, length(temp), by = 2)]
      to   <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      # V[index & ((x$set.number+300) %in% from)] <- to[from %in% (x$set.number[index]+300)]
      #temp <- c(83:84, 86:93)
      #V[index & (V %in% (temp+300))] <- V[index & (V %in% (temp+300))] - 300
      
      # Comparative survey in 2005:
      index <- (x$vessel.code == "T") & (x$cruise.number == "507")
      temp <- c(121, 221,
                135, 235,
                136, 236,
                137, 237,
                138, 238,
                139, 239,
                140, 240,
                141, 241,
                142, 242,
                143, 243)
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      #V[index & (x$set.number %in% from)] <- to[from %in% x$set.number[index]]
      
      # Comparative survey in 2021:
      index <- (x$vessel.code == "C") & (x$cruise.number == "222")
      temp <- c(
         87, 110,
         95, 117,
         160, 183,
         161, 184,
         159, 182,
         149, 172,
         148, 170,
         147, 169,
         19, 11,
         17, 13,
         18, 14,
         26, 40,
         25, 39,
         27, 41,
         40, 56,
         38, 53,
         32, 48,
         61, 78,
         62, 79,
         63, 80,
         64, 81,
         65, 82,
         69, 86,
         56, 73,
         58, 75,
         60, 77,
         72, 89,
         74, 91,
         71, 88,
         68, 85,
         67, 84,
         15, 37,
         81, 103,
         82, 104,
         49, 65,
         78, 98,
         84, 107,
         85, 108,
         76, 95,
         75, 93,
         42, 58,
         43, 59,
         44, 60,
         51, 67,
         50, 66,
         53, 69,
         54, 70,
         55, 71,
         2, 22,
         45, 61,
         47, 63,
         16, 38,
         9, 30,
         79, 101,
         80, 102,
         92, 114,
         93, 115,
         90, 113,
         89, 112,
         94, 116,
         86, 109,
         88, 111,
         105, 127,
         106, 128,
         98, 120,
         99, 121,
         97, 119,
         96, 118,
         157, 180,
         156, 179,
         150, 173,
         146, 168,
         145, 167,
         144, 166,
         151, 174,
         153, 176,
         152, 175,
         154, 177,
         155, 178,
         111, 133,
         110, 132,
         104, 126,
         109, 131,
         107, 129,
         141, 163,
         139, 161,
         140, 162,
         114, 136,
         113, 135,
         102, 124,
         103, 125,
         122, 144,
         115, 137,
         132, 154,
         133, 155,
         112, 134,
         124, 146,
         129, 151,
         131, 153,
         130, 152,
         136, 158,
         138, 160,
         120, 142,
         123, 145,
         126, 148,
         119, 141,
         121, 143,
         125, 147,
         128, 150,
         127, 149,
         162, 185,
         41, 57,
         33, 49,
         70, 87,
         100, 122,
         143, 165,
         158, 181,
         137, 159,
         116, 138,
         134, 156,
         142, 164,
         34, 51,
         39, 54,
         66, 83,
         5, 26,
         11, 32,
         163, 186,
         101, 123,
         135, 157,
         118, 140
      )
      from <- temp[seq(1, length(temp), by = 2)]
      to <- temp[seq(2, length(temp), by = 2)]
      s <- match(x$set.number[index], from)
      V[index][!is.na(s)] <- to[s[!is.na(s)]]
      
      
      # Additionnal station number reassignments:
      index <- (x$year %in% 1984:1987)
      V[index & (cruise %in% c("H159", "H179")) & (V == 117)] <- 1017
   }
   
   # Group sets using observed coordinates:
   if (method == "calculated"){
      years <- sort(unique(year(x)))
      V <- rep(NA, nrow(x))
      for (i in 1:length(years)){
         index <- which((year(x) == years[i]) & (x$experiment != 3))
         time <- as.POSIXct(paste0(x[index, "date"], " ", x[index, "start.time"]))
         V[index] <- station.number(gulf.spatial::longitude(x[index, ]), gulf.spatial::latitude(x[index, ]), time = time ,
                                    distance.tolerance = distance.tolerance, time.tolerance = time.tolerance)
      }
   }
   
   return(V)
}
