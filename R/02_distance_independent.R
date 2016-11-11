#' Distance-independent Steneker & Jarvis (1963) index
#'
#' Sum of the basal area (g) of the neighbouring trees j 
#' for a subject tree i (m2 ha-1)
#' 
#' @export
#' 
di_steneker <- function(dbh, area) {
  ba <- (pi*dbh^2)/(4*area)
  z <- sum(ba, na.rm = TRUE) - ba
  
  return(z)
}

#' Distance-independent Wikoff (1982) index
#'
#' Sum of the basal area of trees larger than the subject tree (m2 ha-1)
#' 
#' @export
#' 
di_wikoff <- function(dbh, area) {
  ba <- (pi*dbh^2)/(4*area)

  z <- vector(length = length(ba))
  for (i in seq_along(dbh)) {
    if (!is.na(ba[i])) {
      z[i] <- sum(ba[ba[i] < ba], na.rm = TRUE)
    } else {
      z[i] <- NA
    }
  }
  
  return(z)
}

#' Distance-independent Lorimer (1982) index
#'
#' Sums up the d of neighbours divided by the subject
#' tree d in the plot (ha-1)
#' 
#' @export
#' 
di_lorimer <- function(dbh) {
  z <- vector(length = length(dbh))
  for (i in seq_along(dbh)) {
    if (!is.na(dbh[i])) {
      z[i] <- sum(dbh[-i], na.rm = TRUE) / dbh[i]
    } else {
      z[i] <- NA
    }
  }
  
  return(z)
}

#' Distance-independent Hamilton (1986) index
#'
#' Ratio of the diameter of the subject tree to the quadratic mean diameter
#' 
#' @export
#' 
di_hamilton <- function(dbh) {
  dg <- sqrt(mean(dbh^2, na.rm = TRUE))
  z <- dbh / dg

  return(z)
}

#' Distance-independent Corrona & Ferrara (1989)
#'
#' Sums up the basal area of neighbours divided by the subject
#' tree basal area in the plot (ha-1)
#' 
#' @export
#' 
di_corrona <- function(dbh, area) {
  ba <- (pi*dbh^2)/(4*area)
  
  z <- vector(length = length(dbh))
  for (i in seq_along(dbh)) {
    if (!is.na(dbh[i])) {
      z[i] <- sum(ba[-i], na.rm = TRUE) / ba[i]
    } else {
      z[i] <- NA
    }
  } 
  
  return(z)
}

#' Distance-independent Vanclay (1991)
#' 
#' Ratio of basal area of trees larger than the subject tree
#' to basal area of the plot
#' 
#' @export
#' 
di_vanclay <- function(dbh, area) {
  ba <- (pi*dbh^2)/(4*area)
  g <- sum(ba, na.rm = TRUE)
  
  z <- vector(length = length(dbh))
  for (i in seq_along(dbh)) {
    if (!is.na(dbh[i])) {
      z[i] <- sum(ba[ba[i] < ba], na.rm = TRUE) / g
    } else {
      z[i] <- NA
    }
  } 
  
  return(z)
}