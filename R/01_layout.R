#' Local x coordinator
#'
#' @examples
#' 
#' #library(ggplot2)
#' #library(dplyr)
#' #
#' #d <- data_frame(a=1:80) %>% 
#' #mutate(
#' # x = xcoord(a, start = 'left-bottom'),
#' # y = ycoord(a, start = 'left-bottom')
#' #) 
#' #
#' #ggplot(d, aes(x, y, label = a)) +
#' # geom_text()
#'
#' @export
#' 
xcoord <- function(x, xspacing = 3, ncol = 8,start = "left-bottom") {
  n <- length(x)
  m <- seqmeasure(n, ncol, start)
  z <- vector(length = n)
  
  for (i in seq_len(n)){
    z[i] <- which(m == i, arr.ind = T)[2] * xspacing - xspacing
  }
  
  return(z)
}

#' Local x coordinator
#' 
#' @export
#' 
ycoord <- function(x, yspacing = 3, ncol = 8, start = "left-bottom") {
  n <- length(x)
  m <- seqmeasure(n, ncol, start)
  w <- n/ncol
  z <- vector(length = n)
  
  for (i in seq_len(n)){
    z[i] <- (w - which(m == i, arr.ind = T)[1])  * yspacing
  }
  
  return(z)
}