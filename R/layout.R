#' Local x coordinator
#'
#' @export
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

# Number of tree in layout sample plot
#
seqmeasure <- function(ntree, ncol, start = "left-bottom") {
  nline <- ntree/ncol
  z <- matrix(ncol = ncol, nrow = nline)
  l <- seq(1, ntree, nline)

  switch(start,
         "left-bottom" = for(i in 1:ncol){
           if(i %% 2 == 0) z[,i] = seq(l[i], l[i] + nline-1)
           else z[,i] = seq(l[i] + nline-1, l[i])
         },
         "left-top" = for(i in 1:ncol){
           if(i %% 2 == 1) z[,i] = seq(l[i], l[i] + nline-1)
           else z[,i] = seq(l[i] + nline-1, l[i])
         },
         "right-bottom" = for(i in 1:ncol){
           if(i %% 2 == 0) z[,ncol-i+1] = seq(l[i], l[i] + nline-1)
           else z[,ncol-i+1] = seq(l[i] + nline-1, l[i])
         },
         "right-top" = for(i in 1:ncol){
           if(i %% 2 == 1) z[,ncol-i+1] = seq(l[i], l[i] + nline-1)
           else z[,ncol-i+1] = seq(l[i] + nline-1, l[i])
         }
  )

  return(z)
}
