# @importFrom magrittr %>%
#NULL

#' Number of tree in layout sample plot
#'
#'
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

#' Search competitor
#' @export
#'
search_comp <- function(id, x, y, dbh, search, n) {

  if (search == "nearest") {
    compts <- search_nearest(id, x, y, n)
  } else {
    compts <- search_dfixed(id, x, y, n)
  }

}

#' get classe to set all columns in the same object-class
#'
#'
get_class <- function(x) {
  if (class(x) == "factor") {
    "character"
  } else {
    class(x)
  }
}

#' Join Object-tree with competitor-tree in the same data.frame
#'
#'
join_objtree_compt <- function(id, x, y, dbh, search, n) {

  compts <- search_comp(id, x, y, dbh, search, n)

  .class <- get_class(id)

  aux <- dplyr::data_frame(id = 'class<-'(id, .class), dbh)

  df <- compts %>% dplyr::left_join(
    aux,
    by = c("competitor" = "id")
    ) %>%
    dplyr::arrange(as.numeric(id)) %>%
    left_join(aux, by = c("id" = "id")) %>%
    rename(dbh_id = dbh.y, dbh_comp = dbh.x)

  return(df)
}

