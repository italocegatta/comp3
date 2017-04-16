# Search competitor
#
search_comp <- function(id, x, y, search = "nearest", n = 6) {

  if (search == "nearest") {
    compts <- search_nearest(id, x, y, n)
  } else {
    compts <- search_dfixed(id, x, y, n)
  }

  return(compts)

}

# Search potential competitor around fixed distance
#
search_dfixed <- function(id, x, y, dist) {
  .class <- if (class(id) == "factor") {
    "character"
  } else {
    class(id)
  }

  m <- as.matrix(dist(data.frame(x, y)))

  rownames(m) <- id; colnames(m) <- id

  z <- as.data.frame(m) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = id) %>%
    tidyr::gather(competitor, .dist, -id) %>%
    dplyr::filter(
      .dist != 0,
      .dist <= dist
    ) %>%
    dplyr::arrange(id, .dist) %>%
    dplyr::mutate(
      id = 'class<-'(id, .class),
      competitor = 'class<-'(competitor, .class)
    )

  return(z)
}

# Search n nearest potential competitor
#
search_nearest <- function(id, x, y, nearest) {
  .class <- if (class(id) == "factor") {
    "character"
  } else {
    class(id)
  }

  m <- as.matrix(dist(data.frame(x, y)))

  rownames(m) <- id; colnames(m) <- id

  z <- as.data.frame(m) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = id) %>%
    tidyr::gather(competitor, .dist, -id) %>%
    dplyr::filter(.dist != 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(.rank = dplyr::min_rank(.dist)) %>%
    dplyr::arrange(id, .rank) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.rank <= nearest) %>%
    dplyr::select(-.rank) %>%
    dplyr::mutate(
      id = 'class<-'(id, .class),
      competitor = 'class<-'(competitor, .class)
    )

  return(z)
}

# get classe to set all columns in the same object-class
#
get_class <- function(x) {
  if (class(x) == "factor") {
    "character"
  } else {
    class(x)
  }
}

# Join Object-tree with competitor-tree in the same data.frame
#
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
