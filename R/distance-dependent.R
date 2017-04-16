#' Index of Daniels et al. (1986)
#'
#' @export
dd_daniels <- function(id, x, y, dbh, search = "nearest", n) {

  df <-   join_objtree_compt(id, x, y, dbh, search, n)

  summ <- df %>%
    group_by(id, dbh_id) %>%
    summarise(
      n_comp = n(),
      sum2 = sum(dbh_comp^2, na.rm=TRUE)
    ) %>%
    arrange(as.numeric(id))

  z <- summ %>%
    mutate(z = ((dbh_id^2 * n_comp) / sum2)) %>%
    .$z

  return(z)
}

#' Index of Hegyi (1974)
#'
#' @export
dd_hegyi <- function(id, x, y, dbh, search = "nearest", n) {

  df <- join_objtree_compt(id, x, y, dbh, search, n)

  z <- df  %>%
    mutate(z = dbh_comp/(dbh_id * .dist)) %>%
    group_by(id) %>%
    summarise(z = sum(z, na.rm = any(!is.na(z)))) %>%
    arrange(as.numeric(id)) %>%
    .$z

  return(z)
}

#' Index of Alemdag (1978)
#'
#' @export
dd_alemdag <- function(id, x, y, dbh, search = "nearest", n) {

  df <- join_objtree_compt(id, x, y, dbh, search, n)

  df1 <- df %>%
    mutate(b = (dbh_comp / .dist)) %>%
    group_by(id) %>%
    summarise(b = sum(b, na.rm=TRUE))

  df2 <- df %>%
    mutate(
      a = pi * ((.dist * dbh_id) / (dbh_id + dbh_comp))^2 *
        (dbh_comp / .dist)
    ) %>%
    left_join(df1, by = c("id" = "id"))

  z <- df2 %>%
    group_by(id) %>%
    summarise(z = sum(a / b , na.rm = any(!is.na(a)))) %>%
    arrange(as.numeric(id)) %>%
    .$z

  return(z)
}

#' Index of Matin & Ek (1984)
#'
#' @export
dd_martin <- function(id, x, y, dbh, search = "nearest", n) {
  df <- join_objtree_compt(id, x, y, dbh, search, n)

  z <- df %>%
    mutate(z = (dbh_comp / dbh_id) * exp((16 * .dist) /  (dbh_id / dbh_comp))) %>%
    group_by(id) %>%
    summarise(z = sum(z, na.rm = any(!is.na(z)))) %>%
    arrange(as.numeric(id)) %>%
    .$z

  return(z)
}
