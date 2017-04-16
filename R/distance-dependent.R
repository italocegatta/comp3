#' Distance dependent index
#'
#' These are functions to compute some distante dependente indexs.
#' each index is named that author who propose.
#'
#' @name dd
#'
#' @param id tree identification
#' @param x, x numeric vector of tree coordinates.
#' @param dbh diameter of trees at 1.3 meters.
#' @param search method to search competitive trees. Use "nearest" to find
#' the \code{n} trees nearest of object tree or "dfixed" to find any tree around
#' of \code{n} distance.
NULL

#' Daniels index
#'
#' Sum of circular segments calculated between
#' the object tree and its competitors.
#'
#' @inheritParams dd
#'
#' @references DANIELS, R. F.; BURKHART, H. E.; CLASON, T. R. A comparison of
#' competition measures for predicting growth of loblolly pine trees. Canadian
#' Journal of Forest Research, v. 16, n. 6, p. 1230–1237, dez. 1986.
#'
#' @export
dd_daniels <- function(id, x, y, dbh, search = "nearest", n = 6) {

  df <- join_objtree_compt(id, x, y, dbh, search, n)

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

#' Hegyi index
#'
#' Sum of the competitor's diameter divided by the distance and diameter of the
#' object tree.
#'
#' @inheritParams dd
#'
#' @references HEGYI F. A simulation model for managing jack-pine stands.
#' In: FRIES G, editor. Growth models for tree and stand population.
#' Stockolm: Royal College of Forestry; 1974. p. 74-90.
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

#' Alemdag index
#'
#' Sum of circular segments calculated between the object tree and
#' its competitors.
#'
#' @inheritParams dd
#'
#' @references ALEMDAG, I.S., 1978. Evaluation of some competition indexes
#' for the predictions of diameter increment in planted white spruce. Inf.
#' Rep. FMR-X-108, Canadian Forestry Management Institute, Ottawa, Ont., 39 pp.
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

#' Matin & Ek (1984) index
#'
#' Sum of the ratio between the diameter of the competitor and
#' object tree multiplied by the exponential factor weighted by
#' the distance and diameter of the trees.
#'
#' @inheritParams dd
#'
#' @references MARTIN, G.L.; EK, A.R., 1984. A comparison of competition
#' measures and growth models for predicting plantation red pine diameter
#' and height growth. Forest Science 30(3): 731-743.
#'
#' @export
dd_martin <- function(id, x, y, dbh, search = "nearest", n) {
  df <- join_objtree_compt(id, x, y, dbh, search, n)

  z <- df %>%
    mutate(z = (dbh_comp / dbh_id) * exp((16 * .dist) / (dbh_id / dbh_comp))) %>%
    group_by(id) %>%
    summarise(z = sum(z, na.rm = any(!is.na(z)))) %>%
    arrange(as.numeric(id)) %>%
    .$z

  return(z)
}
