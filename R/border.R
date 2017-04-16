#' Available trees for measure and compute index.
#'
#' This function works in retangular plots and identify wich trees are
#' available for a util plot. The \code{border} parameter set how many spatial units (i.e. meters)
#' will be used to creates buffer offsets.
#'
#' @param x, x numeric vector of coordinates.
#' @param border numeric, buffer distance of boundary plot.
#'
#' @examples
#' df <- data_frame(id = 1:25) %>%
#'   mutate(
#'     x = xcoord(x = id, xspacing =  2, ncol =  5, star = "left-bottom"),
#'     y = ycoord(x = id, yspacing =  2, ncol =  5, star = "left-bottom")
#'   ) %>%
#'   mutate(border = available_tree(x = x, y = y, border = 1))
#'
#' if (require(ggplot2)) {
#'  ggplot(df, aes(x, y, label = id, color = border)) +
#'    geom_point(size = 4)
#' }
#'
#' @export
available_tree <- function(x, y, border) {
  ifelse(
    '&'(
      '&'(
        x < max(x) - border,
        x > min(x) + border
      ),
      '&'(
        y < max(y) - border,
        y > min(y) + border
      )
    ),
    TRUE, FALSE
  )
}
